# ============================================================================
# Qingyue / Huawei Cloud
# National-control surface-water station catalogue
#
# Collect the station catalogue separately for 31 provincial-level regions.
#
# Endpoint:
#   https://nawaterstation.apistore.huaweicloud.com/api/surface_water/staname
#
# Design:
#   - Huawei APIG AppKey/AppSecret signing
#   - exact request style already verified successfully for Fujian
#   - one request per province
#   - raw JSON preserved exactly
#   - resumable: successful province files are never requested again
#   - stops on HTTP/API failure instead of automatically retrying and
#     unexpectedly consuming additional paid requests
#   - builds combined national JSON + CSV
#   - creates QA and province manifest
# ============================================================================


from apig_sdk import signer

import csv
import json
import os
import time

from datetime import datetime
from pathlib import Path
from urllib.parse import urlencode

import requests


# ============================================================================
# 1. CONFIGURATION
# ============================================================================

BASE_URL = (
    "https://nawaterstation.apistore.huaweicloud.com"
    "/api/surface_water/staname"
)

PROJECT_DIR = Path(
    r"D:\# R Project\penelitian\nmemc"
)

QINGYUE_DIR = (
    PROJECT_DIR
    / "data"
    / "surfacewater"
    / "qingyue"
)

SOURCE_DIR = (
    QINGYUE_DIR
    / "source"
    / "province"
)

PROCESSED_DIR = (
    QINGYUE_DIR
    / "processed"
)

APP_KEY = os.getenv("QINGYUE_APP_KEY")
APP_SECRET = os.getenv("QINGYUE_APP_SECRET")

# Successful files are reused when the script is rerun.
REUSE_EXISTING = True

# Small polite delay between actual API requests.
PAUSE_SECONDS = 0.7


PROVINCES = [
    "北京市",
    "天津市",
    "河北省",
    "山西省",
    "内蒙古自治区",
    "辽宁省",
    "吉林省",
    "黑龙江省",
    "上海市",
    "江苏省",
    "浙江省",
    "安徽省",
    "福建省",
    "江西省",
    "山东省",
    "河南省",
    "湖北省",
    "湖南省",
    "广东省",
    "广西壮族自治区",
    "海南省",
    "重庆市",
    "四川省",
    "贵州省",
    "云南省",
    "西藏自治区",
    "陕西省",
    "甘肃省",
    "青海省",
    "宁夏回族自治区",
    "新疆维吾尔自治区",
]


# ============================================================================
# 2. CONFIGURATION CHECKS
# ============================================================================

if not APP_KEY:
    raise RuntimeError(
        "Environment variable QINGYUE_APP_KEY is not set."
    )

if not APP_SECRET:
    raise RuntimeError(
        "Environment variable QINGYUE_APP_SECRET is not set."
    )


SOURCE_DIR.mkdir(
    parents=True,
    exist_ok=True
)

PROCESSED_DIR.mkdir(
    parents=True,
    exist_ok=True
)


# ============================================================================
# 3. HUAWEI SIGNER
# ============================================================================

sig = signer.Signer()
sig.Key = APP_KEY
sig.Secret = APP_SECRET


# ============================================================================
# 4. HELPERS
# ============================================================================

def source_path_for(index, province):
    """
    Stable file name so successful calls can be reused on later runs.
    """

    return SOURCE_DIR / (
        f"{index:02d}_{province}.json"
    )


def parse_payload(raw_bytes, expected_province=None):
    """
    Parse and validate one Qingyue response.
    """

    try:
        payload = json.loads(
            raw_bytes.decode("utf-8")
        )
    except Exception as exc:
        raise RuntimeError(
            f"Could not parse response JSON: {exc}"
        )

    err_code = payload.get("errCode")
    err_msg = payload.get("errMsg")

    if err_code != 0:
        raise RuntimeError(
            f"API error: errCode={err_code}; "
            f"errMsg={err_msg}"
        )

    rows = payload.get("data")

    if not isinstance(rows, list):
        raise RuntimeError(
            "Unexpected API response: 'data' is not a list."
        )

    # Check that a province-filtered response does not contain
    # records labelled as a different province.
    if expected_province is not None:

        returned_provinces = sorted({
            str(x.get("province"))
            for x in rows
            if x.get("province") not in (None, "")
        })

        unexpected = [
            p
            for p in returned_provinces
            if p != expected_province
        ]

        if unexpected:
            raise RuntimeError(
                f"Province validation failed for "
                f"{expected_province}; response also contains: "
                f"{unexpected}"
            )

    return payload, rows


def load_existing(index, province):
    """
    Load a previously successful province response.
    """

    path = source_path_for(
        index,
        province
    )

    if not path.exists():
        return None

    try:

        raw = path.read_bytes()

        payload, rows = parse_payload(
            raw,
            expected_province=province
        )

        return {
            "payload": payload,
            "rows": rows,
            "path": path
        }

    except Exception as exc:

        print(
            f"Existing file rejected for {province}: "
            f"{exc}"
        )

        return None


def fetch_province(index, province):
    """
    Retrieve one province using exactly the request method
    already verified successfully for Fujian.
    """

    # ------------------------------------------------------------------------
    # Resume from existing successful raw response
    # ------------------------------------------------------------------------

    if REUSE_EXISTING:

        existing = load_existing(
            index,
            province
        )

        if existing is not None:

            print(
                f"[{index:02d}/31] "
                f"{province}: "
                f"reuse {len(existing['rows'])} rows"
            )

            return (
                existing["payload"],
                existing["rows"],
                "reused"
            )


    # ------------------------------------------------------------------------
    # Build exact query
    # ------------------------------------------------------------------------

    query = {
        "city": "",
        "province": province,
        "valley": ""
    }

    url = (
        BASE_URL
        + "?"
        + urlencode(query)
    )


    # ------------------------------------------------------------------------
    # Build and sign Huawei request
    # ------------------------------------------------------------------------

    headers = {
        "User-Agent":
            "APIGatewayDebugClient/1.0",

        "X-Stage":
            "RELEASE"
    }

    body = ""

    r = signer.HttpRequest(
        "GET",
        url,
        headers,
        body
    )

    sig.Sign(r)


    # ------------------------------------------------------------------------
    # Send EXACT full URL supplied to signer
    # ------------------------------------------------------------------------

    print(
        f"[{index:02d}/31] "
        f"{province}: requesting..."
    )

    response = requests.request(
        "GET",
        url,
        headers=r.headers,
        data=body,
        timeout=60,
        allow_redirects=False
    )


    # ------------------------------------------------------------------------
    # HTTP validation
    # ------------------------------------------------------------------------

    if response.status_code != 200:

        print()
        print(
            f"{province}: HTTP "
            f"{response.status_code} "
            f"{response.reason}"
        )

        print("Response body:")
        print(
            response.text[:3000]
        )

        raise RuntimeError(
            f"{province}: HTTP "
            f"{response.status_code}"
        )


    # ------------------------------------------------------------------------
    # API/application validation
    # ------------------------------------------------------------------------

    payload, rows = parse_payload(
        response.content,
        expected_province=province
    )


    # ------------------------------------------------------------------------
    # Preserve exact raw response
    # ------------------------------------------------------------------------

    path = source_path_for(
        index,
        province
    )

    path.write_bytes(
        response.content
    )


    request_id = payload.get(
        "requestId"
    )

    print(
        f"[{index:02d}/31] "
        f"{province}: "
        f"{len(rows)} rows | "
        f"requestId={request_id}"
    )


    return (
        payload,
        rows,
        "downloaded"
    )


def safe_float(value):
    """
    Convert coordinate strings to float without destroying
    the original source string.
    """

    if value in (
        None,
        ""
    ):
        return None

    try:
        return float(value)

    except (
        TypeError,
        ValueError
    ):
        return None


# ============================================================================
# 5. COLLECTION
# ============================================================================

print("=" * 78)
print("Qingyue national station catalogue")
print("31-province collection")
print("=" * 78)
print()


started_at = datetime.now()

all_rows = []
manifest = []


for index, province in enumerate(
    PROVINCES,
    start=1
):

    try:

        payload, rows, source_method = fetch_province(
            index,
            province
        )

        request_id = payload.get(
            "requestId"
        )


        # --------------------------------------------------------------------
        # Add rows to analytical national copy
        # --------------------------------------------------------------------

        for row in rows:

            x = dict(row)

            # Provenance added only to combined processed version.
            # Raw source files remain untouched.
            x["source_province_request"] = province
            x["source_request_id"] = request_id

            all_rows.append(x)


        # --------------------------------------------------------------------
        # Manifest
        # --------------------------------------------------------------------

        manifest.append({
            "province":
                province,

            "status":
                "success",

            "source_method":
                source_method,

            "n_rows":
                len(rows),

            "request_id":
                request_id,

            "error":
                ""
        })


        # --------------------------------------------------------------------
        # Do not sleep when only reading an existing local file
        # --------------------------------------------------------------------

        if source_method == "downloaded":
            time.sleep(
                PAUSE_SECONDS
            )


    except Exception as exc:

        manifest.append({
            "province":
                province,

            "status":
                "error",

            "source_method":
                "",

            "n_rows":
                0,

            "request_id":
                "",

            "error":
                str(exc)
        })


        print()
        print("=" * 78)
        print("COLLECTION STOPPED")
        print("=" * 78)

        print(
            "Successful province responses have "
            "already been saved."
        )

        print(
            "Fix the problem and rerun this script. "
            "Completed provinces will be reused without "
            "making another paid request."
        )

        raise


# ============================================================================
# 6. NORMALIZE NATIONAL DATASET
# ============================================================================

normalized = []


for x in all_rows:

    longitude_raw = x.get(
        "longitude"
    )

    latitude_raw = x.get(
        "latitude"
    )

    longitude = safe_float(
        longitude_raw
    )

    latitude = safe_float(
        latitude_raw
    )


    normalized.append({

        "province":
            x.get("province"),

        "city":
            x.get("city"),

        "valley":
            x.get("valley"),

        "station_name":
            x.get("staname"),

        "longitude_raw":
            longitude_raw,

        "latitude_raw":
            latitude_raw,

        "longitude":
            longitude,

        "latitude":
            latitude,

        "first_time":
            x.get("first_time"),

        "last_time":
            x.get("last_time"),

        "coordinate_complete":
            (
                longitude is not None
                and latitude is not None
            ),

        "coordinate_valid_earth":
            (
                longitude is not None
                and latitude is not None
                and -180 <= longitude <= 180
                and -90 <= latitude <= 90
            ),

        "source_province_request":
            x.get(
                "source_province_request"
            ),

        "source_request_id":
            x.get(
                "source_request_id"
            ),

        "source_endpoint":
            "/api/surface_water/staname"
    })


# ============================================================================
# 7. WRITE COMBINED JSON
# ============================================================================

combined_json_path = (
    PROCESSED_DIR
    / "qingyue_station_catalog_31_provinces.json"
)


with combined_json_path.open(
    "w",
    encoding="utf-8"
) as f:

    json.dump(
        all_rows,
        f,
        ensure_ascii=False,
        indent=2
    )


# ============================================================================
# 8. WRITE COMBINED CSV
# ============================================================================

combined_csv_path = (
    PROCESSED_DIR
    / "qingyue_station_catalog_31_provinces.csv"
)


csv_fields = [
    "province",
    "city",
    "valley",
    "station_name",
    "longitude_raw",
    "latitude_raw",
    "longitude",
    "latitude",
    "first_time",
    "last_time",
    "coordinate_complete",
    "coordinate_valid_earth",
    "source_province_request",
    "source_request_id",
    "source_endpoint"
]


with combined_csv_path.open(
    "w",
    newline="",
    encoding="utf-8-sig"
) as f:

    writer = csv.DictWriter(
        f,
        fieldnames=csv_fields
    )

    writer.writeheader()

    writer.writerows(
        normalized
    )


# ============================================================================
# 9. WRITE PROVINCE MANIFEST
# ============================================================================

manifest_path = (
    PROCESSED_DIR
    / "qingyue_station_catalog_province_manifest.csv"
)


with manifest_path.open(
    "w",
    newline="",
    encoding="utf-8-sig"
) as f:

    writer = csv.DictWriter(
        f,
        fieldnames=[
            "province",
            "status",
            "source_method",
            "n_rows",
            "request_id",
            "error"
        ]
    )

    writer.writeheader()

    writer.writerows(
        manifest
    )


# ============================================================================
# 10. QA
# ============================================================================

total_rows = len(
    normalized
)


complete_coordinates = sum(
    1
    for x in normalized
    if x["coordinate_complete"]
)


missing_coordinates = (
    total_rows
    - complete_coordinates
)


invalid_earth_coordinates = sum(
    1
    for x in normalized
    if (
        x["coordinate_complete"]
        and not x["coordinate_valid_earth"]
    )
)


# Full-record identity.
full_keys = [
    (
        x["province"],
        x["city"],
        x["valley"],
        x["station_name"],
        x["longitude_raw"],
        x["latitude_raw"],
        x["first_time"],
        x["last_time"]
    )
    for x in normalized
]


unique_full_records = len(
    set(full_keys)
)


exact_duplicate_rows = (
    total_rows
    - unique_full_records
)


# Province + station name identity.
province_station_keys = [
    (
        x["province"],
        x["station_name"]
    )
    for x in normalized
]


unique_province_station_names = len(
    set(province_station_keys)
)


repeated_province_station_rows = (
    total_rows
    - unique_province_station_names
)


# Coordinate identity.
coordinate_keys = [
    (
        x["longitude_raw"],
        x["latitude_raw"]
    )
    for x in normalized
    if x["coordinate_complete"]
]


unique_coordinate_pairs = len(
    set(coordinate_keys)
)


# Province counts.
province_counts = {}

for row in normalized:

    province = row["province"]

    province_counts[province] = (
        province_counts.get(
            province,
            0
        )
        + 1
    )


# ============================================================================
# 11. QA OUTPUT FILE
# ============================================================================

qa_path = (
    PROCESSED_DIR
    / "qingyue_station_catalog_qa.json"
)


qa = {

    "created_at":
        datetime.now().isoformat(
            timespec="seconds"
        ),

    "province_requests_expected":
        len(PROVINCES),

    "province_requests_successful":
        sum(
            1
            for x in manifest
            if x["status"] == "success"
        ),

    "total_rows":
        total_rows,

    "unique_full_records":
        unique_full_records,

    "exact_duplicate_rows":
        exact_duplicate_rows,

    "unique_province_station_names":
        unique_province_station_names,

    "repeated_province_station_rows":
        repeated_province_station_rows,

    "complete_coordinates":
        complete_coordinates,

    "missing_coordinates":
        missing_coordinates,

    "invalid_earth_coordinates":
        invalid_earth_coordinates,

    "unique_coordinate_pairs":
        unique_coordinate_pairs,

    "province_counts":
        province_counts
}


with qa_path.open(
    "w",
    encoding="utf-8"
) as f:

    json.dump(
        qa,
        f,
        ensure_ascii=False,
        indent=2
    )


# ============================================================================
# 12. FINAL SUMMARY
# ============================================================================

elapsed = (
    datetime.now()
    - started_at
)


print()
print("=" * 78)
print("NATIONAL COLLECTION FINISHED")
print("=" * 78)

print(
    "Province requests represented:",
    len(PROVINCES)
)

print(
    "Total catalogue rows:",
    total_rows
)

print(
    "Unique full records:",
    unique_full_records
)

print(
    "Exact duplicate full rows:",
    exact_duplicate_rows
)

print(
    "Unique province + station names:",
    unique_province_station_names
)

print(
    "Repeated province + station rows:",
    repeated_province_station_rows
)

print(
    "Complete coordinates:",
    complete_coordinates
)

print(
    "Missing coordinates:",
    missing_coordinates
)

print(
    "Invalid Earth coordinates:",
    invalid_earth_coordinates
)

print(
    "Unique coordinate pairs:",
    unique_coordinate_pairs
)

print(
    "Elapsed:",
    elapsed
)


print()
print("Province counts:")
print("-" * 40)

for province in PROVINCES:

    print(
        f"{province}: "
        f"{province_counts.get(province, 0)}"
    )


print()
print("Combined JSON:")
print(combined_json_path)

print()
print("Combined CSV:")
print(combined_csv_path)

print()
print("Manifest:")
print(manifest_path)

print()
print("QA:")
print(qa_path)

print()
print("FINISHED")