# ============================================================================
# Qingyue / Huawei Cloud
# National-control surface-water station catalogue
#
# Purpose:
#   Retrieve the UNFILTERED national station catalogue in one API request.
#
# Endpoint:
#   https://nawaterstation.apistore.huaweicloud.com/api/surface_water/staname
#
# Query:
#   city=
#   province=
#   valley=
#
# Authentication:
#   Huawei APIG AppKey + AppSecret using apig_sdk/signer.py
#
# Outputs:
#   source/
#       qingyue_station_catalog_national_TIMESTAMP.json
#
#   processed/
#       qingyue_national_station_catalog_unfiltered.json
#       qingyue_national_station_catalog_unfiltered.csv
#       qingyue_national_station_catalog_unfiltered_qa.json
#
# Notes:
#   - Exactly ONE API request is made.
#   - The exact raw response bytes are preserved before processing.
#   - No pagination is assumed because this endpoint documents none.
# ============================================================================


from apig_sdk import signer

import csv
import json
import os
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
)

PROCESSED_DIR = (
    QINGYUE_DIR
    / "processed"
)

APP_KEY = os.getenv("QINGYUE_APP_KEY")
APP_SECRET = os.getenv("QINGYUE_APP_SECRET")


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
# 3. BUILD UNFILTERED QUERY
# ============================================================================

query = {
    "city": "",
    "province": "",
    "valley": ""
}

url = (
    BASE_URL
    + "?"
    + urlencode(query)
)


print("=" * 78)
print("Qingyue national unfiltered station catalogue")
print("=" * 78)
print()

print("Request URL:")
print(url)
print()


# ============================================================================
# 4. PREPARE HUAWEI APIG SIGNING
# ============================================================================

sig = signer.Signer()

sig.Key = APP_KEY
sig.Secret = APP_SECRET


headers = {
    "User-Agent":
        "APIGatewayDebugClient/1.0",

    "X-Stage":
        "RELEASE"
}

body = ""


# ============================================================================
# 5. CREATE AND SIGN REQUEST
# ============================================================================

r = signer.HttpRequest(
    "GET",
    url,
    headers,
    body
)


print("Parsed URI:")
print(
    getattr(
        r,
        "uri",
        None
    )
)

print()

print("Parsed query:")
print(
    getattr(
        r,
        "query",
        None
    )
)

print()


sig.Sign(r)


print("Signed header names:")
print(
    sorted(
        r.headers.keys()
    )
)


# Print only the SignedHeaders field.
# Do not print the full Authorization value because it contains the AppKey.

authorization = r.headers.get(
    "Authorization",
    ""
)

if "SignedHeaders=" in authorization:

    signed_headers = (
        authorization
        .split(
            "SignedHeaders=",
            1
        )[1]
        .split(
            ",",
            1
        )[0]
    )

    print(
        "SignedHeaders:",
        signed_headers
    )


print()


# ============================================================================
# 6. SEND EXACTLY THE FULL URL THAT WAS SIGNED
# ============================================================================

print("Sending ONE unfiltered national request...")
print()


response = requests.request(
    "GET",
    url,
    headers=r.headers,
    data=body,
    timeout=120,
    allow_redirects=False
)


print(
    "HTTP:",
    response.status_code,
    response.reason
)

print(
    "Response bytes:",
    len(response.content)
)

print()


# ============================================================================
# 7. HANDLE HTTP ERROR
# ============================================================================

if response.status_code != 200:

    print("Response body:")
    print(
        response.text[:3000]
    )

    raise RuntimeError(
        "Qingyue national request failed with "
        f"HTTP {response.status_code}"
    )


# ============================================================================
# 8. SAVE EXACT RAW RESPONSE FIRST
# ============================================================================

stamp = datetime.now().strftime(
    "%Y%m%d_%H%M%S"
)


raw_path = SOURCE_DIR / (
    f"qingyue_station_catalog_national_{stamp}.json"
)


raw_path.write_bytes(
    response.content
)


print("Raw response saved:")
print(raw_path)
print()


# ============================================================================
# 9. PARSE JSON
# ============================================================================

try:

    payload = json.loads(
        response.content.decode(
            "utf-8"
        )
    )

except Exception as exc:

    raise RuntimeError(
        "HTTP request succeeded, but the response "
        f"could not be parsed as complete JSON: {exc}"
    )


err_code = payload.get(
    "errCode"
)

err_msg = payload.get(
    "errMsg"
)

request_id = payload.get(
    "requestId"
)


print("API result:")
print(
    "  errCode  :",
    err_code
)

print(
    "  errMsg   :",
    err_msg
)

print(
    "  requestId:",
    request_id
)

print()


if err_code != 0:

    raise RuntimeError(
        "Qingyue application error: "
        f"errCode={err_code}; "
        f"errMsg={err_msg}"
    )


rows = payload.get(
    "data"
)


if not isinstance(
    rows,
    list
):

    raise RuntimeError(
        "Unexpected API response: "
        "'data' is not a list."
    )


print("API SUCCESS")
print(
    "Records returned:",
    len(rows)
)

print()


# ============================================================================
# 10. BASIC NATIONAL COVERAGE CHECKS
# ============================================================================

returned_provinces = sorted({
    str(
        x.get(
            "province"
        )
    )
    for x in rows
    if x.get(
        "province"
    ) not in (
        None,
        ""
    )
})


returned_cities = sorted({
    str(
        x.get(
            "city"
        )
    )
    for x in rows
    if x.get(
        "city"
    ) not in (
        None,
        ""
    )
})


returned_valleys = sorted({
    str(
        x.get(
            "valley"
        )
    )
    for x in rows
    if x.get(
        "valley"
    ) not in (
        None,
        ""
    )
})


print(
    "Province-level values returned:",
    len(returned_provinces)
)

print(
    "Cities returned:",
    len(returned_cities)
)

print(
    "Valleys/basins returned:",
    len(returned_valleys)
)

print()


print("Province-level values:")
for province in returned_provinces:
    print(
        "  ",
        province
    )

print()


# ============================================================================
# 11. SAVE CLEAN API DATA ARRAY
# ============================================================================

catalog_json_path = (
    PROCESSED_DIR
    / "qingyue_national_station_catalog_unfiltered.json"
)


with catalog_json_path.open(
    "w",
    encoding="utf-8"
) as f:

    json.dump(
        rows,
        f,
        ensure_ascii=False,
        indent=2
    )


# ============================================================================
# 12. HELPERS
# ============================================================================

def safe_float(value):

    if value in (
        None,
        ""
    ):
        return None

    try:
        return float(
            value
        )

    except (
        TypeError,
        ValueError
    ):
        return None


# ============================================================================
# 13. NORMALIZE NATIONAL STATION CATALOGUE
# ============================================================================

normalized = []


for x in rows:

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
            x.get(
                "province"
            ),

        "city":
            x.get(
                "city"
            ),

        "valley":
            x.get(
                "valley"
            ),

        "station_name":
            x.get(
                "staname"
            ),

        "longitude_raw":
            longitude_raw,

        "latitude_raw":
            latitude_raw,

        "longitude":
            longitude,

        "latitude":
            latitude,

        "first_time":
            x.get(
                "first_time"
            ),

        "last_time":
            x.get(
                "last_time"
            ),

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

        "source_request_id":
            request_id,

        "source_endpoint":
            "/api/surface_water/staname",

        "source_query":
            "city=&province=&valley="
    })


# ============================================================================
# 14. SAVE NATIONAL CSV
# ============================================================================

csv_path = (
    PROCESSED_DIR
    / "qingyue_national_station_catalog_unfiltered.csv"
)


fieldnames = [
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
    "source_request_id",
    "source_endpoint",
    "source_query"
]


with csv_path.open(
    "w",
    newline="",
    encoding="utf-8-sig"
) as f:

    writer = csv.DictWriter(
        f,
        fieldnames=fieldnames
    )

    writer.writeheader()

    writer.writerows(
        normalized
    )


# ============================================================================
# 15. QA
# ============================================================================

total_rows = len(
    normalized
)


complete_coordinates = sum(
    1
    for x in normalized
    if x[
        "coordinate_complete"
    ]
)


missing_coordinates = (
    total_rows
    - complete_coordinates
)


invalid_earth_coordinates = sum(
    1
    for x in normalized
    if (
        x[
            "coordinate_complete"
        ]
        and not x[
            "coordinate_valid_earth"
        ]
    )
)


# ---------------------------------------------------------------------------
# Exact full-record identity
# ---------------------------------------------------------------------------

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
    set(
        full_keys
    )
)


exact_duplicate_rows = (
    total_rows
    - unique_full_records
)


# ---------------------------------------------------------------------------
# Province + station-name identity
# ---------------------------------------------------------------------------

province_station_keys = [
    (
        x["province"],
        x["station_name"]
    )
    for x in normalized
]


unique_province_station_names = len(
    set(
        province_station_keys
    )
)


repeated_province_station_rows = (
    total_rows
    - unique_province_station_names
)


# ---------------------------------------------------------------------------
# Station-name-only identity
# ---------------------------------------------------------------------------

station_names = [
    x["station_name"]
    for x in normalized
    if x[
        "station_name"
    ] not in (
        None,
        ""
    )
]


unique_station_names = len(
    set(
        station_names
    )
)


# ---------------------------------------------------------------------------
# Coordinate-pair identity
# ---------------------------------------------------------------------------

coordinate_keys = [
    (
        x["longitude_raw"],
        x["latitude_raw"]
    )
    for x in normalized
    if x[
        "coordinate_complete"
    ]
]


unique_coordinate_pairs = len(
    set(
        coordinate_keys
    )
)


# ---------------------------------------------------------------------------
# Province row counts
# ---------------------------------------------------------------------------

province_counts = {}


for x in normalized:

    province = x[
        "province"
    ]

    province_counts[
        province
    ] = (
        province_counts.get(
            province,
            0
        )
        + 1
    )


# ============================================================================
# 16. QA JSON
# ============================================================================

qa_path = (
    PROCESSED_DIR
    / "qingyue_national_station_catalog_unfiltered_qa.json"
)


qa = {

    "created_at":
        datetime.now().isoformat(
            timespec="seconds"
        ),

    "source_request_id":
        request_id,

    "response_bytes":
        len(
            response.content
        ),

    "total_rows":
        total_rows,

    "province_level_values":
        len(
            returned_provinces
        ),

    "city_values":
        len(
            returned_cities
        ),

    "valley_values":
        len(
            returned_valleys
        ),

    "unique_full_records":
        unique_full_records,

    "exact_duplicate_rows":
        exact_duplicate_rows,

    "unique_station_names":
        unique_station_names,

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
        province_counts,

    "province_values":
        returned_provinces,

    "valley_values_list":
        returned_valleys
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
# 17. FINAL SUMMARY
# ============================================================================

print("=" * 78)
print("NATIONAL UNFILTERED CATALOGUE SUMMARY")
print("=" * 78)

print(
    "Response bytes:",
    len(
        response.content
    )
)

print(
    "Rows:",
    total_rows
)

print(
    "Province-level values:",
    len(
        returned_provinces
    )
)

print(
    "Cities:",
    len(
        returned_cities
    )
)

print(
    "Valleys/basins:",
    len(
        returned_valleys
    )
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
    "Unique station names:",
    unique_station_names
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


print()
print("Province row counts:")
print("-" * 50)


for province in returned_provinces:

    print(
        f"{province}: "
        f"{province_counts.get(province, 0)}"
    )


print()
print("Raw source:")
print(raw_path)

print()
print("Processed JSON:")
print(catalog_json_path)

print()
print("Processed CSV:")
print(csv_path)

print()
print("QA:")
print(qa_path)

print()
print("FINISHED")