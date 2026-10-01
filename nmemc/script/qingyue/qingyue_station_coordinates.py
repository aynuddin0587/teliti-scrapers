# ============================================================================
# Qingyue / Huawei Cloud
# Fujian national-control surface-water station catalogue
#
# Endpoint:
#   https://nawaterstation.apistore.huaweicloud.com/api/surface_water/staname
#
# Purpose:
#   Test Huawei APIG authentication locally and retrieve Fujian station
#   metadata, including longitude and latitude.
#
# Authentication:
#   Huawei APIG AppKey + AppSecret using apig_sdk/signer.py
#
# IMPORTANT:
#   - AppKey and AppSecret are read from environment variables.
#   - The exact full URL that is signed is also the exact URL sent.
#   - Raw API response is preserved before processing.
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

PROVINCE = "福建省"

PROJECT_DIR = Path(
    r"D:\# R Project\penelitian\nmemc"
)

OUTPUT_DIR = (
    PROJECT_DIR
    / "data"
    / "surfacewater"
    / "qingyue"
)

SOURCE_DIR = OUTPUT_DIR / "source"
PROCESSED_DIR = OUTPUT_DIR / "processed"

APP_KEY = os.getenv("QINGYUE_APP_KEY")
APP_SECRET = os.getenv("QINGYUE_APP_SECRET")


# ============================================================================
# 2. CHECK CONFIGURATION
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
# 3. BUILD THE EXACT QUERY USED BY THE HUAWEI DEBUGGER
# ============================================================================

query = {
    "city": "",
    "province": PROVINCE,
    "valley": ""
}

query_string = urlencode(query)

url = BASE_URL + "?" + query_string


print("=" * 72)
print("Qingyue Fujian station catalogue test")
print("=" * 72)
print()

print("Original URL:")
print(url)
print()


# ============================================================================
# 4. PREPARE HUAWEI APIG SIGNING
# ============================================================================

sig = signer.Signer()

sig.Key = APP_KEY
sig.Secret = APP_SECRET

method = "GET"

# These reproduce the relevant headers seen in the successful Huawei debugger.
headers = {
    "User-Agent": "APIGatewayDebugClient/1.0",
    "X-Stage": "RELEASE"
}

# This GET endpoint has no request body.
body = ""


# ============================================================================
# 5. CREATE AND SIGN REQUEST
# ============================================================================

r = signer.HttpRequest(
    method,
    url,
    headers,
    body
)


# Safe diagnostics.
# These do NOT expose AppSecret.

print("Parsed URI:")
print(getattr(r, "uri", None))

print()
print("Parsed query:")
print(getattr(r, "query", None))
print()


sig.Sign(r)


print("Signed header names:")
print(sorted(r.headers.keys()))


# Print only SignedHeaders, not the complete Authorization header.
authorization = r.headers.get("Authorization", "")

if "SignedHeaders=" in authorization:
    signed_headers = (
        authorization
        .split("SignedHeaders=", 1)[1]
        .split(",", 1)[0]
    )

    print(
        "SignedHeaders:",
        signed_headers
    )

print()


# ============================================================================
# 6. SEND THE EXACT FULL URL THAT WAS SIGNED
# ============================================================================
#
# DO NOT reconstruct the outgoing URL from:
#
#     r.scheme + "://" + r.host + r.uri
#
# because this version of Huawei signer.py stores the query separately,
# while r.uri contains only the path.
#
# We therefore send exactly `url`, the full URL supplied to HttpRequest().
# ============================================================================

print("Sending request...")
print("Province:", PROVINCE)
print()


response = requests.request(
    method,
    url,
    headers=r.headers,
    data=body,
    timeout=60,
    allow_redirects=False
)


print(
    "HTTP:",
    response.status_code,
    response.reason
)

print()


# ============================================================================
# 7. HANDLE HTTP ERROR
# ============================================================================

if response.status_code != 200:

    print("Response body:")
    print(response.text[:3000])

    raise RuntimeError(
        "Qingyue request failed with "
        f"HTTP {response.status_code}"
    )


# ============================================================================
# 8. SAVE THE UNALTERED RESPONSE FIRST
# ============================================================================

stamp = datetime.now().strftime(
    "%Y%m%d_%H%M%S"
)

raw_path = SOURCE_DIR / (
    f"qingyue_station_catalog_fujian_{stamp}.json"
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

    payload = response.json()

except Exception as exc:

    raise RuntimeError(
        "HTTP request succeeded, but response "
        f"could not be parsed as JSON: {exc}"
    )


err_code = payload.get("errCode")
err_msg = payload.get("errMsg")
request_id = payload.get("requestId")


print("API result:")
print("  errCode  :", err_code)
print("  errMsg   :", err_msg)
print("  requestId:", request_id)
print()


if err_code != 0:

    raise RuntimeError(
        "Qingyue application error: "
        f"errCode={err_code}; "
        f"errMsg={err_msg}"
    )


rows = payload.get("data")


if not isinstance(rows, list):

    raise RuntimeError(
        "Unexpected API response: "
        "'data' is not a list."
    )


print("API SUCCESS")
print("Records returned:", len(rows))
print()


# ============================================================================
# 10. VALIDATE PROVINCE FILTER
# ============================================================================

returned_provinces = sorted({
    str(x.get("province"))
    for x in rows
    if x.get("province") not in (None, "")
})


print("Province values returned:")
print(returned_provinces)
print()


unexpected_provinces = [
    x
    for x in returned_provinces
    if x != PROVINCE
]


if unexpected_provinces:

    raise RuntimeError(
        "Province-filter validation failed. "
        "Unexpected province(s): "
        + ", ".join(unexpected_provinces)
    )


# ============================================================================
# 11. SAVE CLEAN COPY OF API DATA ARRAY
# ============================================================================

catalog_json_path = PROCESSED_DIR / (
    "qingyue_fujian_station_catalog.json"
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
# 12. NORMALIZE STATION METADATA
# ============================================================================

output_rows = []


for x in rows:

    latitude_raw = x.get("latitude")
    longitude_raw = x.get("longitude")


    # ------------------------------------------------------------------------
    # Latitude
    # ------------------------------------------------------------------------

    try:

        latitude = (
            float(latitude_raw)
            if latitude_raw not in (
                None,
                ""
            )
            else None
        )

    except (
        TypeError,
        ValueError
    ):

        latitude = None


    # ------------------------------------------------------------------------
    # Longitude
    # ------------------------------------------------------------------------

    try:

        longitude = (
            float(longitude_raw)
            if longitude_raw not in (
                None,
                ""
            )
            else None
        )

    except (
        TypeError,
        ValueError
    ):

        longitude = None


    output_rows.append({

        "station_name":
            x.get("staname"),

        "province":
            x.get("province"),

        "city":
            x.get("city"),

        "valley":
            x.get("valley"),

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

        "qingyue_request_id":
            request_id,

        "source_endpoint":
            "/api/surface_water/staname"
    })


# ============================================================================
# 13. SAVE CSV
# ============================================================================

csv_path = PROCESSED_DIR / (
    "qingyue_fujian_station_coordinates.csv"
)


fieldnames = [
    "station_name",
    "province",
    "city",
    "valley",
    "longitude_raw",
    "latitude_raw",
    "longitude",
    "latitude",
    "first_time",
    "last_time",
    "coordinate_complete",
    "qingyue_request_id",
    "source_endpoint"
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
        output_rows
    )


# ============================================================================
# 14. QA
# ============================================================================

n_rows = len(
    output_rows
)

complete_coordinates = sum(
    1
    for x in output_rows
    if x["coordinate_complete"]
)

missing_coordinates = (
    n_rows
    - complete_coordinates
)


station_names = [
    x["station_name"]
    for x in output_rows
    if x["station_name"] not in (
        None,
        ""
    )
]


unique_station_names = len(
    set(station_names)
)


duplicate_station_name_rows = (
    len(station_names)
    - unique_station_names
)


# Check whether exact full source records are duplicated.

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
    for x in output_rows
]


exact_duplicate_rows = (
    len(full_keys)
    - len(set(full_keys))
)


# ============================================================================
# 15. FINAL SUMMARY
# ============================================================================

print("=" * 72)
print("STATION CATALOGUE SUMMARY")
print("=" * 72)

print(
    "Rows:",
    n_rows
)

print(
    "Unique station names:",
    unique_station_names
)

print(
    "Repeated station-name rows:",
    duplicate_station_name_rows
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
    "Exact duplicate full records:",
    exact_duplicate_rows
)

print()

print("Processed JSON:")
print(catalog_json_path)

print()

print("Processed CSV:")
print(csv_path)

print()

print("FINISHED")