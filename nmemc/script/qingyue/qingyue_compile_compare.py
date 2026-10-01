# ============================================================================
# Qingyue national station catalogue
# Compile 31 province responses and compare with unfiltered national response
#
# IMPORTANT:
#   This script makes ZERO API requests.
#   It works entirely from files already downloaded locally.
#
# Expected source files:
#
#   data/surfacewater/qingyue/source/province/
#       01_北京市.json
#       02_天津市.json
#       ...
#       31_新疆维吾尔自治区.json
#
#   data/surfacewater/qingyue/source/
#       qingyue_station_catalog_national_YYYYMMDD_HHMMSS.json
#
# Outputs:
#
#   data/surfacewater/qingyue/processed/
#
#       qingyue_station_catalog_province_union_raw.json
#       qingyue_station_catalog_province_union.csv
#
#       qingyue_comparison_exact_matches.csv
#       qingyue_comparison_only_province_union.csv
#       qingyue_comparison_only_national.csv
#       qingyue_comparison_province_counts.csv
#
#       qingyue_comparison_summary.json
#
# Comparison identity:
#   province
#   city
#   valley
#   station_name
#   longitude
#   latitude
#   first_time
#   last_time
#
# Coordinates are compared as SOURCE STRINGS, not floats.
# ============================================================================


import csv
import json
from collections import Counter
from datetime import datetime
from pathlib import Path


# ============================================================================
# 1. CONFIGURATION
# ============================================================================

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

PROVINCE_SOURCE_DIR = (
    SOURCE_DIR
    / "province"
)

PROCESSED_DIR = (
    QINGYUE_DIR
    / "processed"
)


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


# Source fields used for exact comparison.
COMPARE_FIELDS = [
    "province",
    "city",
    "valley",
    "station_name",
    "longitude",
    "latitude",
    "first_time",
    "last_time",
]


PROCESSED_DIR.mkdir(
    parents=True,
    exist_ok=True
)


# ============================================================================
# 2. SMALL HELPERS
# ============================================================================

def clean_source_value(value):
    """
    Preserve source meaning while making values hashable/comparable.

    No numeric conversion is performed.
    """

    if value is None:
        return ""

    return str(value).strip()


def source_record_to_standard(row):
    """
    Convert the Qingyue source field names to our standard comparison names.
    """

    return {
        "province":
            clean_source_value(
                row.get("province")
            ),

        "city":
            clean_source_value(
                row.get("city")
            ),

        "valley":
            clean_source_value(
                row.get("valley")
            ),

        "station_name":
            clean_source_value(
                row.get("staname")
            ),

        "longitude":
            clean_source_value(
                row.get("longitude")
            ),

        "latitude":
            clean_source_value(
                row.get("latitude")
            ),

        "first_time":
            clean_source_value(
                row.get("first_time")
            ),

        "last_time":
            clean_source_value(
                row.get("last_time")
            ),
    }


def record_key(row):
    """
    Exact eight-field identity.
    """

    return tuple(
        clean_source_value(
            row.get(field)
        )
        for field in COMPARE_FIELDS
    )


def read_api_json(path):
    """
    Read one raw Qingyue API response and validate its structure.
    """

    try:

        with path.open(
            "r",
            encoding="utf-8"
        ) as f:

            payload = json.load(f)

    except Exception as exc:

        raise RuntimeError(
            f"Could not parse JSON file:\n"
            f"{path}\n"
            f"{exc}"
        )


    if payload.get("errCode") != 0:

        raise RuntimeError(
            f"API file contains error response:\n"
            f"{path}\n"
            f"errCode={payload.get('errCode')}; "
            f"errMsg={payload.get('errMsg')}"
        )


    rows = payload.get("data")

    if not isinstance(
        rows,
        list
    ):

        raise RuntimeError(
            f"File does not contain data as a list:\n"
            f"{path}"
        )


    return payload, rows


def write_csv_rows(path, rows, fieldnames):

    with path.open(
        "w",
        newline="",
        encoding="utf-8-sig"
    ) as f:

        writer = csv.DictWriter(
            f,
            fieldnames=fieldnames,
            extrasaction="ignore"
        )

        writer.writeheader()

        writer.writerows(
            rows
        )


def safe_float(value):

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
# 3. LOCATE AND VALIDATE ALL 31 PROVINCE FILES
# ============================================================================

print("=" * 80)
print("Qingyue national catalogue compilation and comparison")
print("=" * 80)
print()

print("Checking 31 province source files...")
print()


province_source_files = []
missing_province_files = []


for index, province in enumerate(
    PROVINCES,
    start=1
):

    path = (
        PROVINCE_SOURCE_DIR
        / f"{index:02d}_{province}.json"
    )

    if path.exists():

        province_source_files.append(
            (
                index,
                province,
                path
            )
        )

    else:

        missing_province_files.append(
            str(path)
        )


if missing_province_files:

    print(
        "Missing province files:"
    )

    for path in missing_province_files:
        print(
            "  ",
            path
        )

    raise RuntimeError(
        f"{len(missing_province_files)} "
        "province source file(s) are missing."
    )


print(
    "Province files found:",
    len(province_source_files)
)

print()


# ============================================================================
# 4. COMPILE PROVINCE RESPONSES
# ============================================================================

province_union = []

province_manifest = []


for index, expected_province, path in province_source_files:

    payload, source_rows = read_api_json(
        path
    )


    standardized_rows = [
        source_record_to_standard(
            row
        )
        for row in source_rows
    ]


    # ------------------------------------------------------------------------
    # Validate province filter
    # ------------------------------------------------------------------------

    returned_provinces = sorted({
        row["province"]
        for row in standardized_rows
        if row["province"]
    })


    unexpected = [
        province
        for province in returned_provinces
        if province != expected_province
    ]


    if unexpected:

        raise RuntimeError(
            f"{expected_province}: source file contains "
            f"unexpected province(s): {unexpected}\n"
            f"File: {path}"
        )


    # ------------------------------------------------------------------------
    # Add provenance to analytical copy
    # ------------------------------------------------------------------------

    for row in standardized_rows:

        x = dict(
            row
        )

        x[
            "source_province_request"
        ] = expected_province

        x[
            "source_request_id"
        ] = clean_source_value(
            payload.get(
                "requestId"
            )
        )

        x[
            "source_file"
        ] = path.name

        province_union.append(
            x
        )


    province_manifest.append({

        "province":
            expected_province,

        "rows":
            len(
                standardized_rows
            ),

        "request_id":
            clean_source_value(
                payload.get(
                    "requestId"
                )
            ),

        "source_file":
            path.name
    })


    print(
        f"[{index:02d}/31] "
        f"{expected_province}: "
        f"{len(standardized_rows)} rows"
    )


print()

print(
    "Province-union rows:",
    len(province_union)
)

print()


# ============================================================================
# 5. SAVE COMPILED PROVINCE UNION
# ============================================================================

province_union_json_path = (
    PROCESSED_DIR
    / "qingyue_station_catalog_province_union_raw.json"
)


with province_union_json_path.open(
    "w",
    encoding="utf-8"
) as f:

    json.dump(
        province_union,
        f,
        ensure_ascii=False,
        indent=2
    )


province_union_csv_path = (
    PROCESSED_DIR
    / "qingyue_station_catalog_province_union.csv"
)


province_union_fields = (
    COMPARE_FIELDS
    + [
        "source_province_request",
        "source_request_id",
        "source_file",
    ]
)


write_csv_rows(
    province_union_csv_path,
    province_union,
    province_union_fields
)


# ============================================================================
# 6. FIND THE NEWEST NATIONAL UNFILTERED SOURCE FILE
# ============================================================================

national_candidates = sorted(
    SOURCE_DIR.glob(
        "qingyue_station_catalog_national_*.json"
    ),
    key=lambda p: p.stat().st_mtime
)


if not national_candidates:

    raise RuntimeError(
        "No locally saved national unfiltered "
        "Qingyue source JSON was found in:\n"
        f"{SOURCE_DIR}"
    )


national_source_path = (
    national_candidates[-1]
)


print(
    "National source selected:"
)

print(
    national_source_path
)

print()


# ============================================================================
# 7. LOAD NATIONAL UNFILTERED RESPONSE
# ============================================================================

national_payload, national_source_rows = read_api_json(
    national_source_path
)


national_rows = [
    source_record_to_standard(
        row
    )
    for row in national_source_rows
]


print(
    "National unfiltered rows:",
    len(national_rows)
)

print()


# ============================================================================
# 8. BASIC COORDINATE QA
# ============================================================================

def coordinate_summary(rows):

    complete = 0
    missing = 0
    invalid = 0


    for row in rows:

        lon = safe_float(
            row.get(
                "longitude"
            )
        )

        lat = safe_float(
            row.get(
                "latitude"
            )
        )


        if (
            lon is None
            or lat is None
        ):

            missing += 1
            continue


        complete += 1


        if not (
            -180 <= lon <= 180
            and -90 <= lat <= 90
        ):

            invalid += 1


    return {
        "complete":
            complete,

        "missing":
            missing,

        "invalid":
            invalid
    }


province_coordinate_qa = coordinate_summary(
    province_union
)

national_coordinate_qa = coordinate_summary(
    national_rows
)


# ============================================================================
# 9. BUILD EXACT RECORD COUNTERS
# ============================================================================
#
# Counter rather than set allows us to detect duplicate multiplicity.
# ============================================================================

province_counter = Counter(
    record_key(
        row
    )
    for row in province_union
)


national_counter = Counter(
    record_key(
        row
    )
    for row in national_rows
)


province_unique_keys = set(
    province_counter.keys()
)

national_unique_keys = set(
    national_counter.keys()
)


# ============================================================================
# 10. EXACT SET COMPARISON
# ============================================================================

matched_keys = (
    province_unique_keys
    & national_unique_keys
)


only_province_keys = (
    province_unique_keys
    - national_unique_keys
)


only_national_keys = (
    national_unique_keys
    - province_unique_keys
)


# ============================================================================
# 11. CONVERT COMPARISON KEYS BACK TO ROWS
# ============================================================================

def key_to_row(key):

    return dict(
        zip(
            COMPARE_FIELDS,
            key
        )
    )


matched_rows = [
    key_to_row(
        key
    )
    for key in sorted(
        matched_keys
    )
]


only_province_rows = [
    key_to_row(
        key
    )
    for key in sorted(
        only_province_keys
    )
]


only_national_rows = [
    key_to_row(
        key
    )
    for key in sorted(
        only_national_keys
    )
]


# ============================================================================
# 12. DUPLICATE QA
# ============================================================================

province_duplicate_keys = {
    key: count
    for key, count in province_counter.items()
    if count > 1
}


national_duplicate_keys = {
    key: count
    for key, count in national_counter.items()
    if count > 1
}


province_duplicate_extra_rows = sum(
    count - 1
    for count in province_duplicate_keys.values()
)


national_duplicate_extra_rows = sum(
    count - 1
    for count in national_duplicate_keys.values()
)


# ============================================================================
# 13. MULTIPLICITY COMPARISON
# ============================================================================
#
# Even if the unique record sets are equal, one source could repeat an
# otherwise identical record more times than the other.
# ============================================================================

all_keys = (
    province_unique_keys
    | national_unique_keys
)


multiplicity_differences = []


for key in sorted(
    all_keys
):

    province_n = province_counter.get(
        key,
        0
    )

    national_n = national_counter.get(
        key,
        0
    )


    if province_n != national_n:

        row = key_to_row(
            key
        )

        row[
            "province_union_occurrences"
        ] = province_n

        row[
            "national_occurrences"
        ] = national_n

        multiplicity_differences.append(
            row
        )


# ============================================================================
# 14. PROVINCE COUNT COMPARISON
# ============================================================================

province_union_counts = Counter(
    row["province"]
    for row in province_union
)


national_counts = Counter(
    row["province"]
    for row in national_rows
)


all_province_labels = sorted(
    set(
        province_union_counts.keys()
    )
    | set(
        national_counts.keys()
    )
)


province_count_comparison = []


for province in all_province_labels:

    union_n = province_union_counts.get(
        province,
        0
    )

    national_n = national_counts.get(
        province,
        0
    )


    province_count_comparison.append({

        "province":
            province,

        "province_union_rows":
            union_n,

        "national_unfiltered_rows":
            national_n,

        "difference_national_minus_union":
            national_n - union_n,

        "counts_equal":
            union_n == national_n
    })


# ============================================================================
# 15. SAVE COMPARISON OUTPUTS
# ============================================================================

matched_path = (
    PROCESSED_DIR
    / "qingyue_comparison_exact_matches.csv"
)


only_province_path = (
    PROCESSED_DIR
    / "qingyue_comparison_only_province_union.csv"
)


only_national_path = (
    PROCESSED_DIR
    / "qingyue_comparison_only_national.csv"
)


multiplicity_path = (
    PROCESSED_DIR
    / "qingyue_comparison_multiplicity_differences.csv"
)


province_counts_path = (
    PROCESSED_DIR
    / "qingyue_comparison_province_counts.csv"
)


province_manifest_path = (
    PROCESSED_DIR
    / "qingyue_station_catalog_province_compilation_manifest.csv"
)


write_csv_rows(
    matched_path,
    matched_rows,
    COMPARE_FIELDS
)


write_csv_rows(
    only_province_path,
    only_province_rows,
    COMPARE_FIELDS
)


write_csv_rows(
    only_national_path,
    only_national_rows,
    COMPARE_FIELDS
)


write_csv_rows(
    multiplicity_path,
    multiplicity_differences,
    (
        COMPARE_FIELDS
        + [
            "province_union_occurrences",
            "national_occurrences",
        ]
    )
)


write_csv_rows(
    province_counts_path,
    province_count_comparison,
    [
        "province",
        "province_union_rows",
        "national_unfiltered_rows",
        "difference_national_minus_union",
        "counts_equal",
    ]
)


write_csv_rows(
    province_manifest_path,
    province_manifest,
    [
        "province",
        "rows",
        "request_id",
        "source_file",
    ]
)


# ============================================================================
# 16. HIGHER-LEVEL NAME QA
# ============================================================================

province_station_names = {
    (
        row["province"],
        row["station_name"]
    )
    for row in province_union
}


national_station_names = {
    (
        row["province"],
        row["station_name"]
    )
    for row in national_rows
}


station_names_only_province = (
    province_station_names
    - national_station_names
)


station_names_only_national = (
    national_station_names
    - province_station_names
)


# ============================================================================
# 17. DECIDE WHETHER THE TWO CATALOGUES ARE EXACTLY EQUIVALENT
# ============================================================================

unique_record_sets_equal = (
    len(
        only_province_keys
    ) == 0
    and len(
        only_national_keys
    ) == 0
)


row_multiplicity_equal = (
    len(
        multiplicity_differences
    ) == 0
)


fully_equivalent = (
    unique_record_sets_equal
    and row_multiplicity_equal
)


# ============================================================================
# 18. SUMMARY JSON
# ============================================================================

summary_path = (
    PROCESSED_DIR
    / "qingyue_comparison_summary.json"
)


summary = {

    "created_at":
        datetime.now().isoformat(
            timespec="seconds"
        ),

    "comparison_fields":
        COMPARE_FIELDS,

    "national_source_file":
        national_source_path.name,

    "province_files_expected":
        31,

    "province_files_found":
        len(
            province_source_files
        ),

    "province_union": {

        "rows":
            len(
                province_union
            ),

        "unique_full_records":
            len(
                province_unique_keys
            ),

        "duplicate_full_record_keys":
            len(
                province_duplicate_keys
            ),

        "duplicate_extra_rows":
            province_duplicate_extra_rows,

        "complete_coordinates":
            province_coordinate_qa[
                "complete"
            ],

        "missing_coordinates":
            province_coordinate_qa[
                "missing"
            ],

        "invalid_coordinates":
            province_coordinate_qa[
                "invalid"
            ],
    },

    "national_unfiltered": {

        "rows":
            len(
                national_rows
            ),

        "unique_full_records":
            len(
                national_unique_keys
            ),

        "duplicate_full_record_keys":
            len(
                national_duplicate_keys
            ),

        "duplicate_extra_rows":
            national_duplicate_extra_rows,

        "complete_coordinates":
            national_coordinate_qa[
                "complete"
            ],

        "missing_coordinates":
            national_coordinate_qa[
                "missing"
            ],

        "invalid_coordinates":
            national_coordinate_qa[
                "invalid"
            ],
    },

    "comparison": {

        "exact_matched_unique_records":
            len(
                matched_keys
            ),

        "unique_records_only_in_province_union":
            len(
                only_province_keys
            ),

        "unique_records_only_in_national":
            len(
                only_national_keys
            ),

        "records_with_different_multiplicity":
            len(
                multiplicity_differences
            ),

        "province_station_names_only_in_union":
            len(
                station_names_only_province
            ),

        "province_station_names_only_in_national":
            len(
                station_names_only_national
            ),

        "unique_record_sets_equal":
            unique_record_sets_equal,

        "row_multiplicity_equal":
            row_multiplicity_equal,

        "fully_equivalent":
            fully_equivalent,
    }
}


with summary_path.open(
    "w",
    encoding="utf-8"
) as f:

    json.dump(
        summary,
        f,
        ensure_ascii=False,
        indent=2
    )


# ============================================================================
# 19. FINAL CONSOLE REPORT
# ============================================================================

print("=" * 80)
print("COMPARISON RESULTS")
print("=" * 80)

print()

print("31-PROVINCE UNION")
print("-" * 40)

print(
    "Rows:",
    len(
        province_union
    )
)

print(
    "Unique full records:",
    len(
        province_unique_keys
    )
)

print(
    "Duplicate extra rows:",
    province_duplicate_extra_rows
)

print(
    "Complete coordinates:",
    province_coordinate_qa[
        "complete"
    ]
)

print(
    "Missing coordinates:",
    province_coordinate_qa[
        "missing"
    ]
)

print()


print("UNFILTERED NATIONAL")
print("-" * 40)

print(
    "Rows:",
    len(
        national_rows
    )
)

print(
    "Unique full records:",
    len(
        national_unique_keys
    )
)

print(
    "Duplicate extra rows:",
    national_duplicate_extra_rows
)

print(
    "Complete coordinates:",
    national_coordinate_qa[
        "complete"
    ]
)

print(
    "Missing coordinates:",
    national_coordinate_qa[
        "missing"
    ]
)

print()


print("EXACT COMPARISON")
print("-" * 40)

print(
    "Matched unique records:",
    len(
        matched_keys
    )
)

print(
    "Only in province union:",
    len(
        only_province_keys
    )
)

print(
    "Only in national:",
    len(
        only_national_keys
    )
)

print(
    "Multiplicity differences:",
    len(
        multiplicity_differences
    )
)

print()


print("Province + station-name differences")
print("-" * 40)

print(
    "Only in province union:",
    len(
        station_names_only_province
    )
)

print(
    "Only in national:",
    len(
        station_names_only_national
    )
)

print()


print("OVERALL")
print("-" * 40)

print(
    "Unique record sets equal:",
    unique_record_sets_equal
)

print(
    "Row multiplicity equal:",
    row_multiplicity_equal
)

print(
    "FULLY EQUIVALENT:",
    fully_equivalent
)

print()


print("Province count comparison")
print("-" * 80)

for row in province_count_comparison:

    marker = (
        "OK"
        if row[
            "counts_equal"
        ]
        else "DIFF"
    )

    print(
        f"{marker:4s} "
        f"{row['province']}: "
        f"province={row['province_union_rows']} | "
        f"national={row['national_unfiltered_rows']} | "
        f"delta={row['difference_national_minus_union']:+d}"
    )


print()
print("=" * 80)
print("OUTPUT FILES")
print("=" * 80)

print()
print("Compiled province union:")
print(
    province_union_csv_path
)

print()
print("Only in province union:")
print(
    only_province_path
)

print()
print("Only in national:")
print(
    only_national_path
)

print()
print("Province counts:")
print(
    province_counts_path
)

print()
print("Multiplicity differences:")
print(
    multiplicity_path
)

print()
print("Summary:")
print(
    summary_path
)

print()
print("FINISHED")