# ============================================================================
# 15_ingest_cnemc_station_coordinates.R
#
# Fast CNEMC station-coordinate ingestion and coverage audit.
#
# Purpose:
#   1. Accumulate legitimate external station-coordinate metadata from
#      coordinate_inbox/ into a reproducible CNEMC station crosswalk.
#   2. Flag materially conflicting coordinates instead of silently choosing.
#   3. Measure coordinate coverage against a DISTINCT CNEMC station catalogue.
#
# Performance change from the earlier version:
#   * Coverage no longer re-cleans the full cnemc_latest_analysis.rds.
#   * Preferred source is analysis/station_metadata.rds (~thousands of rows).
#   * A compact cnemc_station_catalogue.csv is cached for later runs.
#   * Full CNEMC observations are used only as a last-resort fallback, and even
#     then are reduced to distinct station labels BEFORE text decoding.
#
# This script DOES NOT scrape or automate a limited preview endpoint.
# ============================================================================

options(stringsAsFactors = FALSE)

# ----------------------------------------------------------------------------
# 1. Paths
# ----------------------------------------------------------------------------
PROJECT_DIR <- "D:/# R Project/penelitian"
RECON_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation")
HELPER_FILE <- file.path(RECON_DIR, "script", "cnemc_analysis_helpers.R")

STATION_METADATA_FILE <- file.path(RECON_DIR, "analysis", "station_metadata.rds")
ANALYSIS_FILE <- file.path(RECON_DIR, "analysis", "cnemc_latest_analysis.rds")

CNEMC_SURFACE_DIR <- file.path(PROJECT_DIR, "nmemc", "data", "surfacewater")
INBOX_DIR <- file.path(CNEMC_SURFACE_DIR, "coordinate_inbox")
PROCESSED_DIR <- file.path(CNEMC_SURFACE_DIR, "processed")

CROSSWALK_FILE <- file.path(PROCESSED_DIR, "cnemc_station_crosswalk.csv")
CONFLICT_FILE <- file.path(PROCESSED_DIR, "cnemc_station_coordinate_conflicts.csv")
COVERAGE_FILE <- file.path(PROCESSED_DIR, "cnemc_station_coordinate_coverage.csv")
CATALOGUE_FILE <- file.path(PROCESSED_DIR, "cnemc_station_catalogue.csv")

SOURCE_URL_DEFAULT <- "https://data.epmap.org/product/water?tab=download"
SOURCE_LABEL_DEFAULT <- "external_station_metadata"

# ----------------------------------------------------------------------------
# 2. Packages and helpers
# ----------------------------------------------------------------------------
required <- c("dplyr", "readr", "tibble")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0L) {
  stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

if (!file.exists(HELPER_FILE)) {
  stop("Missing helper file: ", HELPER_FILE, call. = FALSE)
}
source(HELPER_FILE, encoding = "UTF-8")

dir.create(INBOX_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", ...)
}

norm_names <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[[:space:]]+", "", x, perl = TRUE)
  tolower(x)
}

normalize_station_component <- function(x) {
  z <- trimws(as.character(x))
  z <- gsub(intToUtf8(12288L), "", z, fixed = TRUE)
  z <- gsub("[[:space:]]+", "", z, perl = TRUE)
  tolower(z)
}

make_station_key <- function(area, section) {
  paste(
    normalize_station_component(area),
    normalize_station_component(section),
    sep = "|"
  )
}

find_col <- function(df, candidates) {
  nm <- names(df)
  low <- norm_names(nm)
  cand <- norm_names(candidates)
  for (i in seq_along(cand)) {
    hit <- match(cand[[i]], low)
    if (!is.na(hit)) return(nm[[hit]])
  }
  NA_character_
}

safe_chr <- function(df, col, n = nrow(df)) {
  if (is.na(col) || !col %in% names(df)) return(rep(NA_character_, n))
  as.character(df[[col]])
}

safe_num <- function(df, col, n = nrow(df)) {
  if (is.na(col) || !col %in% names(df)) return(rep(NA_real_, n))
  suppressWarnings(as.numeric(df[[col]]))
}

valid_lonlat <- function(lon, lat) {
  is.finite(lon) & is.finite(lat) &
    lon >= -180 & lon <= 180 &
    lat >= -90 & lat <= 90
}

# ----------------------------------------------------------------------------
# 3. Coordinate input readers
# ----------------------------------------------------------------------------
read_coordinate_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("csv", "gz")) {
    return(suppressMessages(
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    ))
  }

  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Excel coordinate input found but package 'readxl' is not installed: ",
        basename(path),
        "\nInstall once with install.packages('readxl').",
        call. = FALSE
      )
    }
    return(readxl::read_excel(path))
  }

  NULL
}

standardize_coordinate_file <- function(df, path) {
  if (is.null(df) || nrow(df) == 0L) return(tibble())

  area_col <- find_col(df, c(
    "area", "province", "province_name", "所属省份", "省份", "省", "地区"
  ))
  city_col <- find_col(df, c(
    "city", "city_name", "prefecture", "所属地区", "地市", "城市", "市"
  ))
  basin_col <- find_col(df, c(
    "river_basin", "basin", "basin_name", "所属流域", "流域"
  ))
  section_col <- find_col(df, c(
    "monitoring_section", "section", "section_name", "station", "station_name",
    "断面名称", "监测断面", "监测断面名称", "站点名称"
  ))
  lon_col <- find_col(df, c(
    "longitude", "lon", "lng", "经度", "station_longitude"
  ))
  lat_col <- find_col(df, c(
    "latitude", "lat", "纬度", "station_latitude"
  ))

  required_cols <- c(area_col, section_col, lon_col, lat_col)
  if (any(is.na(required_cols))) {
    msg(
      "Skipping ", basename(path),
      " because required columns were not recognized. Columns: ",
      paste(names(df), collapse = ", ")
    )
    return(tibble())
  }

  area <- repair_cnemc_mojibake(df[[area_col]])
  section <- repair_cnemc_mojibake(df[[section_col]])
  basin <- if (!is.na(basin_col)) {
    repair_cnemc_mojibake(df[[basin_col]])
  } else {
    rep(NA_character_, nrow(df))
  }
  city <- if (!is.na(city_col)) {
    repair_cnemc_mojibake(df[[city_col]])
  } else {
    rep(NA_character_, nrow(df))
  }

  longitude <- suppressWarnings(readr::parse_number(as.character(df[[lon_col]])))
  latitude <- suppressWarnings(readr::parse_number(as.character(df[[lat_col]])))

  tibble(
    area_cn = trimws(area),
    city_cn = trimws(city),
    river_basin_cn = trimws(basin),
    monitoring_section_cn = trimws(section),
    longitude = longitude,
    latitude = latitude,
    station_key = make_station_key(area, section),
    coordinate_source = SOURCE_LABEL_DEFAULT,
    source_url = SOURCE_URL_DEFAULT,
    source_file = normalizePath(path, winslash = "/", mustWork = TRUE),
    retrieved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")
  ) %>%
    mutate(
      coordinate_valid = valid_lonlat(longitude, latitude),
      coordinate_plausible_china = coordinate_valid &
        longitude >= 73 & longitude <= 135 &
        latitude >= 18 & latitude <= 54
    ) %>%
    filter(
      !is.na(station_key),
      nzchar(station_key),
      !grepl("^na\\|", station_key),
      !grepl("\\|na$", station_key),
      coordinate_valid
    )
}

# ----------------------------------------------------------------------------
# 4. Fast CNEMC station-catalogue builders
# ----------------------------------------------------------------------------
station_catalogue_from_metadata <- function(path) {
  if (!file.exists(path)) return(tibble())

  x <- readRDS(path)
  if (!is.data.frame(x) || nrow(x) == 0L) return(tibble())

  network_col <- find_col(x, c("network", "dataset", "source_network"))
  if (!is.na(network_col)) {
    x <- x[as.character(x[[network_col]]) == "CNEMC surface water", , drop = FALSE]
  }
  if (nrow(x) == 0L) return(tibble())

  key_col <- find_col(x, c("station_key", "analysis_station_key"))
  area_col <- find_col(x, c("admin1", "area_cn", "area", "province"))
  city_col <- find_col(x, c("admin2", "city_cn", "city"))
  basin_col <- find_col(x, c("waterbody", "river_basin_cn", "river_basin", "basin"))
  section_col <- find_col(x, c(
    "station_name", "monitoring_section_cn", "monitoring_section", "section_name"
  ))

  if (is.na(area_col) || is.na(section_col)) return(tibble())

  area <- repair_cnemc_mojibake(safe_chr(x, area_col))
  section <- repair_cnemc_mojibake(safe_chr(x, section_col))
  city <- repair_cnemc_mojibake(safe_chr(x, city_col))
  basin <- repair_cnemc_mojibake(safe_chr(x, basin_col))

  key <- if (!is.na(key_col)) {
    as.character(x[[key_col]])
  } else {
    make_station_key(area, section)
  }

  # Rebuild missing keys using exactly the same convention as the crosswalk.
  missing_key <- is.na(key) | !nzchar(key)
  if (any(missing_key)) {
    key[missing_key] <- make_station_key(area[missing_key], section[missing_key])
  }

  tibble(
    station_key = key,
    area_cn = trimws(area),
    city_cn = trimws(city),
    river_basin_cn = trimws(basin),
    monitoring_section_cn = trimws(section)
  ) %>%
    filter(
      !is.na(station_key),
      nzchar(station_key),
      !is.na(monitoring_section_cn),
      nzchar(monitoring_section_cn)
    ) %>%
    distinct(station_key, .keep_all = TRUE) %>%
    arrange(area_cn, monitoring_section_cn)
}

station_catalogue_from_previous_coverage <- function(path) {
  if (!file.exists(path)) return(tibble())

  x <- suppressMessages(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
  if (nrow(x) == 0L || !"station_key" %in% names(x)) return(tibble())

  area_col <- find_col(x, c("area_cn", "admin1", "area", "province"))
  city_col <- find_col(x, c("city_cn", "admin2", "city"))
  basin_col <- find_col(x, c("river_basin_cn", "waterbody", "river_basin", "basin"))
  section_col <- find_col(x, c(
    "monitoring_section_cn", "station_name", "monitoring_section", "section_name"
  ))

  if (is.na(area_col) || is.na(section_col)) return(tibble())

  tibble(
    station_key = as.character(x$station_key),
    area_cn = repair_cnemc_mojibake(safe_chr(x, area_col)),
    city_cn = repair_cnemc_mojibake(safe_chr(x, city_col)),
    river_basin_cn = repair_cnemc_mojibake(safe_chr(x, basin_col)),
    monitoring_section_cn = repair_cnemc_mojibake(safe_chr(x, section_col))
  ) %>%
    filter(!is.na(station_key), nzchar(station_key)) %>%
    distinct(station_key, .keep_all = TRUE)
}

station_catalogue_from_full_analysis <- function(path) {
  if (!file.exists(path)) return(tibble())

  msg("WARNING: compact station metadata unavailable; reading full CNEMC RDS once.")
  msg("         Only distinct station labels will be decoded, not all observations.")

  x <- readRDS(path)
  if (!is.data.frame(x) || nrow(x) == 0L) return(tibble())

  area_col <- find_col(x, c("area_cn", "area", "province"))
  city_col <- find_col(x, c("station_city", "city_cn", "city"))
  basin_col <- find_col(x, c("river_basin_cn", "river_basin", "river"))
  section_col <- find_col(x, c(
    "monitoring_section_cn", "monitoring_section", "station_name", "section_name"
  ))

  if (is.na(area_col) || is.na(section_col)) return(tibble())

  # Critical performance optimization: reduce to distinct labels FIRST.
  slim <- tibble(
    area_raw = safe_chr(x, area_col),
    city_raw = safe_chr(x, city_col),
    basin_raw = safe_chr(x, basin_col),
    section_raw = safe_chr(x, section_col)
  ) %>%
    distinct(area_raw, city_raw, basin_raw, section_raw)

  area <- repair_cnemc_mojibake(slim$area_raw)
  city <- repair_cnemc_mojibake(slim$city_raw)
  basin <- repair_cnemc_mojibake(slim$basin_raw)
  section <- repair_cnemc_mojibake(slim$section_raw)

  tibble(
    station_key = make_station_key(area, section),
    area_cn = trimws(area),
    city_cn = trimws(city),
    river_basin_cn = trimws(basin),
    monitoring_section_cn = trimws(section)
  ) %>%
    filter(
      !is.na(station_key),
      nzchar(station_key),
      !is.na(monitoring_section_cn),
      nzchar(monitoring_section_cn)
    ) %>%
    distinct(station_key, .keep_all = TRUE) %>%
    arrange(area_cn, monitoring_section_cn)
}

load_station_catalogue <- function() {
  # Preferred: small unified station metadata produced by script 12.
  if (file.exists(STATION_METADATA_FILE)) {
    out <- station_catalogue_from_metadata(STATION_METADATA_FILE)
    if (nrow(out) > 0L) {
      readr::write_csv(out, CATALOGUE_FILE, na = "")
      return(list(data = out, source = "station_metadata.rds"))
    }
  }

  # Second choice: compact cache from an earlier successful run.
  if (file.exists(CATALOGUE_FILE)) {
    out <- suppressMessages(
      readr::read_csv(CATALOGUE_FILE, show_col_types = FALSE, progress = FALSE)
    )
    if (nrow(out) > 0L && "station_key" %in% names(out)) {
      out <- out %>% distinct(station_key, .keep_all = TRUE)
      return(list(data = out, source = "cnemc_station_catalogue.csv cache"))
    }
  }

  # Third choice: reuse station identities from previous coverage output.
  out <- station_catalogue_from_previous_coverage(COVERAGE_FILE)
  if (nrow(out) > 0L) {
    readr::write_csv(out, CATALOGUE_FILE, na = "")
    return(list(data = out, source = "previous coverage table"))
  }

  # Last resort: load the large analysis RDS, but decode only distinct labels.
  out <- station_catalogue_from_full_analysis(ANALYSIS_FILE)
  if (nrow(out) > 0L) {
    readr::write_csv(out, CATALOGUE_FILE, na = "")
    return(list(data = out, source = "full CNEMC analysis fallback"))
  }

  list(data = tibble(), source = "none")
}

# ----------------------------------------------------------------------------
# 5. Ingest coordinate files
# ----------------------------------------------------------------------------
files <- list.files(
  INBOX_DIR,
  pattern = "\\.(csv|csv\\.gz|xlsx|xls)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(files) == 0L) {
  stop(
    "No coordinate files found in: ", INBOX_DIR,
    "\nPlace legitimate station-metadata CSV/XLSX exports there and rerun.",
    call. = FALSE
  )
}

msg("Reading ", length(files), " coordinate input file(s) ...")
new_rows <- bind_rows(lapply(files, function(path) {
  standardize_coordinate_file(read_coordinate_file(path), path)
}))

if (nrow(new_rows) == 0L) {
  stop("No usable coordinate rows were recognized.", call. = FALSE)
}

old <- if (file.exists(CROSSWALK_FILE)) {
  suppressMessages(
    readr::read_csv(CROSSWALK_FILE, show_col_types = FALSE, progress = FALSE)
  )
} else {
  tibble()
}

# Normalize the crosswalk schema BEFORE bind_rows().
# readr may auto-parse retrieved_at from an existing CSV as POSIXct, whereas
# freshly ingested rows store it as character text. Explicit coercion here
# prevents vctrs type conflicts and also makes old/new crosswalk versions safe
# to combine when other columns were inferred differently.
normalize_crosswalk_schema <- function(df) {
  n <- nrow(df)

  char_cols <- c(
    "area_cn", "city_cn", "river_basin_cn", "monitoring_section_cn",
    "station_key", "coordinate_source", "source_url", "source_file",
    "retrieved_at"
  )
  num_cols <- c("longitude", "latitude")
  logi_cols <- c("coordinate_valid", "coordinate_plausible_china")

  for (nm in char_cols) {
    if (!nm %in% names(df)) df[[nm]] <- rep(NA_character_, n)
  }
  for (nm in num_cols) {
    if (!nm %in% names(df)) df[[nm]] <- rep(NA_real_, n)
  }
  for (nm in logi_cols) {
    if (!nm %in% names(df)) df[[nm]] <- rep(NA, n)
  }

  # Preserve retrieved_at as sortable ISO-like text in the crosswalk.
  # POSIXct values from older CSV reads are converted explicitly.
  if (inherits(df$retrieved_at, "POSIXt")) {
    df$retrieved_at <- format(
      df$retrieved_at,
      "%Y-%m-%d %H:%M:%S%z",
      tz = "UTC"
    )
  } else {
    df$retrieved_at <- as.character(df$retrieved_at)
  }

  df %>%
    mutate(
      across(all_of(setdiff(char_cols, "retrieved_at")), as.character),
      longitude = suppressWarnings(as.numeric(longitude)),
      latitude = suppressWarnings(as.numeric(latitude)),
      coordinate_valid = as.logical(coordinate_valid),
      coordinate_plausible_china = as.logical(coordinate_plausible_china)
    ) %>%
    select(
      all_of(c(char_cols, num_cols, logi_cols)),
      everything()
    )
}

old <- normalize_crosswalk_schema(old)
new_rows <- normalize_crosswalk_schema(new_rows)

all_rows <- bind_rows(old, new_rows) %>%
  mutate(
    area_cn = repair_cnemc_mojibake(area_cn),
    city_cn = repair_cnemc_mojibake(city_cn),
    river_basin_cn = repair_cnemc_mojibake(river_basin_cn),
    monitoring_section_cn = repair_cnemc_mojibake(monitoring_section_cn),
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude = suppressWarnings(as.numeric(latitude)),
    station_key = ifelse(
      is.na(station_key) | !nzchar(as.character(station_key)),
      make_station_key(area_cn, monitoring_section_cn),
      as.character(station_key)
    ),
    coordinate_valid = valid_lonlat(longitude, latitude),
    coordinate_plausible_china = coordinate_valid &
      longitude >= 73 & longitude <= 135 &
      latitude >= 18 & latitude <= 54
  ) %>%
  filter(coordinate_valid) %>%
  distinct(station_key, longitude, latitude, .keep_all = TRUE)

# ----------------------------------------------------------------------------
# 6. Conflict detection and canonical crosswalk
# ----------------------------------------------------------------------------
# Materially different coordinate candidates are not resolved automatically.
conflicts <- all_rows %>%
  group_by(station_key) %>%
  summarise(
    coordinate_versions = n(),
    longitude_range = max(longitude, na.rm = TRUE) - min(longitude, na.rm = TRUE),
    latitude_range = max(latitude, na.rm = TRUE) - min(latitude, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    coordinate_versions > 1L &
      (longitude_range > 0.001 | latitude_range > 0.001)
  )

if (nrow(conflicts) > 0L) {
  conflict_detail <- all_rows %>% semi_join(conflicts, by = "station_key")
  readr::write_csv(conflict_detail, CONFLICT_FILE, na = "")
  msg("Coordinate conflicts requiring review: ", n_distinct(conflict_detail$station_key))
} else if (file.exists(CONFLICT_FILE)) {
  # Avoid leaving a stale conflict report from an older run.
  unlink(CONFLICT_FILE)
}

# For non-conflicting duplicates, prefer the latest ingested row.
crosswalk <- all_rows %>%
  anti_join(conflicts, by = "station_key") %>%
  arrange(station_key, desc(retrieved_at)) %>%
  distinct(station_key, .keep_all = TRUE) %>%
  arrange(area_cn, city_cn, monitoring_section_cn)

readr::write_csv(crosswalk, CROSSWALK_FILE, na = "")
msg("Crosswalk rows written: ", nrow(crosswalk))
msg("Crosswalk: ", CROSSWALK_FILE)

# ----------------------------------------------------------------------------
# 7. FAST coverage audit
# ----------------------------------------------------------------------------
coverage_started <- Sys.time()
catalogue_result <- load_station_catalogue()
stations <- catalogue_result$data

if (nrow(stations) == 0L) {
  msg("WARNING: no CNEMC station catalogue available; coverage table not written.")
} else {
  msg(
    "CNEMC station catalogue: ", nrow(stations),
    " stations | source: ", catalogue_result$source
  )

  coverage <- stations %>%
    select(
      station_key,
      area_cn,
      city_cn,
      river_basin_cn,
      monitoring_section_cn
    ) %>%
    left_join(
      crosswalk %>%
        select(
          station_key,
          longitude,
          latitude,
          coordinate_source,
          source_url,
          source_file,
          retrieved_at
        ),
      by = "station_key"
    ) %>%
    mutate(
      has_coordinates = valid_lonlat(longitude, latitude),
      coordinate_status = ifelse(has_coordinates, "mapped", "unmapped")
    ) %>%
    arrange(desc(has_coordinates), area_cn, monitoring_section_cn)

  readr::write_csv(coverage, COVERAGE_FILE, na = "")

  n_total <- nrow(coverage)
  n_mapped <- sum(coverage$has_coordinates, na.rm = TRUE)
  pct <- if (n_total > 0L) 100 * n_mapped / n_total else NA_real_
  elapsed <- as.numeric(difftime(Sys.time(), coverage_started, units = "secs"))

  msg(
    "Canonical CNEMC coordinate coverage: ",
    n_mapped, " / ", n_total,
    " (", sprintf("%.1f", pct), "%)"
  )
  msg("Coverage table: ", COVERAGE_FILE)
  msg("Station catalogue cache: ", CATALOGUE_FILE)
  msg("Coverage audit time: ", sprintf("%.2f", elapsed), " seconds")
}
