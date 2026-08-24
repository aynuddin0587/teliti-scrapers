# ============================================================================
# 12_build_analysis_datasets.R
#
# Build a stable, non-destructive canonical analysis layer from the reconciled
# Teliti collection archives.
#
# Design principles
# -----------------
# 1. Never modify collection/archive files.
# 2. Prefer the source that reconciliation shows is most complete/current.
# 3. Preserve source-native published values and provenance.
# 4. Apply only explicit canonicalization rules here (deduplication/versioning,
#    source selection, station metadata attachment, coverage flags).
# 5. Keep volatile collector architecture out of downstream analysis scripts.
#
# Canonical source rules
# ----------------------
# Fujian weekly:
#   Rebuild from GitHub persistent canonical year-state files. This captures
#   current-year source states that may be newer than the PC master.
#
# NMEMC marine:
#   Use the local processed master. Formal reconciliation established exact
#   equivalence with the GitHub-raw reconstruction.
#
# CNEMC surface water:
#   Build a union of:
#     - local/PC cumulative version archive,
#     - GitHub retained nationwide processed checkpoints,
#     - GitHub Fujian-targeted row-version deltas.
#   Deduplicate by row_hash, preserve the union as revision history, and derive
#   one latest-published row per observation_key_hash for ordinary analysis.
#
# ONLIMO daily:
#   Use the local/PC cumulative archive. GitHub daily snapshots are independent
#   validation/backup samples, not the most complete cumulative history.
#
# ONLIMO historical Pollution Index:
#   Reconstruct from all GitHub immutable observation partitions. Deduplicate
#   exact station-date-IP versions, select the latest retrieved version for each
#   station-date, and attach reconciliation station-coverage information.
#
# Outputs
# -------
# teliti_reconciliation/analysis/
#   fujian_weekly_analysis.rds
#   nmemc_marine_analysis.rds
#   cnemc_latest_analysis.rds
#   cnemc_revision_history.rds
#   onlimo_daily_analysis.rds
#   onlimo_historical_analysis.rds
#   station_metadata.rds
#   dataset_inventory.csv
#   analysis_build_manifest.csv
#
# Generated analysis files should normally remain untracked. Version the script,
# not the derived data, unless you intentionally decide otherwise.
# ============================================================================

options(stringsAsFactors = FALSE)

# ----------------------------------------------------------------------------
# 1. Configuration
# ----------------------------------------------------------------------------
PROJECT_DIR <- "D:/# R Project/penelitian"
BACKUP_DIR <- "D:/# R Project/teliti-data-backup"
RECON_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation")
RECON_OUTPUT_DIR <- file.path(RECON_DIR, "output")
ANALYSIS_DIR <- file.path(RECON_DIR, "analysis")

REQUIRE_RECONCILIATION <- TRUE
ALLOW_FUJIAN_LOCAL_FALLBACK <- FALSE
WRITE_BUILD_MANIFEST <- TRUE

# Canonical paths -------------------------------------------------------------
FUJIAN_GITHUB_STATE <- file.path(
  BACKUP_DIR, "fujian_weekly_surfacewater", "state", "source"
)
FUJIAN_LOCAL_SOURCE <- file.path(
  PROJECT_DIR, "fujian_surfacewater", "data", "source"
)
FUJIAN_LOCAL_PROCESSED <- file.path(
  PROJECT_DIR, "fujian_surfacewater", "data", "processed"
)

NMEMC_MARINE_MASTER <- file.path(
  PROJECT_DIR, "nmemc", "data", "processed", "nmemc_water_master.rds"
)

CNEMC_PC_MASTER <- file.path(
  PROJECT_DIR, "nmemc", "data", "surfacewater", "processed",
  "nmemc_surfacewater_observations.rds"
)
CNEMC_GITHUB_ROOT <- file.path(BACKUP_DIR, "cnemc_surfacewater")

ONLIMO_DAILY_ARCHIVE <- file.path(
  PROJECT_DIR, "onlimo", "data", "onlimo_daily_parameters_archive.csv"
)
ONLIMO_CATALOG <- file.path(
  PROJECT_DIR, "onlimo", "data", "onlimo_station_catalog.csv"
)
ONLIMO_HIST_GITHUB_ROOT <- file.path(BACKUP_DIR, "onlimo_pollution_index")
ONLIMO_HIST_COVERAGE <- file.path(
  RECON_OUTPUT_DIR, "onlimo_historical", "onlimo_historical_station_coverage.csv"
)

# Required reconciliation evidence ------------------------------------------
RECON_EVIDENCE <- c(
  fujian = file.path(
    RECON_OUTPUT_DIR, "fujian_weekly", "fujian_reconciliation_summary.txt"
  ),
  nmemc_marine = file.path(
    RECON_OUTPUT_DIR, "nmemc_marine", "nmemc_marine_reconciliation_summary.csv"
  ),
  cnemc_rows = file.path(
    RECON_OUTPUT_DIR, "cnemc", "cnemc_row_reconciliation_global_summary.csv"
  ),
  cnemc_revisions = file.path(
    RECON_OUTPUT_DIR, "cnemc", "cnemc_revision_magnitude_summary.csv"
  ),
  onlimo_daily = file.path(
    RECON_OUTPUT_DIR, "onlimo_daily"
  ),
  onlimo_historical = file.path(
    RECON_OUTPUT_DIR, "onlimo_historical", "onlimo_historical_reconciliation_summary.txt"
  )
)

# ----------------------------------------------------------------------------
# 2. Packages
# ----------------------------------------------------------------------------
required_packages <- c("dplyr", "readr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them first with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------------------------
# 3. General helpers
# ----------------------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", ...)
}

trim_na <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) return(NULL)
  suppressMessages(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE,
      ...
    )
  )
}

save_rds_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "analysis_", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, compress = "xz", version = 3)
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) {
    stop("Failed to atomically replace analysis file: ", path, call. = FALSE)
  }
  invisible(path)
}

write_csv_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "analysis_", tmpdir = dirname(path), fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(x, tmp, na = "")
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) {
    stop("Failed to atomically replace analysis CSV: ", path, call. = FALSE)
  }
  invisible(path)
}

md5_if_file <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return(NA_character_)
  unname(as.character(tools::md5sum(path)))
}

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) NA_character_ else hit[[1]]
}

find_recursive <- function(root, pattern) {
  if (!dir.exists(root)) return(character())
  list.files(
    root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )
}

col_or_na <- function(df, candidates, default = NA_character_) {
  nm <- names(df)
  low <- tolower(nm)
  for (cand in candidates) {
    idx <- match(tolower(cand), low)
    if (!is.na(idx)) return(df[[idx]])
  }
  rep(default, nrow(df))
}

find_col <- function(df, candidates) {
  nm <- names(df)
  low <- tolower(nm)
  for (cand in candidates) {
    idx <- match(tolower(cand), low)
    if (!is.na(idx)) return(nm[[idx]])
  }
  NA_character_
}

find_coord_col_flexible <- function(df, type = c("lon", "lat")) {
  type <- match.arg(type)
  nm <- names(df)
  low <- tolower(nm)

  if (type == "lon") {
    exact <- c(
      "longitude", "lon", "lng", "station_longitude", "site_longitude",
      "longitude_final", "station_lon", "lon_final", "coord_lon", "经度"
    )
    patterns <- c("longitude", "(^|_)lon($|_)", "lng", "经度")
  } else {
    exact <- c(
      "latitude", "lat", "station_latitude", "site_latitude",
      "latitude_final", "station_lat", "lat_final", "coord_lat", "纬度"
    )
    patterns <- c("latitude", "(^|_)lat($|_)", "纬度")
  }

  hit <- find_col(df, exact)
  if (!is.na(hit)) return(hit)

  for (pat in patterns) {
    idx <- grep(pat, low, perl = TRUE)
    if (length(idx) > 0L) return(nm[[idx[[1]]]])
  }
  NA_character_
}

coalesce_candidate_cols <- function(df, candidates) {
  if (is.null(df) || nrow(df) == 0L) return(character())
  out <- rep(NA_character_, nrow(df))
  for (cand in candidates) {
    nm <- find_col(df, cand)
    if (is.na(nm)) next
    z <- trim_na(df[[nm]])
    fill <- is.na(out) & !is.na(z)
    out[fill] <- z[fill]
  }
  out
}

first_non_missing_chr <- function(x) {
  z <- trim_na(x)
  z <- z[!is.na(z)]
  if (length(z) == 0L) return(NA_character_)
  z[[1]]
}

first_non_missing_num <- function(x) {
  z <- suppressWarnings(as.numeric(x))
  z <- z[is.finite(z)]
  if (length(z) == 0L) return(NA_real_)
  z[[1]]
}

valid_lonlat <- function(lon, lat) {
  is.finite(lon) & is.finite(lat) &
    lon >= -180 & lon <= 180 & lat >= -90 & lat <= 90
}

parse_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  ch <- trim_na(x)
  out <- as.Date(rep(NA_character_, length(ch)))

  specs <- list(
    c("^\\d{4}-\\d{2}-\\d{2}$", "%Y-%m-%d"),
    c("^\\d{4}/\\d{2}/\\d{2}$", "%Y/%m/%d"),
    c("^\\d{8}$", "%Y%m%d")
  )
  for (sp in specs) {
    idx <- !is.na(ch) & grepl(sp[[1]], ch)
    if (any(idx)) {
      out[idx] <- as.Date(ch[idx], format = sp[[2]])
    }
  }
  out
}

parse_posix_safe <- function(x, tz = "Asia/Taipei") {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = tz))

  ch <- trim_na(x)
  out <- as.POSIXct(rep(NA_character_, length(ch)), tz = tz)
  if (length(ch) == 0L) return(out)

  formats <- c(
    "%Y-%m-%d %H:%M:%S%z",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%dT%H:%M:%S%z",
    "%Y-%m-%dT%H:%M:%SZ"
  )

  remaining <- which(!is.na(ch))
  for (fmt in formats) {
    if (length(remaining) == 0L) break
    parsed <- suppressWarnings(as.POSIXct(ch[remaining], format = fmt, tz = tz))
    ok <- !is.na(parsed)
    if (any(ok)) {
      out[remaining[ok]] <- parsed[ok]
      remaining <- remaining[!ok]
    }
  }

  out
}

coalesce_posix_max <- function(...) {
  xs <- list(...)
  if (length(xs) == 0L) return(as.POSIXct(character()))
  n <- max(vapply(xs, length, integer(1)))
  if (n == 0L) return(as.POSIXct(character()))

  vals <- lapply(xs, function(x) {
    y <- parse_posix_safe(x)
    if (length(y) == 1L && n > 1L) y <- rep(y, n)
    y
  })

  mat <- do.call(cbind, lapply(vals, as.numeric))
  ans <- apply(mat, 1, function(z) {
    if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)
  })
  as.POSIXct(ans, origin = "1970-01-01", tz = "Asia/Taipei")
}

normalize_station_key <- function(x) {
  z <- trim_na(x)
  if (length(z) == 0L) return(z)
  full_width_space <- intToUtf8(12288L)
  z <- gsub(full_width_space, "", z, fixed = TRUE)
  z <- gsub("[[:space:]]+", "", z, perl = TRUE)
  tolower(z)
}

assert_unique <- function(df, key_cols, label) {
  if (nrow(df) == 0L) return(invisible(TRUE))
  dup <- df %>% count(across(all_of(key_cols)), name = "n") %>% filter(n > 1L)
  if (nrow(dup) > 0L) {
    stop(label, " is not unique by: ", paste(key_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

prefix_except <- function(df, prefix, keep) {
  rename_cols <- setdiff(names(df), keep)
  names(df)[match(rename_cols, names(df))] <- paste0(prefix, rename_cols)
  df
}

# ----------------------------------------------------------------------------
# 4. Reconciliation gate
# ----------------------------------------------------------------------------
if (isTRUE(REQUIRE_RECONCILIATION)) {
  evidence_ok <- vapply(
    RECON_EVIDENCE,
    function(x) if (dir.exists(x)) TRUE else file.exists(x),
    logical(1)
  )
  if (!all(evidence_ok)) {
    missing <- names(RECON_EVIDENCE)[!evidence_ok]
    stop(
      "Required reconciliation evidence is missing for: ",
      paste(missing, collapse = ", "),
      "\nRun the reconciliation scripts before rebuilding the canonical analysis layer.",
      call. = FALSE
    )
  }
}

# ----------------------------------------------------------------------------
# 5. Fujian weekly: rebuild canonical master from GitHub persistent state
# ----------------------------------------------------------------------------
source_col_or_na <- function(df, nm) {
  if (nm %in% names(df)) return(df[[nm]])
  rep(NA_character_, nrow(df))
}

was5_period_to_date <- function(x) {
  z <- trim_na(x)
  out <- as.Date(rep(NA_character_, length(z)))

  is_serial <- !is.na(z) & grepl("^[0-9]+(?:\\.[0-9]+)?$", z)
  if (any(is_serial)) {
    nums <- suppressWarnings(as.numeric(z[is_serial]))
    out[is_serial] <- as.Date(nums, origin = "1899-12-30")
  }

  is_dmy <- !is.na(z) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", z)
  if (any(is_dmy)) {
    out[is_dmy] <- as.Date(z[is_dmy], format = "%d/%m/%Y")
  }

  is_iso <- !is.na(z) & grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", z)
  if (any(is_iso)) {
    out[is_iso] <- as.Date(z[is_iso], format = "%Y-%m-%d")
  }

  out
}

normalize_fujian_station_name <- function(x) {
  z <- trim_na(x)
  if (length(z) == 0L) return(z)
  full_width_space <- intToUtf8(12288L)
  z <- gsub(full_width_space, "", z, fixed = TRUE)
  z <- gsub("[[:space:]]+", "", z, perl = TRUE)
  trim_na(z)
}

normalize_source_label <- function(x) {
  z <- trim_na(x)
  if (length(z) == 0L) return(z)
  full_width_space <- intToUtf8(12288L)
  z <- gsub(full_width_space, " ", z, fixed = TRUE)
  z <- gsub("[[:space:]]+", " ", z, perl = TRUE)
  trim_na(z)
}

standardize_fujian_year <- function(raw_df, year, meta, source_filename) {
  required <- c(
    "s1", "s2", "s3", "s4", "s5",
    "f1", "f2", "f3", "f4", "f5", "f6",
    "s8", "s9", "s10"
  )
  missing <- setdiff(required, names(raw_df))
  if (length(missing) > 0L) {
    stop(
      "Fujian canonical year ", year,
      " is missing expected field(s): ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(raw_df) == 0L) return(tibble())

  out <- tibble(
    source_recid = trim_na(source_col_or_na(raw_df, "recid")),
    source_metadataid = trim_na(source_col_or_na(raw_df, "metadataid")),
    source_docorder = trim_na(source_col_or_na(raw_df, "docorder")),
    river_system_raw = trim_na(raw_df$s1),
    river_system = normalize_source_label(raw_df$s1),
    station_name_raw = trim_na(raw_df$s2),
    station_name = normalize_fujian_station_name(raw_df$s2),
    section_status_raw = trim_na(raw_df$s3),
    section_status = normalize_source_label(raw_df$s3),
    year = suppressWarnings(as.integer(trim_na(raw_df$s4))),
    week = suppressWarnings(as.integer(trim_na(raw_df$s5))),
    source_period_start_raw = trim_na(source_col_or_na(raw_df, "s6")),
    source_period_end_raw = trim_na(source_col_or_na(raw_df, "s7")),
    report_period_start = was5_period_to_date(source_col_or_na(raw_df, "s6")),
    report_period_end = was5_period_to_date(source_col_or_na(raw_df, "s7")),
    ph_raw = trim_na(raw_df$f1),
    dissolved_oxygen_mg_l_raw = trim_na(raw_df$f2),
    permanganate_index_mg_l_raw = trim_na(raw_df$f3),
    total_phosphorus_mg_l_raw = trim_na(raw_df$f4),
    ammonia_nitrogen_mg_l_raw = trim_na(raw_df$f5),
    total_nitrogen_mg_l_raw = trim_na(raw_df$f6),
    previous_week_water_quality = trim_na(raw_df$s8),
    current_week_water_quality = trim_na(raw_df$s9),
    main_pollution_indicator = trim_na(raw_df$s10),
    source_page_number = if ("source_page" %in% names(raw_df)) {
      suppressWarnings(as.integer(raw_df$source_page))
    } else {
      NA_integer_
    },
    source_year_file = source_filename,
    source_snapshot_md5 = as.character(meta$snapshot_md5 %||% NA_character_),
    source_checked_at = as.character(meta$checked_at %||% NA_character_)
  )

  out$report_year_week <- ifelse(
    !is.na(out$year) & !is.na(out$week),
    sprintf("%04d-week-%02d", out$year, out$week),
    NA_character_
  )

  fallback_key <- paste(
    out$year,
    out$week,
    ifelse(is.na(out$river_system), "", out$river_system),
    ifelse(is.na(out$station_name), "", out$station_name),
    ifelse(is.na(out$source_docorder), "", out$source_docorder),
    sep = "|"
  )

  out$observation_key <- ifelse(
    !is.na(out$source_recid) & nzchar(out$source_recid),
    paste0("recid:", out$source_recid),
    fallback_key
  )

  out
}

find_fujian_crosswalk <- function() {
  candidates <- c(
    file.path(FUJIAN_LOCAL_PROCESSED, "fujian_station_crosswalk.csv"),
    file.path(PROJECT_DIR, "fujian_surfacewater", "data", "fujian_station_crosswalk.csv"),
    file.path(PROJECT_DIR, "fujian_surfacewater", "fujian_station_crosswalk.csv")
  )
  direct <- first_existing(candidates)
  if (!is.na(direct)) return(direct)

  all <- find_recursive(
    file.path(PROJECT_DIR, "fujian_surfacewater"),
    "^fujian_station_crosswalk\\.csv$"
  )
  if (length(all) == 0L) NA_character_ else all[[1]]
}

build_fujian_crosswalk_lookup <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  cw <- safe_read_csv(path)
  if (is.null(cw) || nrow(cw) == 0L) return(NULL)

  name_col <- find_col(
    cw,
    c(
      "station_name", "station_name_cn", "station_name_zh",
      "station", "site_name", "站点名称"
    )
  )
  raw_name_col <- find_col(
    cw,
    c("station_name_raw", "station_raw", "site_name_raw")
  )
  lon_col <- find_coord_col_flexible(cw, "lon")
  lat_col <- find_coord_col_flexible(cw, "lat")
  city_col <- find_col(
    cw,
    c("city", "prefecture", "municipality", "admin_city", "city_name")
  )
  en_col <- find_col(
    cw,
    c("station_name_en", "english_name", "name_en")
  )
  id_col <- find_col(
    cw,
    c("mn", "station_id", "site_code", "station_code")
  )

  if (is.na(name_col) && is.na(raw_name_col)) return(NULL)

  primary_name <- if (!is.na(name_col)) cw[[name_col]] else cw[[raw_name_col]]
  secondary_name <- if (!is.na(raw_name_col)) cw[[raw_name_col]] else primary_name

  base_lookup <- tibble(
    primary_key = normalize_station_key(primary_name),
    raw_key = normalize_station_key(secondary_name),
    station_name_crosswalk = as.character(primary_name),
    station_name_en = if (!is.na(en_col)) as.character(cw[[en_col]]) else NA_character_,
    station_external_id = if (!is.na(id_col)) as.character(cw[[id_col]]) else NA_character_,
    station_city = if (!is.na(city_col)) as.character(cw[[city_col]]) else NA_character_,
    station_longitude = if (!is.na(lon_col)) {
      suppressWarnings(readr::parse_number(as.character(cw[[lon_col]])))
    } else {
      NA_real_
    },
    station_latitude = if (!is.na(lat_col)) {
      suppressWarnings(readr::parse_number(as.character(cw[[lat_col]])))
    } else {
      NA_real_
    },
    station_metadata_source = basename(path)
  )

  metadata_cols <- setdiff(names(base_lookup), c("primary_key", "raw_key"))
  primary_lookup <- base_lookup %>%
    mutate(station_name_key = primary_key) %>%
    select(station_name_key, all_of(metadata_cols))
  raw_lookup <- base_lookup %>%
    mutate(station_name_key = raw_key) %>%
    select(station_name_key, all_of(metadata_cols))

  bind_rows(primary_lookup, raw_lookup) %>%
    filter(!is.na(station_name_key), nzchar(station_name_key)) %>%
    arrange(
      station_name_key,
      desc(!is.na(station_longitude) & !is.na(station_latitude))
    ) %>%
    distinct(station_name_key, .keep_all = TRUE)
}

build_fujian_crosswalk_station_rows <- function(path) {
  if (is.na(path) || !file.exists(path)) return(tibble())
  cw <- safe_read_csv(path)
  if (is.null(cw) || nrow(cw) == 0L) return(tibble())

  lon_col <- find_coord_col_flexible(cw, "lon")
  lat_col <- find_coord_col_flexible(cw, "lat")
  if (is.na(lon_col) || is.na(lat_col)) return(tibble())

  raw_name <- coalesce_candidate_cols(
    cw,
    c(
      "station_name_raw", "station_name_zh", "station_name_cn", "站点名称",
      "station_name", "station", "site_name", "station_name_en"
    )
  )
  display_name <- coalesce_candidate_cols(
    cw,
    c(
      "station_name_en", "station_name", "station_name_raw",
      "station_name_zh", "station_name_cn", "站点名称", "station", "site_name"
    )
  )
  station_id <- coalesce_candidate_cols(
    cw,
    c("MN", "mn", "station_id", "site_code", "station_code")
  )
  city <- coalesce_candidate_cols(
    cw,
    c("city", "city_en", "prefecture", "municipality", "admin_city", "city_name")
  )

  lon <- suppressWarnings(readr::parse_number(as.character(cw[[lon_col]])))
  lat <- suppressWarnings(readr::parse_number(as.character(cw[[lat_col]])))
  key_name <- ifelse(!is.na(raw_name), raw_name, display_name)

  tibble(
    network = "Fujian weekly",
    station_id = station_id,
    station_name = ifelse(!is.na(display_name), display_name, raw_name),
    station_name_en = coalesce_candidate_cols(cw, c("station_name_en", "english_name", "name_en")),
    waterbody = NA_character_,
    admin1 = "Fujian",
    admin2 = city,
    longitude = lon,
    latitude = lat,
    coordinate_source = paste0("fujian_crosswalk:", basename(path)),
    station_key = normalize_station_key(key_name)
  ) %>%
    filter(!is.na(station_key), nzchar(station_key), valid_lonlat(longitude, latitude)) %>%
    arrange(station_key) %>%
    distinct(network, station_key, .keep_all = TRUE)
}

build_fujian <- function() {
  msg("Building Fujian weekly canonical analysis dataset ...")

  source_dir <- FUJIAN_GITHUB_STATE
  source_basis <- "github_persistent_canonical_state"

  if (!dir.exists(source_dir)) {
    if (!ALLOW_FUJIAN_LOCAL_FALLBACK) {
      stop(
        "GitHub Fujian canonical state directory is missing: ", source_dir,
        "\nPull teliti-data-backup before running this script.",
        call. = FALSE
      )
    }
    source_dir <- FUJIAN_LOCAL_SOURCE
    source_basis <- "local_source_fallback"
  }

  year_files <- list.files(
    source_dir,
    pattern = "^fujian_weekly_[0-9]{4}\\.rds$",
    full.names = TRUE
  )
  if (length(year_files) == 0L) {
    stop("No Fujian canonical year RDS files found in: ", source_dir, call. = FALSE)
  }

  years <- as.integer(sub(
    "^fujian_weekly_([0-9]{4})\\.rds$", "\\1", basename(year_files)
  ))
  ord <- order(years)
  years <- years[ord]
  year_files <- year_files[ord]

  parts <- vector("list", length(year_files))
  for (i in seq_along(year_files)) {
    yr <- years[[i]]
    raw <- readRDS(year_files[[i]])
    meta_file <- file.path(source_dir, sprintf("fujian_weekly_%d_meta.rds", yr))
    meta <- if (file.exists(meta_file)) {
      tryCatch(readRDS(meta_file), error = function(e) list())
    } else {
      list()
    }
    parts[[i]] <- standardize_fujian_year(raw, yr, meta, basename(year_files[[i]]))
  }

  dat <- bind_rows(parts) %>%
    arrange(year, week, river_system, station_name) %>%
    mutate(
      analysis_source_basis = source_basis,
      station_name_key = normalize_station_key(station_name),
      station_name_raw_key = normalize_station_key(station_name_raw)
    )

  if (anyDuplicated(dat$observation_key)) {
    stop(
      "Fujian canonical analysis dataset contains duplicated observation_key values.",
      call. = FALSE
    )
  }

  crosswalk_path <- find_fujian_crosswalk()
  lookup <- build_fujian_crosswalk_lookup(crosswalk_path)
  if (!is.null(lookup)) {
    dat <- dat %>%
      left_join(
        lookup,
        by = "station_name_key"
      )
  } else {
    dat <- dat %>%
      mutate(
        station_name_crosswalk = NA_character_,
        station_name_en = NA_character_,
        station_external_id = NA_character_,
        station_city = NA_character_,
        station_longitude = NA_real_,
        station_latitude = NA_real_,
        station_metadata_source = NA_character_
      )
  }

  dat
}

# ----------------------------------------------------------------------------
# 6. NMEMC marine: exact reconciled local processed master
# ----------------------------------------------------------------------------
build_nmemc_marine <- function() {
  msg("Building NMEMC marine canonical analysis dataset ...")
  if (!file.exists(NMEMC_MARINE_MASTER)) {
    stop("NMEMC marine master is missing: ", NMEMC_MARINE_MASTER, call. = FALSE)
  }
  dat <- readRDS(NMEMC_MARINE_MASTER)
  dat$analysis_source_basis <- "local_processed_master_exactly_reconciled"
  dat
}

# ----------------------------------------------------------------------------
# 7. CNEMC: reconciled union of PC versions + GitHub checkpoints + target deltas
# ----------------------------------------------------------------------------
# CNEMC CSV files come from multiple archive products and readr may infer
# different column types from different files (for example a class code can
# be numeric in one CSV and character in another). Read these files
# conservatively as character first. They are cast to the processed PC-master
# schema immediately before the reconciled union is assembled.
read_cnemc_csv_files <- function(files, source_label) {
  if (length(files) == 0L) return(tibble())
  pieces <- vector("list", length(files))

  for (i in seq_along(files)) {
    x <- safe_read_csv(
      files[[i]],
      col_types = readr::cols(.default = readr::col_character())
    )
    if (is.null(x) || nrow(x) == 0L) next
    x$analysis_archive_source <- source_label
    x$analysis_archive_file <- normalizePath(files[[i]], winslash = "/", mustWork = TRUE)
    pieces[[i]] <- x
  }

  bind_rows(pieces)
}

cast_like_reference <- function(x, reference, column_name = "") {
  # Preserve exact published strings whenever the canonical processed schema
  # treats the field as character. This includes *_raw values, hashes and the
  # current water_quality_class_code field.
  if (is.character(reference) || is.factor(reference)) {
    return(as.character(x))
  }

  if (inherits(reference, "POSIXt")) {
    return(parse_posix_safe(x))
  }

  if (inherits(reference, "Date")) {
    return(parse_date_safe(x))
  }

  if (is.integer(reference)) {
    return(suppressWarnings(as.integer(x)))
  }

  if (is.double(reference) || is.numeric(reference)) {
    return(suppressWarnings(as.numeric(x)))
  }

  if (is.logical(reference)) {
    ch <- tolower(trim_na(x))
    out <- rep(NA, length(ch))
    out[ch %in% c("true", "t", "1", "yes", "y")] <- TRUE
    out[ch %in% c("false", "f", "0", "no", "n")] <- FALSE
    return(out)
  }

  # Unknown/list-like classes should not block reconstruction of the
  # analytical archive. Character is the safest lossless CSV representation.
  as.character(x)
}

align_cnemc_schema <- function(df, reference) {
  if (is.null(df) || nrow(df) == 0L) return(df)

  common <- intersect(names(df), names(reference))
  for (nm in common) {
    df[[nm]] <- cast_like_reference(df[[nm]], reference[[nm]], nm)
  }

  # These analysis-only provenance fields are intentionally character in all
  # archive sources regardless of how readr might otherwise infer them.
  for (nm in intersect(c("analysis_archive_source", "analysis_archive_file"), names(df))) {
    df[[nm]] <- as.character(df[[nm]])
  }

  df
}

build_cnemc <- function() {
  msg("Building CNEMC reconciled version union ...")

  if (!file.exists(CNEMC_PC_MASTER)) {
    stop("CNEMC PC cumulative master is missing: ", CNEMC_PC_MASTER, call. = FALSE)
  }

  pc <- readRDS(CNEMC_PC_MASTER) %>%
    mutate(
      analysis_archive_source = "pc_cumulative_master",
      analysis_archive_file = normalizePath(CNEMC_PC_MASTER, winslash = "/", mustWork = TRUE)
    )

  checkpoint_files <- find_recursive(
    file.path(CNEMC_GITHUB_ROOT, "snapshots"),
    "_processed\\.csv\\.gz$"
  )
  gh_checkpoints <- read_cnemc_csv_files(
    checkpoint_files,
    "github_full_checkpoint"
  )

  targeted_files <- find_recursive(
    file.path(CNEMC_GITHUB_ROOT, "deltas", "targeted"),
    "^row_versions_.*\\.csv\\.gz$"
  )
  gh_targeted <- read_cnemc_csv_files(
    targeted_files,
    "github_targeted_delta"
  )

  # Harmonize the CSV-derived archives to the processed PC master schema
  # before row-binding. This prevents readr type-guess differences from
  # becoming false schema conflicts while preserving the canonical types.
  gh_checkpoints <- align_cnemc_schema(gh_checkpoints, pc)
  gh_targeted <- align_cnemc_schema(gh_targeted, pc)

  all_versions <- bind_rows(pc, gh_checkpoints, gh_targeted)

  msg(
    "CNEMC union rows before row-hash deduplication: ", nrow(all_versions),
    " | PC: ", nrow(pc),
    " | GitHub checkpoints: ", nrow(gh_checkpoints),
    " | GitHub targeted deltas: ", nrow(gh_targeted)
  )

  required <- c("row_hash", "observation_key_hash")
  missing <- setdiff(required, names(all_versions))
  if (length(missing) > 0L) {
    stop(
      "CNEMC version union is missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  all_versions <- all_versions %>%
    filter(
      !is.na(row_hash), nzchar(as.character(row_hash)),
      !is.na(observation_key_hash), nzchar(as.character(observation_key_hash))
    )

  # A single row_hash can appear in both collectors/checkpoints. Preserve one
  # scientific row while recording every archive source that represented it.
  source_map <- all_versions %>%
    group_by(row_hash) %>%
    summarise(
      analysis_version_sources = paste(
        sort(unique(analysis_archive_source)), collapse = ";"
      ),
      analysis_version_source_files_n = n_distinct(analysis_archive_file),
      .groups = "drop"
    )

  # Derive a comparable "seen/published by collection" timestamp before
  # deduplication. last_seen is valuable for the PC cumulative archive;
  # delta_archived_at / collected_at carry the GitHub publication capture time.
  n <- nrow(all_versions)
  get_time_col <- function(nm) {
    if (nm %in% names(all_versions)) all_versions[[nm]] else rep(NA_character_, n)
  }

  all_versions$analysis_version_seen_at <- coalesce_posix_max(
    get_time_col("last_seen"),
    get_time_col("delta_archived_at"),
    get_time_col("collected_at"),
    get_time_col("first_seen")
  )

  history <- all_versions %>%
    arrange(row_hash, desc(analysis_version_seen_at)) %>%
    distinct(row_hash, .keep_all = TRUE) %>%
    left_join(source_map, by = "row_hash")

  revision_counts <- history %>%
    count(observation_key_hash, name = "analysis_revision_count")

  history <- history %>%
    left_join(revision_counts, by = "observation_key_hash") %>%
    arrange(observation_key_hash, analysis_version_seen_at, row_hash)

  latest <- history %>%
    group_by(observation_key_hash) %>%
    arrange(desc(analysis_version_seen_at), desc(row_hash), .by_group = TRUE) %>%
    slice(1L) %>%
    ungroup() %>%
    mutate(
      analysis_source_basis = "reconciled_pc_plus_github_version_union",
      analysis_version_rule = "latest_seen_row_version_per_observation_key_hash"
    )

  assert_unique(history, "row_hash", "CNEMC revision history")
  assert_unique(latest, "observation_key_hash", "CNEMC latest analysis view")

  list(history = history, latest = latest)
}

# ----------------------------------------------------------------------------
# 8. ONLIMO daily: cumulative PC archive
# ----------------------------------------------------------------------------
build_onlimo_daily <- function() {
  msg("Building ONLIMO daily canonical analysis dataset ...")
  if (!file.exists(ONLIMO_DAILY_ARCHIVE)) {
    stop("ONLIMO daily cumulative archive is missing: ", ONLIMO_DAILY_ARCHIVE, call. = FALSE)
  }

  dat <- safe_read_csv(
    ONLIMO_DAILY_ARCHIVE,
    col_types = cols(
      station_id = col_character(),
      date = col_date(),
      .default = col_guess()
    )
  )

  required <- c("station_id", "date")
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0L) {
    stop("ONLIMO daily archive is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  dat <- dat %>%
    arrange(station_id, date) %>%
    distinct(station_id, date, .keep_all = TRUE) %>%
    mutate(analysis_source_basis = "pc_cumulative_archive_independently_reconciled")

  assert_unique(dat, c("station_id", "date"), "ONLIMO daily analysis dataset")
  dat
}

# ----------------------------------------------------------------------------
# 9. ONLIMO historical: reconstruct GitHub immutable partitions
# ----------------------------------------------------------------------------
build_onlimo_historical <- function() {
  msg("Building ONLIMO historical canonical analysis dataset ...")

  snapshot_root <- file.path(ONLIMO_HIST_GITHUB_ROOT, "snapshots")
  files <- find_recursive(
    snapshot_root,
    "^onlimo_pollution_index_.*\\.csv\\.gz$"
  )
  if (length(files) == 0L) {
    stop(
      "No ONLIMO historical GitHub observation partitions found under: ",
      snapshot_root,
      call. = FALSE
    )
  }

  pieces <- vector("list", length(files))
  for (i in seq_along(files)) {
    x <- safe_read_csv(
      files[[i]],
      col_types = cols(
        station_id = col_character(),
        date = col_date(),
        .default = col_guess()
      )
    )
    if (is.null(x) || nrow(x) == 0L) next
    x$analysis_partition_file <- normalizePath(files[[i]], winslash = "/", mustWork = TRUE)
    x$analysis_partition_order <- i
    pieces[[i]] <- x
  }

  raw <- bind_rows(pieces)
  if (nrow(raw) == 0L) {
    stop("ONLIMO historical GitHub partitions contain no observations.", call. = FALSE)
  }

  required <- c("station_id", "date", "pollution_index")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0L) {
    stop(
      "ONLIMO historical partitions are missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Preserve only one copy of identical station-date-IP versions across
  # partitions. Prefer the latest retrieved_at when present, then later file.
  if ("retrieved_at" %in% names(raw)) {
    raw$analysis_retrieved_at <- parse_posix_safe(raw$retrieved_at)
  } else {
    raw$analysis_retrieved_at <- as.POSIXct(
      rep(NA_real_, nrow(raw)), origin = "1970-01-01", tz = "Asia/Taipei"
    )
  }

  versions <- raw %>%
    arrange(
      station_id,
      date,
      pollution_index,
      desc(analysis_retrieved_at),
      desc(analysis_partition_order)
    ) %>%
    distinct(station_id, date, pollution_index, .keep_all = TRUE)

  version_counts <- versions %>%
    count(station_id, date, name = "analysis_ip_version_count")

  latest <- versions %>%
    group_by(station_id, date) %>%
    arrange(
      desc(analysis_retrieved_at),
      desc(analysis_partition_order),
      .by_group = TRUE
    ) %>%
    slice(1L) %>%
    ungroup() %>%
    left_join(version_counts, by = c("station_id", "date")) %>%
    mutate(
      analysis_source_basis = "github_reconstructed_immutable_partitions",
      analysis_version_rule = "latest_retrieved_version_per_station_date"
    )

  # Attach the latest reconciliation coverage table without guessing its schema.
  coverage <- safe_read_csv(ONLIMO_HIST_COVERAGE)
  if (!is.null(coverage) && nrow(coverage) > 0L && "station_id" %in% names(coverage)) {
    coverage <- coverage %>%
      mutate(station_id = as.character(station_id)) %>%
      arrange(station_id) %>%
      distinct(station_id, .keep_all = TRUE)
    coverage <- prefix_except(coverage, "coverage_", keep = "station_id")
    latest <- latest %>% left_join(coverage, by = "station_id")
  } else {
    warning(
      "ONLIMO historical station-coverage reconciliation table was not attached: ",
      ONLIMO_HIST_COVERAGE,
      call. = FALSE
    )
  }

  assert_unique(latest, c("station_id", "date"), "ONLIMO historical analysis dataset")
  latest
}

# ----------------------------------------------------------------------------
# 10. Unified station metadata
# ----------------------------------------------------------------------------
build_station_metadata <- function(fujian, nmemc, cnemc_latest, onlimo_daily, onlimo_hist) {
  msg("Building unified station metadata ...")

  # Fujian -------------------------------------------------------------------
  # Start with stations represented in the canonical observation archive.
  fujian_observed <- fujian %>%
    group_by(station_name_key) %>%
    summarise(
      network = "Fujian weekly",
      station_id = first_non_missing_chr(station_external_id),
      station_name = first_non_missing_chr(station_name),
      station_name_en = first_non_missing_chr(station_name_en),
      waterbody = first_non_missing_chr(river_system),
      admin1 = "Fujian",
      admin2 = first_non_missing_chr(station_city),
      longitude = first_non_missing_num(station_longitude),
      latitude = first_non_missing_num(station_latitude),
      coordinate_source = first_non_missing_chr(station_metadata_source),
      .groups = "drop"
    ) %>%
    mutate(station_key = station_name_key) %>%
    select(-station_name_key)

  # Also read coordinate-bearing crosswalk rows directly. This is deliberate:
  # recovered coordinates must not disappear merely because a translated or
  # normalized station name fails an exact observation-to-crosswalk join.
  fujian_crosswalk <- build_fujian_crosswalk_station_rows(find_fujian_crosswalk())

  fujian_station <- bind_rows(fujian_observed, fujian_crosswalk) %>%
    mutate(
      has_coordinate = valid_lonlat(longitude, latitude),
      coordinate_priority = case_when(
        grepl("^fujian_crosswalk:", coordinate_source %||% "") & has_coordinate ~ 1L,
        has_coordinate ~ 2L,
        TRUE ~ 3L
      )
    ) %>%
    arrange(station_key, coordinate_priority) %>%
    group_by(network, station_key) %>%
    summarise(
      station_id = first_non_missing_chr(station_id),
      station_name = first_non_missing_chr(station_name),
      station_name_en = first_non_missing_chr(station_name_en),
      waterbody = first_non_missing_chr(waterbody),
      admin1 = first_non_missing_chr(admin1),
      admin2 = first_non_missing_chr(admin2),
      longitude = first_non_missing_num(longitude),
      latitude = first_non_missing_num(latitude),
      coordinate_source = first_non_missing_chr(coordinate_source),
      .groups = "drop"
    )

  # NMEMC --------------------------------------------------------------------
  # The processed marine master uses site_code as the site identifier. There
  # is no separate human-readable site-name field in the canonical master.
  nmemc_name <- find_col(nmemc, c("site", "site_name", "station_name", "site_code"))
  nmemc_id <- find_col(nmemc, c("site_code", "station_id", "site"))
  nmemc_lon <- find_coord_col_flexible(nmemc, "lon")
  nmemc_lat <- find_coord_col_flexible(nmemc, "lat")
  nmemc_sea <- find_col(nmemc, c("sea", "sea_area"))
  nmemc_province <- find_col(nmemc, c("province"))
  nmemc_city <- find_col(nmemc, c("city"))

  if (!is.na(nmemc_name)) {
    nmemc_station <- tibble(
      network = "NMEMC marine",
      station_id = if (!is.na(nmemc_id)) as.character(nmemc[[nmemc_id]]) else as.character(nmemc[[nmemc_name]]),
      station_name = as.character(nmemc[[nmemc_name]]),
      station_name_en = NA_character_,
      waterbody = if (!is.na(nmemc_sea)) as.character(nmemc[[nmemc_sea]]) else NA_character_,
      admin1 = if (!is.na(nmemc_province)) as.character(nmemc[[nmemc_province]]) else NA_character_,
      admin2 = if (!is.na(nmemc_city)) as.character(nmemc[[nmemc_city]]) else NA_character_,
      longitude = if (!is.na(nmemc_lon)) suppressWarnings(as.numeric(nmemc[[nmemc_lon]])) else NA_real_,
      latitude = if (!is.na(nmemc_lat)) suppressWarnings(as.numeric(nmemc[[nmemc_lat]])) else NA_real_,
      coordinate_source = if (!is.na(nmemc_lon) && !is.na(nmemc_lat)) "nmemc_water_master" else NA_character_
    ) %>%
      mutate(station_key = normalize_station_key(station_name)) %>%
      arrange(station_key, desc(valid_lonlat(longitude, latitude))) %>%
      distinct(network, station_key, .keep_all = TRUE)
  } else {
    nmemc_station <- tibble()
  }

  # ONLIMO -------------------------------------------------------------------
  catalog <- safe_read_csv(ONLIMO_CATALOG)
  if (!is.null(catalog) && nrow(catalog) > 0L) {
    on_name <- find_col(catalog, c("station_name", "station"))
    on_id <- find_col(catalog, c("station_id", "id_stasiun"))
    on_lon <- find_coord_col_flexible(catalog, "lon")
    on_lat <- find_coord_col_flexible(catalog, "lat")
    on_ws <- find_col(catalog, c("watershed", "river", "das"))
    on_prov <- find_col(catalog, c("province", "provinsi"))
    on_city <- find_col(catalog, c("kabupaten_kota", "city", "kota", "kabupaten"))

    historical_ids <- unique(as.character(onlimo_hist$station_id))
    onlimo_station <- tibble(
      network = "ONLIMO",
      station_id = if (!is.na(on_id)) as.character(catalog[[on_id]]) else NA_character_,
      station_name = if (!is.na(on_name)) as.character(catalog[[on_name]]) else NA_character_,
      station_name_en = NA_character_,
      waterbody = if (!is.na(on_ws)) as.character(catalog[[on_ws]]) else NA_character_,
      admin1 = if (!is.na(on_prov)) as.character(catalog[[on_prov]]) else NA_character_,
      admin2 = if (!is.na(on_city)) as.character(catalog[[on_city]]) else NA_character_,
      longitude = if (!is.na(on_lon)) suppressWarnings(as.numeric(catalog[[on_lon]])) else NA_real_,
      latitude = if (!is.na(on_lat)) suppressWarnings(as.numeric(catalog[[on_lat]])) else NA_real_,
      coordinate_source = "onlimo_station_catalog",
      in_historical_ip = if (!is.na(on_id)) as.character(catalog[[on_id]]) %in% historical_ids else FALSE
    ) %>%
      mutate(station_key = ifelse(
        !is.na(station_id) & nzchar(station_id),
        station_id,
        normalize_station_key(station_name)
      )) %>%
      distinct(network, station_key, .keep_all = TRUE)
  } else {
    onlimo_station <- tibble()
  }

  # CNEMC --------------------------------------------------------------------
  # CNEMC's processed nationwide endpoint often lacks coordinates. Preserve
  # the stations in metadata, but do not fabricate locations. If coordinate
  # fields appear in a future canonical archive they are used automatically.
  cn_name <- find_col(cnemc_latest, c("monitoring_section", "station_name", "section_name"))
  cn_lon <- find_coord_col_flexible(cnemc_latest, "lon")
  cn_lat <- find_coord_col_flexible(cnemc_latest, "lat")
  cn_area <- find_col(cnemc_latest, c("area", "province"))
  cn_river <- find_col(cnemc_latest, c("river_basin", "river"))

  if (!is.na(cn_name)) {
    cnemc_station <- tibble(
      network = "CNEMC surface water",
      station_id = NA_character_,
      station_name = as.character(cnemc_latest[[cn_name]]),
      station_name_en = NA_character_,
      waterbody = if (!is.na(cn_river)) as.character(cnemc_latest[[cn_river]]) else NA_character_,
      admin1 = if (!is.na(cn_area)) as.character(cnemc_latest[[cn_area]]) else NA_character_,
      admin2 = NA_character_,
      longitude = if (!is.na(cn_lon)) suppressWarnings(as.numeric(cnemc_latest[[cn_lon]])) else NA_real_,
      latitude = if (!is.na(cn_lat)) suppressWarnings(as.numeric(cnemc_latest[[cn_lat]])) else NA_real_,
      coordinate_source = if (!is.na(cn_lon) && !is.na(cn_lat)) "cnemc_latest" else NA_character_
    ) %>%
      mutate(station_key = paste(admin1, normalize_station_key(station_name), sep = "|")) %>%
      arrange(station_key, desc(valid_lonlat(longitude, latitude))) %>%
      distinct(network, station_key, .keep_all = TRUE)
  } else {
    cnemc_station <- tibble()
  }

  out <- bind_rows(fujian_station, nmemc_station, onlimo_station, cnemc_station) %>%
    arrange(network, station_name)

  if (!"in_historical_ip" %in% names(out)) out$in_historical_ip <- FALSE
  out$in_historical_ip[is.na(out$in_historical_ip)] <- FALSE
  out
}

# ----------------------------------------------------------------------------
# 11. Dataset inventory helpers
# ----------------------------------------------------------------------------
date_range <- function(df) {
  candidates <- c(
    "date", "report_period_end", "report_period_start", "observation_datetime",
    "monitoring_date", "sample_date", "minitor_month", "year"
  )
  nm <- find_col(df, candidates)
  if (is.na(nm)) return(c(NA_character_, NA_character_))

  x <- df[[nm]]
  if (nm == "year") {
    yr <- suppressWarnings(as.integer(x))
    yr <- yr[!is.na(yr)]
    if (length(yr) == 0L) return(c(NA_character_, NA_character_))
    return(c(as.character(min(yr)), as.character(max(yr))))
  }

  if (inherits(x, "POSIXt")) {
    x <- as.Date(x)
  } else if (!inherits(x, "Date")) {
    x_date <- parse_date_safe(x)
    if (all(is.na(x_date))) {
      x_posix <- parse_posix_safe(x)
      x <- as.Date(x_posix)
    } else {
      x <- x_date
    }
  }

  x <- x[!is.na(x)]
  if (length(x) == 0L) return(c(NA_character_, NA_character_))
  c(as.character(min(x)), as.character(max(x)))
}

make_inventory_row <- function(name, df, key_desc, source_rule, validation_status, output_path, notes = "") {
  rng <- date_range(df)
  tibble(
    dataset = name,
    rows = nrow(df),
    columns = ncol(df),
    distinct_primary_keys = NA_real_,
    primary_key = key_desc,
    date_min = rng[[1]],
    date_max = rng[[2]],
    canonical_source_rule = source_rule,
    validation_status = validation_status,
    output_file = normalizePath(output_path, winslash = "/", mustWork = TRUE),
    output_md5 = md5_if_file(output_path),
    built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S%z"),
    notes = notes
  )
}

# ----------------------------------------------------------------------------
# 12. Build all canonical datasets
# ----------------------------------------------------------------------------
fujian <- build_fujian()
nmemc_marine <- build_nmemc_marine()
cnemc <- build_cnemc()
onlimo_daily <- build_onlimo_daily()
onlimo_historical <- build_onlimo_historical()
station_metadata <- build_station_metadata(
  fujian,
  nmemc_marine,
  cnemc$latest,
  onlimo_daily,
  onlimo_historical
)

# ----------------------------------------------------------------------------
# 13. Save analysis datasets
# ----------------------------------------------------------------------------
paths <- list(
  fujian = file.path(ANALYSIS_DIR, "fujian_weekly_analysis.rds"),
  nmemc = file.path(ANALYSIS_DIR, "nmemc_marine_analysis.rds"),
  cnemc_latest = file.path(ANALYSIS_DIR, "cnemc_latest_analysis.rds"),
  cnemc_history = file.path(ANALYSIS_DIR, "cnemc_revision_history.rds"),
  onlimo_daily = file.path(ANALYSIS_DIR, "onlimo_daily_analysis.rds"),
  onlimo_hist = file.path(ANALYSIS_DIR, "onlimo_historical_analysis.rds"),
  station = file.path(ANALYSIS_DIR, "station_metadata.rds")
)

save_rds_atomic(fujian, paths$fujian)
save_rds_atomic(nmemc_marine, paths$nmemc)
save_rds_atomic(cnemc$latest, paths$cnemc_latest)
save_rds_atomic(cnemc$history, paths$cnemc_history)
save_rds_atomic(onlimo_daily, paths$onlimo_daily)
save_rds_atomic(onlimo_historical, paths$onlimo_hist)
save_rds_atomic(station_metadata, paths$station)

station_coordinate_summary <- station_metadata %>%
  mutate(
    coordinate_valid = valid_lonlat(
      suppressWarnings(as.numeric(longitude)),
      suppressWarnings(as.numeric(latitude))
    )
  ) %>%
  group_by(network) %>%
  summarise(
    stations_total = n(),
    stations_with_coordinates = sum(coordinate_valid, na.rm = TRUE),
    coordinate_completeness_pct = 100 * mean(coordinate_valid, na.rm = TRUE),
    coordinate_sources = paste(
      sort(unique(coordinate_source[coordinate_valid & !is.na(coordinate_source)])),
      collapse = "; "
    ),
    .groups = "drop"
  )

write_csv_atomic(
  station_coordinate_summary,
  file.path(ANALYSIS_DIR, "station_coordinate_summary.csv")
)

# ----------------------------------------------------------------------------
# 14. Dataset inventory
# ----------------------------------------------------------------------------
inventory <- bind_rows(
  make_inventory_row(
    "fujian_weekly_analysis", fujian,
    "observation_key",
    "GitHub persistent canonical year state; rebuilt with scraper-equivalent processing",
    "PASS_WITH_CURRENT_YEAR_TIMING; completed years exact; shared payload 100%",
    paths$fujian,
    "Current-year GitHub-only recent keys retained. Station crosswalk coordinates attached when available."
  ),
  make_inventory_row(
    "nmemc_marine_analysis", nmemc_marine,
    "source-native row",
    "Local processed master proven exactly reproducible from GitHub raw annual files",
    "PASS",
    paths$nmemc,
    "10/10 annual sources reconciled exactly at latest reconciliation."
  ),
  make_inventory_row(
    "cnemc_latest_analysis", cnemc$latest,
    "observation_key_hash",
    "Latest row version from reconciled PC + GitHub version union",
    "ROW_RECONCILED_WITH_SOURCE_REVISION_CONTEXT",
    paths$cnemc_latest,
    "Use for ordinary CNEMC environmental analyses; revision history is preserved separately."
  ),
  make_inventory_row(
    "cnemc_revision_history", cnemc$history,
    "row_hash",
    "Union of PC cumulative versions, GitHub full checkpoints, and GitHub targeted deltas",
    "ROW_RECONCILED_WITH_SOURCE_REVISION_CONTEXT",
    paths$cnemc_history,
    "Use for provenance/revision studies, not as independent repeated observations without version-aware modeling."
  ),
  make_inventory_row(
    "onlimo_daily_analysis", onlimo_daily,
    "station_id + date",
    "Local cumulative archive independently reproduced by retained GitHub daily snapshots",
    "PASS_CURRENT_OVERLAP",
    paths$onlimo_daily,
    "Scientific payload agreement is exact for reconciled GitHub keys."
  ),
  make_inventory_row(
    "onlimo_historical_analysis", onlimo_historical,
    "station_id + date",
    "Reconstructed from all GitHub immutable historical observation partitions",
    "PASS_PARTIAL_CATCHUP",
    paths$onlimo_hist,
    "Coverage fields from reconciliation are attached. Restrict descriptive trends to coverage-comparable stations/periods until catch-up completes."
  ),
  make_inventory_row(
    "station_metadata", station_metadata,
    "network + station_key",
    "Unified metadata derived from source masters and station catalog/crosswalk",
    "DERIVED_METADATA",
    paths$station,
    "Fujian recovered coordinates are retained when present in fujian_station_crosswalk.csv."
  )
)

# Fill primary-key counts explicitly -----------------------------------------
inventory$distinct_primary_keys[inventory$dataset == "fujian_weekly_analysis"] <-
  n_distinct(fujian$observation_key)
inventory$distinct_primary_keys[inventory$dataset == "nmemc_marine_analysis"] <-
  nrow(nmemc_marine)
inventory$distinct_primary_keys[inventory$dataset == "cnemc_latest_analysis"] <-
  n_distinct(cnemc$latest$observation_key_hash)
inventory$distinct_primary_keys[inventory$dataset == "cnemc_revision_history"] <-
  n_distinct(cnemc$history$row_hash)
inventory$distinct_primary_keys[inventory$dataset == "onlimo_daily_analysis"] <-
  nrow(onlimo_daily)
inventory$distinct_primary_keys[inventory$dataset == "onlimo_historical_analysis"] <-
  nrow(onlimo_historical)
inventory$distinct_primary_keys[inventory$dataset == "station_metadata"] <-
  nrow(station_metadata)

inventory_path <- file.path(ANALYSIS_DIR, "dataset_inventory.csv")
write_csv_atomic(inventory, inventory_path)

# ----------------------------------------------------------------------------
# 15. Build manifest / provenance
# ----------------------------------------------------------------------------
if (isTRUE(WRITE_BUILD_MANIFEST)) {
  source_manifest <- tibble(
    role = c(
      "fujian_github_state",
      "nmemc_marine_local_master",
      "cnemc_pc_master",
      "cnemc_github_root",
      "onlimo_daily_local_archive",
      "onlimo_historical_github_root",
      "onlimo_historical_coverage",
      "onlimo_station_catalog"
    ),
    path = c(
      FUJIAN_GITHUB_STATE,
      NMEMC_MARINE_MASTER,
      CNEMC_PC_MASTER,
      CNEMC_GITHUB_ROOT,
      ONLIMO_DAILY_ARCHIVE,
      ONLIMO_HIST_GITHUB_ROOT,
      ONLIMO_HIST_COVERAGE,
      ONLIMO_CATALOG
    )
  ) %>%
    mutate(
      exists = file.exists(path) | dir.exists(path),
      file_md5 = vapply(path, md5_if_file, character(1)),
      built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")
    )

  build_manifest_path <- file.path(ANALYSIS_DIR, "analysis_build_manifest.csv")
  write_csv_atomic(source_manifest, build_manifest_path)
}

# ----------------------------------------------------------------------------
# 16. Console summary
# ----------------------------------------------------------------------------
msg("Canonical analysis layer complete.")
message("")
message("Output directory: ", ANALYSIS_DIR)
message("")
message("Rows written:")
message("  Fujian weekly:             ", format(nrow(fujian), big.mark = ","))
message("  NMEMC marine:              ", format(nrow(nmemc_marine), big.mark = ","))
message("  CNEMC latest observations: ", format(nrow(cnemc$latest), big.mark = ","))
message("  CNEMC row versions:        ", format(nrow(cnemc$history), big.mark = ","))
message("  ONLIMO daily:              ", format(nrow(onlimo_daily), big.mark = ","))
message("  ONLIMO historical:         ", format(nrow(onlimo_historical), big.mark = ","))
message("  Unified station metadata:  ", format(nrow(station_metadata), big.mark = ","))
message("")
message("Station coordinates:")
for (i in seq_len(nrow(station_coordinate_summary))) {
  z <- station_coordinate_summary[i, ]
  message(
    "  ", z$network, ": ",
    format(z$stations_with_coordinates, big.mark = ","), " / ",
    format(z$stations_total, big.mark = ","), " (",
    sprintf("%.1f", z$coordinate_completeness_pct), "%)",
    if (nzchar(z$coordinate_sources)) paste0(" | ", z$coordinate_sources) else ""
  )
}
message("")
message("Important interpretation rules:")
message("  - CNEMC latest_analysis = one latest version per observation key.")
message("  - CNEMC revision_history preserves all reconciled row versions.")
message("  - ONLIMO historical coverage is still partial until GitHub catch-up completes.")
message("  - Generated analysis files are derived products; keep source archives immutable.")

invisible(list(
  fujian = fujian,
  nmemc_marine = nmemc_marine,
  cnemc_latest = cnemc$latest,
  cnemc_revision_history = cnemc$history,
  onlimo_daily = onlimo_daily,
  onlimo_historical = onlimo_historical,
  station_metadata = station_metadata,
  inventory = inventory
))