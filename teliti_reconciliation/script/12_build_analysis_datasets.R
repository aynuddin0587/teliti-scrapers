# ============================================================================
# 12_build_analysis_datasets.R
#
# Build a stable, non-destructive canonical analysis layer from reconciled
# Teliti collection archives.
#
# CNEMC additions in this version
# -------------------------------
# 1. Preserve archived/raw CNEMC strings and hashes unchanged.
# 2. Decode common UTF-8 mojibake into readable Chinese analysis fields.
# 3. Extract highest-precision numeric values from CNEMC HTML parameter cells.
# 4. Attach audited external CNEMC station coordinates when a crosswalk exists.
# 5. Keep CNEMC revision_history raw/lean; enrich only latest_analysis.
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
# Derived RDS files are large and regenerated frequently. gzip is much faster
# than xz while remaining compressed and portable.
RDS_COMPRESSION <- "gzip"

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
CNEMC_COORD_CROSSWALK <- file.path(
  PROJECT_DIR, "nmemc", "data", "surfacewater", "processed",
  "cnemc_station_crosswalk.csv"
)
CNEMC_COORD_COVERAGE <- file.path(
  PROJECT_DIR, "nmemc", "data", "surfacewater", "processed",
  "cnemc_station_coordinate_coverage.csv"
)
CNEMC_STATION_CATALOGUE <- file.path(
  PROJECT_DIR, "nmemc", "data", "surfacewater", "processed",
  "cnemc_station_catalogue.csv"
)
FUJIAN_COORD_DIAGNOSTICS <- file.path(
  ANALYSIS_DIR, "fujian_station_coordinate_match_diagnostics.csv"
)

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

save_rds_atomic <- function(x, path, compress = RDS_COMPRESSION) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "analysis_", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, compress = compress, version = 3)
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

find_recursive <- function(root, pattern) {
  if (!dir.exists(root)) return(character())
  list.files(
    root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )
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
    if (any(idx)) out[idx] <- as.Date(ch[idx], format = sp[[2]])
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
# 4. CNEMC analytical decoding helpers
# ----------------------------------------------------------------------------
repair_cnemc_mojibake <- function(x) {
  special_cp1252 <- c(
    `8364` = 128L, `8218` = 130L, `402` = 131L, `8222` = 132L,
    `8230` = 133L, `8224` = 134L, `8225` = 135L, `710` = 136L,
    `8240` = 137L, `352` = 138L, `8249` = 139L, `338` = 140L,
    `381` = 142L, `8216` = 145L, `8217` = 146L, `8220` = 147L,
    `8221` = 148L, `8226` = 149L, `8211` = 150L, `8212` = 151L,
    `732` = 152L, `8482` = 153L, `353` = 154L, `8250` = 155L,
    `339` = 156L, `382` = 158L, `376` = 159L
  )

  repair_one <- function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    cp <- utf8ToInt(enc2utf8(s))
    if (length(cp) == 0L) return(s)

    bytes <- integer(length(cp))
    for (i in seq_along(cp)) {
      code <- cp[[i]]
      if (code >= 0L && code <= 255L) {
        bytes[[i]] <- code
      } else {
        key <- as.character(code)
        if (!key %in% names(special_cp1252)) return(s)
        bytes[[i]] <- unname(special_cp1252[[key]])
      }
    }

    candidate <- rawToChar(as.raw(bytes))
    Encoding(candidate) <- "UTF-8"
    valid <- suppressWarnings(iconv(candidate, from = "UTF-8", to = "UTF-8"))
    if (is.na(valid)) s else valid
  }

  # CNEMC repeats the same province, basin, station and formatted measurement
  # strings many thousands of times. Repair each UNIQUE string once and map it
  # back instead of running the byte-level loop for every observation row.
  values <- as.character(x)
  unique_values <- unique(values)
  repaired_unique <- vapply(
    unique_values,
    repair_one,
    character(1),
    USE.NAMES = FALSE
  )
  unname(repaired_unique[match(values, unique_values)])
}

cnemc_html_to_text <- function(x) {
  x <- repair_cnemc_mojibake(as.character(x))
  x <- gsub("(?i)<br\\s*/?>", " ", x, perl = TRUE)
  x <- gsub("<[^>]+>", "", x, perl = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&mu;", "µ", x, fixed = TRUE)
  x <- gsub("&sup3;", "³", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

cnemc_parameter_text <- function(x) {
  x <- repair_cnemc_mojibake(as.character(x))
  out <- cnemc_html_to_text(x)

  has_title <- !is.na(x) & grepl("title\\s*=\\s*['\\\"]", x, perl = TRUE)
  if (any(has_title)) {
    title_text <- sub(
      ".*title\\s*=\\s*['\\\"]([^'\\\"]*)['\\\"].*",
      "\\1",
      x[has_title],
      perl = TRUE
    )
    title_text <- cnemc_html_to_text(title_text)
    title_text <- sub(
      "^.*?(?:原始值|raw\\s*value)\\s*[:：]\\s*",
      "",
      title_text,
      perl = TRUE,
      ignore.case = TRUE
    )
    out[has_title] <- trimws(title_text)
  }

  out[out %in% c("", "--", "—", "-", "NA", "N/A", "null", "NULL")] <- NA_character_
  out
}

cnemc_parameter_qualifier <- function(x) {
  z <- cnemc_parameter_text(x)
  out <- rep(NA_character_, length(z))
  out[!is.na(z) & grepl("^\\s*<=", z)] <- "<="
  out[!is.na(z) & grepl("^\\s*>=", z)] <- ">="
  out[!is.na(z) & grepl("^\\s*<", z) & is.na(out)] <- "<"
  out[!is.na(z) & grepl("^\\s*>", z) & is.na(out)] <- ">"
  out[!is.na(z) & grepl("^\\s*≤", z)] <- "<="
  out[!is.na(z) & grepl("^\\s*≥", z)] <- ">="
  out
}

cnemc_parameter_numeric <- function(x) {
  z <- cnemc_parameter_text(x)
  suppressWarnings(readr::parse_number(z, na = c("", "NA", "N/A")))
}

cnemc_parse_parameter <- function(x) {
  z <- cnemc_parameter_text(x)
  qualifier <- rep(NA_character_, length(z))
  qualifier[!is.na(z) & grepl("^\\s*<=", z)] <- "<="
  qualifier[!is.na(z) & grepl("^\\s*>=", z)] <- ">="
  qualifier[!is.na(z) & grepl("^\\s*<", z) & is.na(qualifier)] <- "<"
  qualifier[!is.na(z) & grepl("^\\s*>", z) & is.na(qualifier)] <- ">"
  qualifier[!is.na(z) & grepl("^\\s*≤", z)] <- "<="
  qualifier[!is.na(z) & grepl("^\\s*≥", z)] <- ">="

  list(
    numeric = suppressWarnings(readr::parse_number(z, na = c("", "NA", "N/A"))),
    qualifier = qualifier
  )
}

cnemc_decode_water_class <- function(code) {
  key <- c(`1` = "Ⅰ", `2` = "Ⅱ", `3` = "Ⅲ", `4` = "Ⅳ", `5` = "Ⅴ", `6` = "劣Ⅴ")
  z <- trimws(as.character(code))
  out <- unname(key[z])
  out[!z %in% names(key)] <- NA_character_
  out
}

clean_cnemc_analysis_fields <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)

  text_map <- c(
    area = "area_cn",
    river_basin = "river_basin_cn",
    monitoring_section = "monitoring_section_cn"
  )

  for (src in names(text_map)) {
    if (src %in% names(df)) {
      df[[text_map[[src]]]] <- repair_cnemc_mojibake(df[[src]])
    }
  }

  if ("water_quality_class" %in% names(df)) {
    df$water_quality_class_published <- repair_cnemc_mojibake(df$water_quality_class)
  }

  if ("water_quality_class_code" %in% names(df)) {
    rank <- suppressWarnings(as.integer(as.character(df$water_quality_class_code)))
    rank[!rank %in% 1:6] <- NA_integer_
    df$water_quality_class_rank <- rank
    df$water_quality_class_clean <- cnemc_decode_water_class(df$water_quality_class_code)

    if ("water_quality_class_published" %in% names(df)) {
      fill <- is.na(df$water_quality_class_clean) &
        !is.na(df$water_quality_class_published) &
        nzchar(df$water_quality_class_published)
      df$water_quality_class_clean[fill] <- df$water_quality_class_published[fill]
    }
  } else if ("water_quality_class_published" %in% names(df)) {
    df$water_quality_class_clean <- df$water_quality_class_published
  }

  parameter_map <- c(
    water_temperature_c_raw = "water_temperature_c",
    ph_raw = "ph",
    dissolved_oxygen_mg_l_raw = "dissolved_oxygen_mg_l",
    conductivity_raw = "conductivity_us_cm",
    turbidity_ntu_raw = "turbidity_ntu",
    permanganate_index_mg_l_raw = "permanganate_index_mg_l",
    ammonia_nitrogen_mg_l_raw = "ammonia_nitrogen_mg_l",
    total_phosphorus_mg_l_raw = "total_phosphorus_mg_l",
    total_nitrogen_mg_l_raw = "total_nitrogen_mg_l",
    toc_mg_l_raw = "toc_mg_l",
    chlorophyll_a_raw = "chlorophyll_a",
    algal_density_raw = "algal_density_cells_l"
  )

  for (src in names(parameter_map)) {
    if (!src %in% names(df)) next
    dest <- parameter_map[[src]]
    parsed <- cnemc_parse_parameter(df[[src]])
    df[[dest]] <- parsed$numeric
    df[[paste0(dest, "_qualifier")]] <- parsed$qualifier
  }

  if (all(c("area_cn", "monitoring_section_cn") %in% names(df))) {
    df$analysis_station_key <- paste(
      normalize_station_key(df$area_cn),
      normalize_station_key(df$monitoring_section_cn),
      sep = "|"
    )
  }

  df
}

attach_cnemc_station_coordinates <- function(df, crosswalk_path) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if (!"analysis_station_key" %in% names(df)) {
    df <- clean_cnemc_analysis_fields(df)
  }

  if (!file.exists(crosswalk_path)) {
    msg("CNEMC coordinate crosswalk not found; continuing without external coordinates.")
    return(df)
  }

  cw <- safe_read_csv(crosswalk_path)
  if (is.null(cw) || nrow(cw) == 0L) return(df)

  required <- c("station_key", "longitude", "latitude")
  missing <- setdiff(required, names(cw))
  if (length(missing) > 0L) {
    stop(
      "CNEMC coordinate crosswalk is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  cw <- cw %>%
    mutate(
      station_key = as.character(station_key),
      longitude = suppressWarnings(as.numeric(longitude)),
      latitude = suppressWarnings(as.numeric(latitude))
    ) %>%
    filter(
      !is.na(station_key), nzchar(station_key),
      valid_lonlat(longitude, latitude)
    )

  if (anyDuplicated(cw$station_key)) {
    stop(
      "CNEMC coordinate crosswalk contains duplicated station_key values. ",
      "Resolve conflicts before attaching coordinates.",
      call. = FALSE
    )
  }

  # Base match is substantially lighter than joining a 1,600-row lookup onto
  # nearly one million observation rows, and preserves row order exactly.
  idx <- match(as.character(df$analysis_station_key), cw$station_key)
  cross_lon <- cw$longitude[idx]
  cross_lat <- cw$latitude[idx]
  cross_city <- if ("city_cn" %in% names(cw)) as.character(cw$city_cn[idx]) else rep(NA_character_, nrow(df))
  cross_source <- if ("coordinate_source" %in% names(cw)) as.character(cw$coordinate_source[idx]) else rep(NA_character_, nrow(df))
  cross_url <- if ("source_url" %in% names(cw)) as.character(cw$source_url[idx]) else rep(NA_character_, nrow(df))

  existing_lon <- if ("station_longitude" %in% names(df)) {
    suppressWarnings(as.numeric(df$station_longitude))
  } else rep(NA_real_, nrow(df))
  existing_lat <- if ("station_latitude" %in% names(df)) {
    suppressWarnings(as.numeric(df$station_latitude))
  } else rep(NA_real_, nrow(df))

  use_crosswalk <- valid_lonlat(cross_lon, cross_lat) &
    !valid_lonlat(existing_lon, existing_lat)

  df$station_longitude <- existing_lon
  df$station_latitude <- existing_lat
  df$station_longitude[use_crosswalk] <- cross_lon[use_crosswalk]
  df$station_latitude[use_crosswalk] <- cross_lat[use_crosswalk]

  if (!"station_city" %in% names(df)) df$station_city <- NA_character_
  city_fill <- use_crosswalk & !is.na(cross_city) & nzchar(cross_city)
  df$station_city[city_fill] <- cross_city[city_fill]

  if (!"station_coordinate_source" %in% names(df)) {
    df$station_coordinate_source <- NA_character_
  }
  df$station_coordinate_source[use_crosswalk] <- ifelse(
    is.na(cross_source[use_crosswalk]) | !nzchar(cross_source[use_crosswalk]),
    "cnemc_station_crosswalk",
    cross_source[use_crosswalk]
  )

  if (!"station_coordinate_source_url" %in% names(df)) {
    df$station_coordinate_source_url <- NA_character_
  }
  df$station_coordinate_source_url[use_crosswalk] <- cross_url[use_crosswalk]

  df
}

# ----------------------------------------------------------------------------
# 5. Reconciliation gate
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
# 6. Fujian weekly
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
  if (any(is_dmy)) out[is_dmy] <- as.Date(z[is_dmy], format = "%d/%m/%Y")

  is_iso <- !is.na(z) & grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", z)
  if (any(is_iso)) out[is_iso] <- as.Date(z[is_iso], format = "%Y-%m-%d")

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

normalize_fujian_match_key <- function(x) {
  z <- normalize_station_key(x)
  if (length(z) == 0L) return(z)
  # Remove presentation punctuation only; do not drop administrative or river
  # words, because those may distinguish genuinely different stations.
  z <- gsub("[[:space:][:punct:]，。；：、“”‘’（）【】《》·]+", "", z, perl = TRUE)
  z
}

canonicalize_fujian_station_identity <- function(x) {
  # Fujian weekly reports sometimes append temporary operational states to the
  # station label. These are not new physical stations. Preserve the original
  # station_name/station_name_raw columns, but remove these suffixes for the
  # station identity key used by the crosswalk and station metadata.
  z <- normalize_fujian_station_name(x)
  if (length(z) == 0L) return(z)

  z <- gsub("（", "(", z, fixed = TRUE)
  z <- gsub("）", ")", z, fixed = TRUE)
  z <- sub("\\((更新改造|试运行中|试运行)\\)$", "", z, perl = TRUE)

  # One observed label embeds a descriptive water-source qualifier while the
  # crosswalk uses the underlying station name. This is an explicit known alias,
  # not a general rule that removes arbitrary parenthetical text.
  z <- sub("\\(泉州晋江干流水源地\\)$", "", z, perl = TRUE)

  normalize_fujian_match_key(z)
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
  root <- file.path(PROJECT_DIR, "fujian_surfacewater")
  candidates <- unique(c(
    file.path(FUJIAN_LOCAL_PROCESSED, "fujian_station_crosswalk.csv"),
    file.path(root, "data", "fujian_station_crosswalk.csv"),
    file.path(root, "fujian_station_crosswalk.csv"),
    find_recursive(root, "^fujian_station_crosswalk\\.csv$")
  ))

  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0L) {
    msg("No Fujian station crosswalk found.")
    return(NA_character_)
  }

  scored <- lapply(candidates, function(path) {
    cw <- safe_read_csv(path)
    if (is.null(cw) || nrow(cw) == 0L) {
      return(tibble(path = path, rows = 0L, valid_coordinates = 0L))
    }

    lon_col <- find_coord_col_flexible(cw, "lon")
    lat_col <- find_coord_col_flexible(cw, "lat")
    if (is.na(lon_col) || is.na(lat_col)) {
      return(tibble(path = path, rows = nrow(cw), valid_coordinates = 0L))
    }

    lon <- suppressWarnings(readr::parse_number(as.character(cw[[lon_col]])))
    lat <- suppressWarnings(readr::parse_number(as.character(cw[[lat_col]])))

    tibble(
      path = path,
      rows = nrow(cw),
      valid_coordinates = sum(valid_lonlat(lon, lat), na.rm = TRUE)
    )
  }) %>%
    bind_rows() %>%
    arrange(desc(valid_coordinates), desc(rows))

  best <- scored %>% slice(1)
  msg(
    "Fujian crosswalk selected: ", best$path,
    " | rows=", best$rows,
    " | valid coordinates=", best$valid_coordinates
  )

  best$path[[1]]
}

build_fujian_crosswalk_lookup <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  cw <- safe_read_csv(path)
  if (is.null(cw) || nrow(cw) == 0L) return(NULL)

  lon_col <- find_coord_col_flexible(cw, "lon")
  lat_col <- find_coord_col_flexible(cw, "lat")
  city_col <- find_col(cw, c("city", "city_en", "prefecture", "municipality", "admin_city", "city_name"))
  en_col <- find_col(cw, c("station_name_en", "english_name", "name_en"))
  id_col <- find_col(cw, c("MN", "mn", "station_id", "site_code", "station_code"))

  explicit_aliases <- c(
    "station_name", "station_name_cn", "station_name_zh", "station_name_raw",
    "station", "site_name", "section_name", "monitoring_section",
    "station_name_observed", "station_names_observed", "names_observed",
    "站点名称", "断面名称", "监测断面", "监测断面名称"
  )
  alias_cols <- unique(na.omit(vapply(
    explicit_aliases,
    function(z) find_col(cw, z),
    character(1)
  )))

  # Also accept clearly named station/section alias columns that may have been
  # added during manual crosswalk curation.
  generic_alias_idx <- grep(
    "((station|site|section).*(name|alias|observed))|((name|alias).*(station|site|section))|站点.*名|断面.*名",
    names(cw),
    ignore.case = TRUE,
    perl = TRUE
  )
  alias_cols <- unique(c(alias_cols, names(cw)[generic_alias_idx]))

  if (length(alias_cols) == 0L) return(NULL)

  lon <- if (!is.na(lon_col)) suppressWarnings(readr::parse_number(as.character(cw[[lon_col]]))) else rep(NA_real_, nrow(cw))
  lat <- if (!is.na(lat_col)) suppressWarnings(readr::parse_number(as.character(cw[[lat_col]]))) else rep(NA_real_, nrow(cw))

  base_meta <- tibble(
    cw_row_id = seq_len(nrow(cw)),
    station_name_crosswalk = coalesce_candidate_cols(
      cw,
      c("station_name", "station_name_raw", "station_name_zh", "station_name_cn", "站点名称", "断面名称", "site_name")
    ),
    station_name_en = if (!is.na(en_col)) as.character(cw[[en_col]]) else NA_character_,
    station_external_id = if (!is.na(id_col)) as.character(cw[[id_col]]) else NA_character_,
    station_city = if (!is.na(city_col)) as.character(cw[[city_col]]) else NA_character_,
    station_longitude = lon,
    station_latitude = lat,
    station_metadata_source = basename(path)
  )

  pieces <- lapply(alias_cols, function(alias_col) {
    tibble(
      cw_row_id = seq_len(nrow(cw)),
      station_name_key = canonicalize_fujian_station_identity(cw[[alias_col]]),
      station_alias_value = as.character(cw[[alias_col]]),
      station_alias_source = alias_col
    ) %>%
      left_join(base_meta, by = "cw_row_id") %>%
      mutate(
        station_name_crosswalk = dplyr::coalesce(
          station_name_crosswalk,
          station_alias_value
        )
      )
  })

  bind_rows(pieces) %>%
    filter(!is.na(station_name_key), nzchar(station_name_key)) %>%
    mutate(has_coordinate = valid_lonlat(station_longitude, station_latitude)) %>%
    arrange(
      station_name_key,
      desc(has_coordinate),
      cw_row_id,
      station_alias_source
    ) %>%
    distinct(station_name_key, .keep_all = TRUE) %>%
    select(-has_coordinate)
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
    } else list()

    parts[[i]] <- standardize_fujian_year(raw, yr, meta, basename(year_files[[i]]))
  }

  dat <- bind_rows(parts) %>%
    arrange(year, week, river_system, station_name) %>%
    mutate(
      analysis_source_basis = source_basis,
      station_name_key = canonicalize_fujian_station_identity(station_name),
      station_name_raw_key = normalize_fujian_match_key(station_name_raw)
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
      left_join(lookup, by = "station_name_key") %>%
      mutate(
        station_name_canonical = dplyr::coalesce(
          as.character(station_name_crosswalk),
          as.character(station_name)
        )
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
        station_metadata_source = NA_character_,
        station_alias_value = NA_character_,
        station_alias_source = NA_character_,
        cw_row_id = NA_integer_,
        station_name_canonical = as.character(station_name)
      )
  }

  # Station-level matching diagnostics. This is intentionally descriptive:
  # fuzzy/nearest candidates are not auto-assigned.
  diag <- dat %>%
    filter(!is.na(station_name_key), nzchar(station_name_key)) %>%
    group_by(station_name_key) %>%
    summarise(
      station_name = first_non_missing_chr(station_name_canonical),
      station_name_raw = first_non_missing_chr(station_name_raw),
      observed_station_names = paste(
        sort(unique(stats::na.omit(as.character(station_name)))),
        collapse = ";"
      ),
      station_name_crosswalk = first_non_missing_chr(station_name_crosswalk),
      station_alias_value = first_non_missing_chr(station_alias_value),
      station_alias_source = first_non_missing_chr(station_alias_source),
      station_external_id = first_non_missing_chr(station_external_id),
      station_longitude = first_non_missing_num(station_longitude),
      station_latitude = first_non_missing_num(station_latitude),
      station_metadata_source = first_non_missing_chr(station_metadata_source),
      .groups = "drop"
    ) %>%
    mutate(
      crosswalk_name_matched = !is.na(station_name_crosswalk),
      has_coordinates = valid_lonlat(station_longitude, station_latitude),
      match_status = case_when(
        has_coordinates ~ "matched_with_coordinates",
        crosswalk_name_matched ~ "matched_without_coordinates",
        TRUE ~ "no_crosswalk_name_match"
      )
    ) %>%
    arrange(match_status, station_name)

  write_csv_atomic(diag, FUJIAN_COORD_DIAGNOSTICS)
  msg(
    "Fujian station crosswalk matches: ",
    sum(diag$crosswalk_name_matched, na.rm = TRUE), " / ", nrow(diag),
    " | with coordinates: ", sum(diag$has_coordinates, na.rm = TRUE)
  )

  dat
}

# ----------------------------------------------------------------------------
# 7. NMEMC marine
# ----------------------------------------------------------------------------
build_nmemc_marine <- function() {
  msg("Building NMEMC marine canonical analysis dataset ...")
  if (!file.exists(NMEMC_MARINE_MASTER)) {
    stop("NMEMC marine master is missing: ", NMEMC_MARINE_MASTER, call. = FALSE)
  }
  dat <- readRDS(NMEMC_MARINE_MASTER)
  dat$analysis_source_basis <- "local_processed_master_reconciled"
  dat
}

# ----------------------------------------------------------------------------
# 8. CNEMC: PC + GitHub version union
# ----------------------------------------------------------------------------
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

cast_like_reference <- function(x, reference) {
  if (is.character(reference) || is.factor(reference)) return(as.character(x))
  if (inherits(reference, "POSIXt")) return(parse_posix_safe(x))
  if (inherits(reference, "Date")) return(parse_date_safe(x))
  if (is.integer(reference)) return(suppressWarnings(as.integer(x)))
  if (is.double(reference) || is.numeric(reference)) return(suppressWarnings(as.numeric(x)))

  if (is.logical(reference)) {
    ch <- tolower(trim_na(x))
    out <- rep(NA, length(ch))
    out[ch %in% c("true", "t", "1", "yes", "y")] <- TRUE
    out[ch %in% c("false", "f", "0", "no", "n")] <- FALSE
    return(out)
  }

  as.character(x)
}

align_cnemc_schema <- function(df, reference) {
  if (is.null(df) || nrow(df) == 0L) return(df)

  common <- intersect(names(df), names(reference))
  for (nm in common) {
    df[[nm]] <- cast_like_reference(df[[nm]], reference[[nm]])
  }

  for (nm in intersect(
    c("analysis_archive_source", "analysis_archive_file"),
    names(df)
  )) {
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

  source_map <- all_versions %>%
    group_by(row_hash) %>%
    summarise(
      analysis_version_sources = paste(
        sort(unique(analysis_archive_source)), collapse = ";"
      ),
      analysis_version_source_files_n = n_distinct(analysis_archive_file),
      .groups = "drop"
    )

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
    arrange(
      observation_key_hash,
      desc(analysis_version_seen_at),
      desc(row_hash)
    ) %>%
    distinct(observation_key_hash, .keep_all = TRUE) %>%
    mutate(
      analysis_source_basis = "reconciled_pc_plus_github_version_union",
      analysis_version_rule = "latest_seen_row_version_per_observation_key_hash"
    )

  # --------------------------------------------------------------------------
  # ANALYSIS ENRICHMENT — deliberately after version selection.
  # Raw source strings, row_hash and observation_key_hash stay unchanged.
  # --------------------------------------------------------------------------
  enrichment_started <- Sys.time()
  latest <- clean_cnemc_analysis_fields(latest)
  latest <- attach_cnemc_station_coordinates(
    latest,
    CNEMC_COORD_CROSSWALK
  )
  msg(
    "CNEMC analytical enrichment time: ",
    sprintf(
      "%.2f",
      as.numeric(difftime(Sys.time(), enrichment_started, units = "secs"))
    ),
    " seconds"
  )

  assert_unique(history, "row_hash", "CNEMC revision history")
  assert_unique(latest, "observation_key_hash", "CNEMC latest analysis view")

  list(history = history, latest = latest)
}

# ----------------------------------------------------------------------------
# 9. ONLIMO daily
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
# 10. ONLIMO historical
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
# 11. Unified station metadata
# ----------------------------------------------------------------------------
build_cnemc_station_metadata_fast <- function(cnemc_latest) {
  current_station_n <- if ("analysis_station_key" %in% names(cnemc_latest)) {
    n_distinct(cnemc_latest$analysis_station_key[!is.na(cnemc_latest$analysis_station_key)])
  } else {
    NA_integer_
  }

  # Preferred source when it is demonstrably current: script 15 already reduced
  # CNEMC to one row per canonical station and attached audited coordinates.
  if (file.exists(CNEMC_COORD_COVERAGE)) {
    x <- safe_read_csv(CNEMC_COORD_COVERAGE)
    required <- c("station_key", "area_cn", "monitoring_section_cn")
    coverage_is_current <- !is.null(x) && nrow(x) > 0L &&
      all(required %in% names(x)) &&
      (is.na(current_station_n) || nrow(x) == current_station_n)

    if (coverage_is_current) {
      lon <- if ("longitude" %in% names(x)) suppressWarnings(as.numeric(x$longitude)) else rep(NA_real_, nrow(x))
      lat <- if ("latitude" %in% names(x)) suppressWarnings(as.numeric(x$latitude)) else rep(NA_real_, nrow(x))
      msg("CNEMC station metadata source: coordinate coverage cache (", nrow(x), " stations)")
      return(tibble(
        network = "CNEMC surface water",
        station_id = NA_character_,
        station_name = as.character(x$monitoring_section_cn),
        station_name_en = NA_character_,
        waterbody = if ("river_basin_cn" %in% names(x)) as.character(x$river_basin_cn) else NA_character_,
        admin1 = as.character(x$area_cn),
        admin2 = if ("city_cn" %in% names(x)) as.character(x$city_cn) else NA_character_,
        longitude = lon,
        latitude = lat,
        coordinate_source = if ("coordinate_source" %in% names(x)) as.character(x$coordinate_source) else ifelse(valid_lonlat(lon, lat), "cnemc_station_crosswalk", NA_character_),
        station_key = as.character(x$station_key)
      ) %>%
        arrange(station_key) %>%
        distinct(network, station_key, .keep_all = TRUE))
    }

    if (!is.null(x) && nrow(x) > 0L && !is.na(current_station_n) && nrow(x) != current_station_n) {
      msg(
        "CNEMC coordinate coverage cache is stale (", nrow(x),
        " vs current ", current_station_n, " stations); rebuilding station metadata from current latest view."
      )
    }
  }

  # Fresh compact extraction from the current latest view. This scans only the
  # handful of station-level columns and collapses immediately to distinct keys;
  # it does not re-clean parameters or reconstruct the full observation table.
  cn_name <- find_col(cnemc_latest, c("monitoring_section_cn", "monitoring_section", "station_name", "section_name"))
  cn_area <- find_col(cnemc_latest, c("area_cn", "area", "province"))
  cn_river <- find_col(cnemc_latest, c("river_basin_cn", "river_basin", "river"))
  cn_city <- find_col(cnemc_latest, c("station_city", "city"))
  cn_lon <- find_coord_col_flexible(cnemc_latest, "lon")
  cn_lat <- find_coord_col_flexible(cnemc_latest, "lat")
  cn_coordinate_source <- find_col(cnemc_latest, c("station_coordinate_source", "coordinate_source"))
  cn_key <- find_col(cnemc_latest, c("analysis_station_key", "station_key"))

  if (!is.na(cn_name) && !is.na(cn_area)) {
    station_key <- if (!is.na(cn_key)) {
      as.character(cnemc_latest[[cn_key]])
    } else {
      paste(
        normalize_station_key(cnemc_latest[[cn_area]]),
        normalize_station_key(cnemc_latest[[cn_name]]),
        sep = "|"
      )
    }

    out <- tibble(
      network = "CNEMC surface water",
      station_id = NA_character_,
      station_name = as.character(cnemc_latest[[cn_name]]),
      station_name_en = NA_character_,
      waterbody = if (!is.na(cn_river)) as.character(cnemc_latest[[cn_river]]) else NA_character_,
      admin1 = as.character(cnemc_latest[[cn_area]]),
      admin2 = if (!is.na(cn_city)) as.character(cnemc_latest[[cn_city]]) else NA_character_,
      longitude = if (!is.na(cn_lon)) suppressWarnings(as.numeric(cnemc_latest[[cn_lon]])) else NA_real_,
      latitude = if (!is.na(cn_lat)) suppressWarnings(as.numeric(cnemc_latest[[cn_lat]])) else NA_real_,
      coordinate_source = if (!is.na(cn_coordinate_source)) as.character(cnemc_latest[[cn_coordinate_source]]) else NA_character_,
      station_key = station_key
    ) %>%
      filter(!is.na(station_key), nzchar(station_key)) %>%
      arrange(station_key, desc(valid_lonlat(longitude, latitude))) %>%
      distinct(network, station_key, .keep_all = TRUE)

    msg("CNEMC station metadata source: current latest view compact extraction (", nrow(out), " stations)")
    return(out)
  }

  # Last fallback: cached catalogue + coordinate crosswalk.
  if (file.exists(CNEMC_STATION_CATALOGUE)) {
    x <- safe_read_csv(CNEMC_STATION_CATALOGUE)
    if (!is.null(x) && nrow(x) > 0L && all(c("station_key", "area_cn", "monitoring_section_cn") %in% names(x))) {
      cw <- safe_read_csv(CNEMC_COORD_CROSSWALK)
      if (is.null(cw)) cw <- tibble()
      if (nrow(cw) > 0L && "station_key" %in% names(cw)) {
        cw <- cw %>%
          transmute(
            station_key = as.character(station_key),
            longitude = suppressWarnings(as.numeric(longitude)),
            latitude = suppressWarnings(as.numeric(latitude)),
            cw_city = if ("city_cn" %in% names(cw)) as.character(city_cn) else NA_character_,
            coordinate_source = if ("coordinate_source" %in% names(cw)) as.character(coordinate_source) else "cnemc_station_crosswalk"
          )
        x <- x %>% left_join(cw, by = "station_key")
      } else {
        x$longitude <- NA_real_
        x$latitude <- NA_real_
        x$cw_city <- NA_character_
        x$coordinate_source <- NA_character_
      }
      msg("CNEMC station metadata source: cached station catalogue (", nrow(x), " stations)")
      return(tibble(
        network = "CNEMC surface water",
        station_id = NA_character_,
        station_name = as.character(x$monitoring_section_cn),
        station_name_en = NA_character_,
        waterbody = if ("river_basin_cn" %in% names(x)) as.character(x$river_basin_cn) else NA_character_,
        admin1 = as.character(x$area_cn),
        admin2 = if ("city_cn" %in% names(x)) dplyr::coalesce(as.character(x$cw_city), as.character(x$city_cn)) else as.character(x$cw_city),
        longitude = suppressWarnings(as.numeric(x$longitude)),
        latitude = suppressWarnings(as.numeric(x$latitude)),
        coordinate_source = as.character(x$coordinate_source),
        station_key = as.character(x$station_key)
      ) %>%
        arrange(station_key) %>%
        distinct(network, station_key, .keep_all = TRUE))
    }
  }

  tibble()
}

build_station_metadata <- function(fujian, nmemc, cnemc_latest, onlimo_daily, onlimo_hist) {
  msg("Building unified station metadata ...")

  # Fujian -------------------------------------------------------------------
  # Coordinates have already been attached at observation level by build_fujian().
  # Summarise canonical Fujian station identities after removing temporary
  # operational-status suffixes; do not append unmatched crosswalk rows as stations.
  fujian_station <- fujian %>%
    filter(!is.na(station_name_key), nzchar(station_name_key)) %>%
    group_by(station_name_key) %>%
    summarise(
      network = "Fujian weekly",
      station_id = first_non_missing_chr(station_external_id),
      station_name = first_non_missing_chr(station_name_canonical),
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

  # NMEMC --------------------------------------------------------------------
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
      mutate(
        station_key = ifelse(
          !is.na(station_id) & nzchar(station_id),
          station_id,
          normalize_station_key(station_name)
        )
      ) %>%
      distinct(network, station_key, .keep_all = TRUE)
  } else {
    onlimo_station <- tibble()
  }

  # CNEMC --------------------------------------------------------------------
  cnemc_station <- build_cnemc_station_metadata_fast(cnemc_latest)

  out <- bind_rows(
    fujian_station,
    nmemc_station,
    onlimo_station,
    cnemc_station
  ) %>%
    arrange(network, station_name)

  if (!"in_historical_ip" %in% names(out)) out$in_historical_ip <- FALSE
  out$in_historical_ip[is.na(out$in_historical_ip)] <- FALSE

  out
}

# ----------------------------------------------------------------------------
# 12. Inventory helpers
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
      x <- as.Date(parse_posix_safe(x))
    } else {
      x <- x_date
    }
  }

  x <- x[!is.na(x)]
  if (length(x) == 0L) return(c(NA_character_, NA_character_))
  c(as.character(min(x)), as.character(max(x)))
}

make_inventory_row <- function(
  name, df, key_desc, source_rule, validation_status, output_path, notes = ""
) {
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
# 13. Build all canonical datasets
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
# 14. Save analysis datasets
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
# 15. Dataset inventory
# ----------------------------------------------------------------------------
inventory <- bind_rows(
  make_inventory_row(
    "fujian_weekly_analysis", fujian,
    "observation_key",
    "GitHub persistent canonical year state; rebuilt with scraper-equivalent processing",
    "PASS_WITH_CURRENT_YEAR_TIMING",
    paths$fujian,
    "Current-year GitHub-only recent keys retained. Station crosswalk metadata attached when available."
  ),
  make_inventory_row(
    "nmemc_marine_analysis", nmemc_marine,
    "source-native row",
    "Local processed master",
    "RECONCILIATION_EVIDENCE_REQUIRED",
    paths$nmemc,
    "Use the latest NMEMC reconciliation output to interpret current-year source differences."
  ),
  make_inventory_row(
    "cnemc_latest_analysis", cnemc$latest,
    "observation_key_hash",
    "Latest row version from reconciled PC + GitHub version union",
    "ROW_RECONCILED_WITH_SOURCE_REVISION_CONTEXT",
    paths$cnemc_latest,
    "Human-readable CNEMC fields and numeric parameters are derived after version selection; raw/hash fields are preserved."
  ),
  make_inventory_row(
    "cnemc_revision_history", cnemc$history,
    "row_hash",
    "Union of PC cumulative versions, GitHub full checkpoints, and GitHub targeted deltas",
    "ROW_RECONCILED_WITH_SOURCE_REVISION_CONTEXT",
    paths$cnemc_history,
    "Use for provenance/revision studies; this archive intentionally remains source-native."
  ),
  make_inventory_row(
    "onlimo_daily_analysis", onlimo_daily,
    "station_id + date",
    "Local cumulative archive independently reconciled against GitHub snapshots",
    "PASS_CURRENT_OVERLAP",
    paths$onlimo_daily,
    "Scientific payload agreement is exact for shared reconciled keys."
  ),
  make_inventory_row(
    "onlimo_historical_analysis", onlimo_historical,
    "station_id + date",
    "Reconstructed from all GitHub immutable historical observation partitions",
    "PASS_PARTIAL_CATCHUP",
    paths$onlimo_hist,
    "Coverage fields from reconciliation are attached; coverage remains partial until catch-up completes."
  ),
  make_inventory_row(
    "station_metadata", station_metadata,
    "network + station_key",
    "Unified metadata derived from source masters, station catalogues and crosswalks",
    "DERIVED_METADATA",
    paths$station,
    "CNEMC coordinates are attached only from the audited external crosswalk when available."
  )
)

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
# 16. Build manifest / provenance
# ----------------------------------------------------------------------------
if (isTRUE(WRITE_BUILD_MANIFEST)) {
  source_manifest <- tibble(
    role = c(
      "fujian_github_state",
      "nmemc_marine_local_master",
      "cnemc_pc_master",
      "cnemc_github_root",
      "cnemc_coordinate_crosswalk",
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
      CNEMC_COORD_CROSSWALK,
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

  write_csv_atomic(
    source_manifest,
    file.path(ANALYSIS_DIR, "analysis_build_manifest.csv")
  )
}

# ----------------------------------------------------------------------------
# 17. Console summary
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

if (file.exists(CNEMC_COORD_CROSSWALK)) {
  message("  CNEMC coordinate crosswalk: ", CNEMC_COORD_CROSSWALK)
} else {
  message("  CNEMC coordinate crosswalk: not yet available")
}

message("")
message("Important interpretation rules:")
message("  - CNEMC latest_analysis = one latest version per observation key.")
message("  - CNEMC latest_analysis includes derived readable Chinese and parsed numeric fields.")
message("  - CNEMC raw columns and hashes remain unchanged.")
message("  - CNEMC revision_history preserves source-native reconciled row versions.")
message("  - ONLIMO historical coverage is still partial until GitHub catch-up completes.")
message("  - Generated analysis files are derived products; keep source archives immutable.")
message("  - Derived RDS compression: ", RDS_COMPRESSION)
message("  - Fujian coordinate diagnostics: ", FUJIAN_COORD_DIAGNOSTICS)

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
