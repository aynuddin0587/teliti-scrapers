# ============================================================
# NMEMC marine water-quality archive
# Source: https://ep.nmemc.org.cn:8888/Water/
#
# Usage:
#   Rscript 01_nmemc_water_archive.R current
#   Rscript 01_nmemc_water_archive.R backfill
#
# Modes:
#   current  = check only the current calendar-year JSON file
#   backfill = fetch/check every year from 2017 through current year
#
# Local default output root:
#   D:/# R Project/penelitian/nmemc
#
# GitHub/portable output root:
#   set TELITI_DATA_ROOT and the script writes under
#   <TELITI_DATA_ROOT>/nmemc
#
# Outputs:
#   data/raw/waterYYYY.json
#   data/raw/waterYYYY_meta.rds
#   data/raw/archive/waterYYYY_TIMESTAMP.json
#   data/processed/nmemc_water_master.rds
#   data/processed/nmemc_water_master.csv.gz
#   data/processed/nmemc_water_manifest.csv
#   log/nmemc_water_YYYYMMDD.log
# ============================================================

options(stringsAsFactors = FALSE, timeout = 120)

# -----------------------------
# 1. Configuration
# -----------------------------
TELITI_DATA_ROOT <- Sys.getenv(
  "TELITI_DATA_ROOT",
  unset = "D:/# R Project/penelitian"
)

BASE_DIR <- file.path(TELITI_DATA_ROOT, "nmemc")
BASE_URL <- "https://ep.nmemc.org.cn:8888/Water"
FIRST_YEAR <- 2017L

COLLECTOR_TZ <- Sys.getenv(
  "TELITI_TIMEZONE",
  unset = "Asia/Taipei"
)

COLLECTOR_ID <- Sys.getenv(
  "TELITI_COLLECTOR_ID",
  unset = "local_pc"
)

GITHUB_RUN_ID <- Sys.getenv("GITHUB_RUN_ID", unset = "")
GITHUB_RUN_ATTEMPT <- Sys.getenv("GITHUB_RUN_ATTEMPT", unset = "")
GITHUB_SHA <- Sys.getenv("GITHUB_SHA", unset = "")

RAW_DIR <- file.path(BASE_DIR, "data", "raw")
ARCHIVE_DIR <- file.path(RAW_DIR, "archive")
PROCESSED_DIR <- file.path(BASE_DIR, "data", "processed")
LOG_DIR <- file.path(BASE_DIR, "log")
SCRIPT_DIR <- file.path(BASE_DIR, "script")

for (d in c(RAW_DIR, ARCHIVE_DIR, PROCESSED_DIR, LOG_DIR, SCRIPT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

log_file <- file.path(
  LOG_DIR,
  sprintf(
    "nmemc_water_%s.log",
    format(Sys.time(), "%Y%m%d", tz = COLLECTOR_TZ)
  )
)

log_msg <- function(...) {
  msg <- paste0(...)
  line <- sprintf(
    "%s | %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = COLLECTOR_TZ),
    msg
  )
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

# -----------------------------
# 2. Package checks
# -----------------------------
required_packages <- c("httr2", "jsonlite", "dplyr", "readr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them once before running the scheduled task."
  )
}

# -----------------------------
# 3. Run mode
# -----------------------------
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1L) tolower(args[[1]]) else "current"

if (!mode %in% c("current", "backfill")) {
  stop("Mode must be either 'current' or 'backfill'.")
}

current_year <- as.integer(
  format(Sys.time(), "%Y", tz = COLLECTOR_TZ)
)

years <- if (identical(mode, "backfill")) {
  seq.int(FIRST_YEAR, current_year)
} else {
  current_year
}

log_msg("START NMEMC marine water archive")
log_msg("Mode: ", mode)
log_msg("Collector ID: ", COLLECTOR_ID)
log_msg("Years: ", paste(years, collapse = ", "))

# -----------------------------
# 4. Helpers
# -----------------------------
meta_path <- function(year) {
  file.path(RAW_DIR, sprintf("water%d_meta.rds", year))
}

canonical_path <- function(year) {
  file.path(RAW_DIR, sprintf("water%d.json", year))
}

read_meta <- function(year) {
  p <- meta_path(year)
  if (file.exists(p)) readRDS(p) else list()
}

safe_header <- function(resp, name) {
  out <- httr2::resp_header(resp, name)
  if (
    is.null(out) ||
      length(out) == 0L ||
      is.na(out) ||
      identical(out, "")
  ) {
    NULL
  } else {
    out
  }
}

write_raw_atomic <- function(raw_body, destination) {
  tmp <- tempfile(
    pattern = "nmemc_",
    tmpdir = dirname(destination),
    fileext = ".json"
  )

  con <- file(tmp, open = "wb")

  on.exit({
    try(close(con), silent = TRUE)
    if (file.exists(tmp)) unlink(tmp)
  }, add = TRUE)

  writeBin(raw_body, con)
  close(con)

  if (!file.rename(tmp, destination)) {
    ok <- file.copy(tmp, destination, overwrite = TRUE)
    if (!ok) stop("Could not write raw file: ", destination)
    unlink(tmp)
  }

  invisible(destination)
}

fetch_year <- function(year) {
  url <- sprintf("%s/water%d.json", BASE_URL, year)
  dest <- canonical_path(year)
  meta_old <- read_meta(year)

  req <- httr2::request(url) |>
    httr2::req_user_agent(
      "Mozilla/5.0 (compatible; academic-research/NMEMC-archive)"
    ) |>
    httr2::req_timeout(90) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_error(is_error = function(resp) {
      status <- httr2::resp_status(resp)
      status >= 400L && status != 404L
    })

  # Conditional GET when source validators are available.
  if (file.exists(dest) && !is.null(meta_old$etag)) {
    req <- httr2::req_headers(req, `If-None-Match` = meta_old$etag)
  }

  if (file.exists(dest) && !is.null(meta_old$last_modified)) {
    req <- httr2::req_headers(
      req,
      `If-Modified-Since` = meta_old$last_modified
    )
  }

  log_msg("Checking ", url)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      log_msg("ERROR year ", year, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(
      list(
        year = year,
        status = "error",
        changed = FALSE,
        path = dest
      )
    )
  }

  status <- httr2::resp_status(resp)

  if (status == 304L) {
    log_msg("Year ", year, ": unchanged (HTTP 304)")
    return(
      list(
        year = year,
        status = "not_modified",
        changed = FALSE,
        path = dest
      )
    )
  }

  if (status == 404L) {
    log_msg("Year ", year, ": source file not available (HTTP 404)")
    return(
      list(
        year = year,
        status = "not_found",
        changed = FALSE,
        path = dest
      )
    )
  }

  if (status < 200L || status >= 300L) {
    log_msg("Year ", year, ": unexpected HTTP status ", status)
    return(
      list(
        year = year,
        status = paste0("http_", status),
        changed = FALSE,
        path = dest
      )
    )
  }

  raw_body <- httr2::resp_body_raw(resp)

  if (length(raw_body) == 0L) {
    log_msg("Year ", year, ": empty response; canonical file left unchanged")
    return(
      list(
        year = year,
        status = "empty",
        changed = FALSE,
        path = dest
      )
    )
  }

  tmp <- tempfile(
    pattern = sprintf("water%d_", year),
    fileext = ".json"
  )

  con <- file(tmp, open = "wb")
  writeBin(raw_body, con)
  close(con)

  on.exit(
    if (file.exists(tmp)) unlink(tmp),
    add = TRUE
  )

  new_md5 <- unname(tools::md5sum(tmp))
  old_md5 <- if (file.exists(dest)) {
    unname(tools::md5sum(dest))
  } else {
    NA_character_
  }

  changed <- !file.exists(dest) || !identical(new_md5, old_md5)

  if (changed) {
    timestamp <- format(
      Sys.time(),
      "%Y%m%d_%H%M%S",
      tz = COLLECTOR_TZ
    )

    snapshot <- file.path(
      ARCHIVE_DIR,
      sprintf("water%d_%s.json", year, timestamp)
    )

    if (!file.copy(tmp, snapshot, overwrite = FALSE)) {
      stop("Could not create archive snapshot: ", snapshot)
    }

    if (!file.copy(tmp, dest, overwrite = TRUE)) {
      stop("Could not update canonical raw file: ", dest)
    }

    log_msg(
      "Year ", year,
      ": changed; ", length(raw_body),
      " bytes; md5=", new_md5,
      "; archived as ", basename(snapshot)
    )
  } else {
    log_msg(
      "Year ", year,
      ": body downloaded but checksum unchanged; md5=",
      new_md5
    )
  }

  meta_new <- list(
    year = year,
    url = url,
    etag = safe_header(resp, "etag"),
    last_modified = safe_header(resp, "last-modified"),
    content_type = safe_header(resp, "content-type"),
    checked_at = Sys.time(),
    md5 = new_md5,
    bytes = length(raw_body),
    collector_id = COLLECTOR_ID,
    github_run_id = if (nzchar(GITHUB_RUN_ID)) GITHUB_RUN_ID else NA_character_,
    github_run_attempt = if (nzchar(GITHUB_RUN_ATTEMPT)) {
      GITHUB_RUN_ATTEMPT
    } else {
      NA_character_
    },
    scraper_code_commit = if (nzchar(GITHUB_SHA)) GITHUB_SHA else NA_character_
  )

  saveRDS(meta_new, meta_path(year))

  list(
    year = year,
    status = "ok",
    changed = changed,
    path = dest
  )
}

normalize_nmemc <- function(df, source_year, source_file) {
  expected <- c(
    "sea", "province", "city", "site", "lon", "lat", "minitor_month",
    "pH", "rjy", "hxxyl", "wjd", "hxlxy", "syl", "szlb"
  )

  missing <- setdiff(expected, names(df))

  if (length(missing) > 0L) {
    stop(
      "Unexpected schema in ", basename(source_file),
      "; missing field(s): ", paste(missing, collapse = ", ")
    )
  }

  tibble::as_tibble(df) |>
    dplyr::transmute(
      source_year = as.integer(source_year),
      sea = as.character(.data$sea),
      province = as.character(.data$province),
      city = as.character(.data$city),
      site_code = as.character(.data$site),
      longitude_raw = as.character(.data$lon),
      latitude_raw = as.character(.data$lat),
      longitude = suppressWarnings(
        readr::parse_number(
          as.character(.data$lon),
          na = c("", "-", "--", "NA")
        )
      ),
      latitude = suppressWarnings(
        readr::parse_number(
          as.character(.data$lat),
          na = c("", "-", "--", "NA")
        )
      ),
      monitor_time_raw = as.character(.data$minitor_month),
      ph_raw = as.character(.data$pH),
      dissolved_oxygen_mg_l_raw = as.character(.data$rjy),
      cod_mg_l_raw = as.character(.data$hxxyl),
      inorganic_nitrogen_mg_l_raw = as.character(.data$wjd),
      reactive_phosphate_mg_l_raw = as.character(.data$hxlxy),
      petroleum_mg_l_raw = as.character(.data$syl),
      water_quality_class = as.character(.data$szlb),
      source_file = basename(source_file)
    )
}

parse_raw_year <- function(path) {
  year <- as.integer(
    sub("^water([0-9]{4})\\.json$", "\\1", basename(path))
  )

  x <- tryCatch(
    jsonlite::fromJSON(path, simplifyDataFrame = TRUE),
    error = function(e) {
      stop(
        "Could not parse ", basename(path),
        ": ", conditionMessage(e)
      )
    }
  )

  if (!is.data.frame(x)) {
    stop("Expected a JSON array of records in ", basename(path), ".")
  }

  normalize_nmemc(x, year, path)
}

meta_value <- function(meta, name, default = NA_character_) {
  value <- meta[[name]]
  if (is.null(value) || length(value) == 0L) default else value
}

rebuild_master <- function() {
  raw_files <- list.files(
    RAW_DIR,
    pattern = "^water[0-9]{4}\\.json$",
    full.names = TRUE
  )

  if (length(raw_files) == 0L) {
    log_msg(
      "No canonical raw JSON files are available; master dataset not rebuilt"
    )
    return(invisible(NULL))
  }

  raw_files <- raw_files[order(raw_files)]
  pieces <- lapply(raw_files, parse_raw_year)
  master <- dplyr::bind_rows(pieces)

  duplicate_n <- sum(duplicated(master))

  out_rds <- file.path(
    PROCESSED_DIR,
    "nmemc_water_master.rds"
  )

  out_csv <- file.path(
    PROCESSED_DIR,
    "nmemc_water_master.csv.gz"
  )

  out_manifest <- file.path(
    PROCESSED_DIR,
    "nmemc_water_manifest.csv"
  )

  saveRDS(master, out_rds, compress = "xz")
  readr::write_csv(master, out_csv, na = "")

  manifest <- do.call(
    rbind,
    lapply(seq_along(raw_files), function(i) {
      p <- raw_files[[i]]
      yr <- as.integer(
        sub("^water([0-9]{4})\\.json$", "\\1", basename(p))
      )
      dat <- pieces[[i]]
      meta <- read_meta(yr)

      checked_at <- meta_value(meta, "checked_at", NA)
      checked_at_text <- if (
        length(checked_at) == 1L && !is.na(checked_at)
      ) {
        format(
          as.POSIXct(checked_at, origin = "1970-01-01"),
          "%Y-%m-%d %H:%M:%S",
          tz = COLLECTOR_TZ
        )
      } else {
        NA_character_
      }

      data.frame(
        year = yr,
        rows = nrow(dat),
        raw_file = basename(p),
        raw_bytes = file.info(p)$size,
        md5 = unname(tools::md5sum(p)),
        etag = meta_value(meta, "etag"),
        last_modified = meta_value(meta, "last_modified"),
        checked_at = checked_at_text,
        collector_id = meta_value(meta, "collector_id"),
        github_run_id = meta_value(meta, "github_run_id"),
        github_run_attempt = meta_value(meta, "github_run_attempt"),
        scraper_code_commit = meta_value(meta, "scraper_code_commit"),
        stringsAsFactors = FALSE
      )
    })
  )

  readr::write_csv(manifest, out_manifest, na = "")

  log_msg(
    "Master rebuilt: ", nrow(master),
    " rows across ", length(raw_files),
    " year file(s); exact duplicate rows detected: ", duplicate_n
  )
  log_msg("RDS: ", out_rds)
  log_msg("CSV.GZ: ", out_csv)
  log_msg("Manifest: ", out_manifest)

  invisible(master)
}

# -----------------------------
# 5. Fetch
# -----------------------------
results <- lapply(years, fetch_year)

changed_any <- any(
  vapply(
    results,
    function(x) isTRUE(x$changed),
    logical(1)
  )
)

canonical_missing <- any(
  !file.exists(
    vapply(
      results,
      function(x) x$path,
      character(1)
    )
  )
)

master_rds <- file.path(
  PROCESSED_DIR,
  "nmemc_water_master.rds"
)

if (changed_any || !file.exists(master_rds)) {
  rebuild_master()
} else {
  log_msg("No source changes detected; existing processed master retained")
}

if (canonical_missing) {
  log_msg("NOTE: at least one requested year has no canonical raw file")
}

log_msg("END NMEMC marine water archive")