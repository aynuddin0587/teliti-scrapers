# ============================================================
# NMEMC/CNEMC surface-water automatic monitoring archive
# Public page:
#   https://szzdjc.cnemc.cn:8070/GJZ/Business/Publish/Main.html
# Data endpoint discovered from the public page JavaScript:
#   POST /GJZ/Ajax/Publish.ashx
#   action=getRealDatas
#
# Purpose
#   - retrieve the complete current nationwide public snapshot
#   - preserve source responses when the snapshot changes
#   - maintain a cumulative observation archive across scheduled runs
#   - preserve all measurement strings exactly as published
#
# Recommended use:
#   source("nmemc/script/02_nmemc_surfacewater_archive.R")
#
# Or from Windows Task Scheduler:
#   Rscript "D:/# R Project/penelitian/nmemc/script/02_nmemc_surfacewater_archive.R"
#
# Suggested scheduler cadence:
#   every 20 minutes. The public page itself refreshes regional data at about
#   this interval, and the script archives a source snapshot only when content
#   changes, so frequent checks do not create duplicate raw archives.
#
# Outputs under D:/# R Project/penelitian/nmemc/data/surfacewater
#   source/surfacewater_current_raw.rds
#   source/area_river_current.json
#   archive/YYYY/surfacewater_TIMESTAMP_raw.rds
#   archive/YYYY/area_river_TIMESTAMP.json
#   processed/nmemc_surfacewater_current.rds
#   processed/nmemc_surfacewater_current.csv.gz
#   processed/nmemc_surfacewater_observations.rds
#   processed/nmemc_surfacewater_observations.csv.gz
#   processed/nmemc_surfacewater_header_dictionary.csv
#   processed/nmemc_surfacewater_run_manifest.csv
#
# Notes
#   * "source" means an unaltered copy of the PUBLIC response as received by us;
#     it does not imply pre-QA/QC instrument or laboratory raw data.
#   * The public response supplies table headers dynamically. The script maps
#     the first five positions from the website JavaScript and maps recognized
#     indicator headers conservatively; all unmatched fields remain parameter_NN.
# ============================================================

options(stringsAsFactors = FALSE, timeout = 120, httr2_progress = FALSE)

# -----------------------------
# 1. Configuration
# -----------------------------
TELITI_DATA_ROOT <- Sys.getenv(
  "TELITI_DATA_ROOT",
  unset = "D:/# R Project/penelitian"
)

BASE_DIR <- file.path(TELITI_DATA_ROOT, "nmemc")

# Collector/source time and provenance.
COLLECTOR_TZ <- Sys.getenv("TELITI_TIMEZONE", unset = "Asia/Taipei")
SOURCE_TZ <- "Asia/Shanghai"
COLLECTOR_ID <- Sys.getenv("TELITI_COLLECTOR_ID", unset = "local_pc")
GITHUB_RUN_ID <- Sys.getenv("GITHUB_RUN_ID", unset = "")
GITHUB_RUN_ATTEMPT <- Sys.getenv("GITHUB_RUN_ATTEMPT", unset = "")
GITHUB_SHA <- Sys.getenv("GITHUB_SHA", unset = "")

MAIN_URL <- "https://szzdjc.cnemc.cn:8070/GJZ/Business/Publish/Main.html"
ENDPOINT <- "https://szzdjc.cnemc.cn:8070/GJZ/Ajax/Publish.ashx"
ORIGIN   <- "https://szzdjc.cnemc.cn:8070"

# The public application currently returns a plain-text -1 when getRealDatas
# is called cold. A browser-like warm-up sequence (Main.html, then
# getArea_RiverDic, then getRealDatas) restores the expected JSON response.
# Keep a temporary cookie jar even though the site did not issue cookies in the
# 2026-09-29 diagnostic; this preserves compatibility if that changes later.
BROWSER_USER_AGENT <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/140.0.0.0 Safari/537.36"
)

SURFACE_DIR   <- file.path(BASE_DIR, "data", "surfacewater")
SOURCE_DIR    <- file.path(SURFACE_DIR, "source")
ARCHIVE_DIR   <- file.path(SURFACE_DIR, "archive")
PROCESSED_DIR <- file.path(SURFACE_DIR, "processed")
LOG_DIR       <- file.path(BASE_DIR, "log")
SCRIPT_DIR    <- file.path(BASE_DIR, "script")
FAILED_RESPONSE_DIR <- file.path(LOG_DIR, "failed_responses")

for (d in c(
  SURFACE_DIR, SOURCE_DIR, ARCHIVE_DIR, PROCESSED_DIR, LOG_DIR, SCRIPT_DIR,
  FAILED_RESPONSE_DIR
)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# The public page itself uses PageSize=2000 when returning to the nationwide
# view. Keep this configurable in case the server changes its limit.
PAGE_SIZE <- suppressWarnings(as.integer(getOption("nmemc.surfacewater.page_size", 2000L)))
if (is.na(PAGE_SIZE) || PAGE_SIZE < 1L) PAGE_SIZE <- 2000L

AREA_ID  <- as.character(getOption("nmemc.surfacewater.area_id", ""))
RIVER_ID <- as.character(getOption("nmemc.surfacewater.river_id", ""))
MN_NAME  <- as.character(getOption("nmemc.surfacewater.mn_name", ""))

# A short delay is only relevant if the result spans multiple pages.
PAGE_DELAY_SECONDS <- suppressWarnings(as.numeric(getOption("nmemc.surfacewater.page_delay", 0.25)))
if (is.na(PAGE_DELAY_SECONDS) || PAGE_DELAY_SECONDS < 0) PAGE_DELAY_SECONDS <- 0.25

# Keep the HTTP layer short and bounded. A complete snapshot has its own retry
# loop below, so very long nested HTTP retries only consume the workflow budget.
HTTP_TIMEOUT_SECONDS <- suppressWarnings(as.numeric(
  getOption("nmemc.surfacewater.http_timeout_seconds", 30)
))
if (!is.finite(HTTP_TIMEOUT_SECONDS) || HTTP_TIMEOUT_SECONDS <= 0) {
  HTTP_TIMEOUT_SECONDS <- 30
}

HTTP_MAX_TRIES <- suppressWarnings(as.integer(
  getOption("nmemc.surfacewater.http_max_tries", 2L)
))
if (is.na(HTTP_MAX_TRIES) || HTTP_MAX_TRIES < 1L) HTTP_MAX_TRIES <- 2L

# Retry the complete fetch+parse transaction for semantic source failures such
# as a valid HTTP response containing an empty or otherwise invalid snapshot.
SNAPSHOT_MAX_ATTEMPTS <- suppressWarnings(as.integer(
  getOption("nmemc.surfacewater.snapshot_max_attempts", 3L)
))
if (is.na(SNAPSHOT_MAX_ATTEMPTS) || SNAPSHOT_MAX_ATTEMPTS < 1L) {
  SNAPSHOT_MAX_ATTEMPTS <- 3L
}

SNAPSHOT_RETRY_SECONDS <- suppressWarnings(as.numeric(
  getOption("nmemc.surfacewater.snapshot_retry_seconds", c(10, 30))
))
SNAPSHOT_RETRY_SECONDS <- SNAPSHOT_RETRY_SECONDS[
  is.finite(SNAPSHOT_RETRY_SECONDS) & SNAPSHOT_RETRY_SECONDS >= 0
]

# The cumulative RDS is the canonical processed history and is updated on every
# successful collection. The compressed CSV is a convenience export; rewriting
# ~1 million rows every 20 minutes is expensive and adds no source provenance.
# By default refresh that CSV at most once per day. Set this option to 0 to
# restore the previous always-write behavior, or Inf to disable automatic CSV
# refreshes after the file has been created once.
MASTER_CSV_REFRESH_HOURS <- suppressWarnings(as.numeric(
  getOption("nmemc.surfacewater.master_csv_refresh_hours", 24)
))
if (is.na(MASTER_CSV_REFRESH_HOURS) || MASTER_CSV_REFRESH_HOURS < 0) {
  MASTER_CSV_REFRESH_HOURS <- 24
}

log_file <- file.path(
  LOG_DIR,
  sprintf(
    "nmemc_surfacewater_%s.log",
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
required_packages <- c("httr2", "jsonlite", "dplyr", "readr", "tibble", "digest")
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
# 3. Paths and small helpers
# -----------------------------
current_raw_path <- file.path(SOURCE_DIR, "surfacewater_current_raw.rds")
current_meta_path <- file.path(SOURCE_DIR, "surfacewater_current_meta.rds")
area_river_path <- file.path(SOURCE_DIR, "area_river_current.json")

current_rds_path <- file.path(PROCESSED_DIR, "nmemc_surfacewater_current.rds")
current_csv_path <- file.path(PROCESSED_DIR, "nmemc_surfacewater_current.csv.gz")
master_rds_path <- file.path(PROCESSED_DIR, "nmemc_surfacewater_observations.rds")
master_csv_path <- file.path(PROCESSED_DIR, "nmemc_surfacewater_observations.csv.gz")
header_dictionary_path <- file.path(PROCESSED_DIR, "nmemc_surfacewater_header_dictionary.csv")
run_manifest_path <- file.path(PROCESSED_DIR, "nmemc_surfacewater_run_manifest.csv")
failure_manifest_path <- file.path(LOG_DIR, "nmemc_surfacewater_failure_manifest.csv")

write_raw_atomic <- function(raw_body, destination) {
  tmp <- tempfile(pattern = "nmemc_surface_", tmpdir = dirname(destination))
  con <- file(tmp, open = "wb")
  on.exit({
    try(close(con), silent = TRUE)
    if (file.exists(tmp)) unlink(tmp)
  }, add = TRUE)

  writeBin(raw_body, con)
  close(con)

  if (!file.rename(tmp, destination)) {
    ok <- file.copy(tmp, destination, overwrite = TRUE)
    if (!ok) stop("Could not write file: ", destination)
    unlink(tmp)
  }
  invisible(destination)
}

md5_raw <- function(x) {
  tmp <- tempfile(pattern = "nmemc_md5_")
  con <- file(tmp, open = "wb")
  on.exit({
    try(close(con), silent = TRUE)
    if (file.exists(tmp)) unlink(tmp)
  }, add = TRUE)
  writeBin(x, con)
  close(con)
  unname(tools::md5sum(tmp))
}

md5_text <- function(x) {
  digest::digest(enc2utf8(paste0(x, collapse = "")), algo = "md5", serialize = FALSE)
}

strip_utf8_bom <- function(x) {
  bom <- as.raw(c(0xEF, 0xBB, 0xBF))
  if (length(x) >= 3L && identical(x[1:3], bom)) x[-(1:3)] else x
}

raw_to_utf8 <- function(x) {
  x <- strip_utf8_bom(x)
  txt <- rawToChar(x)
  Encoding(txt) <- "UTF-8"
  txt
}

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  as.character(x[[1]])
}

cnemc_stop <- function(failure_type, message) {
  cond <- structure(
    list(message = as.character(message), call = NULL, failure_type = failure_type),
    class = c("cnemc_collection_error", "error", "condition")
  )
  stop(cond)
}

classify_collection_error <- function(e) {
  if (inherits(e, "cnemc_collection_error") && !is.null(e$failure_type)) {
    return(as.character(e$failure_type))
  }

  msg <- conditionMessage(e)
  if (grepl("timed out|timeout", msg, ignore.case = TRUE)) return("transport_timeout")
  if (grepl(
    "empty reply|server returned nothing|failed to perform http request|curl_fetch|connection reset|could not resolve|couldn't connect",
    msg, ignore.case = TRUE
  )) return("transport_failure")
  if (grepl("HTTP [45][0-9][0-9]", msg, ignore.case = TRUE)) return("http_error")
  "unclassified_error"
}

body_preview <- function(raw_body, max_chars = 500L) {
  if (is.null(raw_body) || length(raw_body) == 0L) return("<empty body>")
  txt <- tryCatch(
    raw_to_utf8(raw_body),
    error = function(e) {
      n <- min(length(raw_body), 128L)
      paste(sprintf("%02X", as.integer(raw_body[seq_len(n)])), collapse = " ")
    }
  )
  txt <- gsub("[\r\n\t]+", " ", txt, perl = TRUE)
  txt <- gsub("\\s+", " ", txt, perl = TRUE)
  txt <- trimws(txt)
  if (!nzchar(txt)) return("<empty text body>")
  substr(txt, 1L, max_chars)
}

save_failed_response <- function(
  raw_body, failure_type, page_index = NA_integer_, snapshot_attempt = NA_integer_,
  status = NA_integer_, content_type = NA_character_, server_date = NA_character_
) {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = COLLECTOR_TZ)
  page_tag <- if (is.na(page_index)) "pageNA" else sprintf("page%02d", page_index)
  attempt_tag <- if (is.na(snapshot_attempt)) "attemptNA" else sprintf("attempt%02d", snapshot_attempt)
  base <- paste("cnemc", stamp, attempt_tag, page_tag, failure_type, sep = "_")

  raw_path <- file.path(FAILED_RESPONSE_DIR, paste0(base, ".raw"))
  meta_path <- file.path(FAILED_RESPONSE_DIR, paste0(base, ".txt"))

  if (!is.null(raw_body)) write_raw_atomic(raw_body, raw_path)

  meta <- c(
    paste0("failure_type=", failure_type),
    paste0("snapshot_attempt=", snapshot_attempt),
    paste0("page_index=", page_index),
    paste0("http_status=", status),
    paste0("content_type=", content_type),
    paste0("server_date=", server_date),
    paste0("body_bytes=", if (is.null(raw_body)) 0L else length(raw_body)),
    paste0("body_preview=", body_preview(raw_body))
  )
  writeLines(meta, meta_path, useBytes = TRUE)

  log_msg(
    "Saved failed CNEMC response: type=", failure_type,
    "; status=", status,
    "; content_type=", content_type,
    "; bytes=", if (is.null(raw_body)) 0L else length(raw_body),
    "; preview=", body_preview(raw_body),
    "; raw=", basename(raw_path)
  )
  invisible(raw_path)
}

append_failure_manifest <- function(attempt, failure_type, message) {
  entry <- tibble::tibble(
    failed_at = as.POSIXct(Sys.time(), tz = COLLECTOR_TZ),
    collector_id = COLLECTOR_ID,
    github_run_id = if (nzchar(GITHUB_RUN_ID)) GITHUB_RUN_ID else NA_character_,
    github_run_attempt = if (nzchar(GITHUB_RUN_ATTEMPT)) GITHUB_RUN_ATTEMPT else NA_character_,
    scraper_code_commit = if (nzchar(GITHUB_SHA)) GITHUB_SHA else NA_character_,
    snapshot_attempt = as.integer(attempt),
    failure_type = as.character(failure_type),
    message = as.character(message),
    endpoint = ENDPOINT
  )

  if (file.exists(failure_manifest_path)) {
    old <- suppressWarnings(readr::read_csv(failure_manifest_path, show_col_types = FALSE))
    out <- dplyr::bind_rows(old, entry)
  } else {
    out <- entry
  }
  readr::write_csv(out, failure_manifest_path, na = "")
  invisible(entry)
}

# CNEMC publishes monitoring time as MM-DD HH:MM without the year.
# Infer the year from the collection timestamp while preserving the original
# monitoring_time_raw exactly as published. Around New Year, a date that lies
# more than 180 days in the future relative to collection is interpreted as
# belonging to the previous year (e.g. Dec 31 observations collected Jan 1).
infer_observation_datetime <- function(monitoring_time_raw, collected_at) {
  x <- trimws(as.character(monitoring_time_raw))
  collected <- as.POSIXct(collected_at, tz = "Asia/Shanghai")

  if (length(collected) == 1L && length(x) > 1L) {
    collected <- rep(collected, length(x))
  }
  if (length(collected) != length(x)) {
    stop("collected_at must have length 1 or match monitoring_time_raw")
  }

  year_now <- suppressWarnings(as.integer(format(collected, "%Y")))
  candidate <- as.POSIXct(
    paste(year_now, x),
    format = "%Y %m-%d %H:%M",
    tz = "Asia/Shanghai"
  )

  future_days <- suppressWarnings(as.numeric(difftime(candidate, collected, units = "days")))
  previous_year <- !is.na(future_days) & future_days > 180

  if (any(previous_year)) {
    candidate[previous_year] <- as.POSIXct(
      paste(year_now[previous_year] - 1L, x[previous_year]),
      format = "%Y %m-%d %H:%M",
      tz = "Asia/Shanghai"
    )
  }

  candidate
}

recompute_observation_hashes <- function(dat, source_cols) {
  if (!all(c("monitoring_time_raw", "collected_at") %in% names(dat))) {
    stop("Cannot build observation timestamp: required time fields are missing")
  }

  dat$collected_at <- as.POSIXct(dat$collected_at, tz = "Asia/Shanghai")
  dat$observation_datetime <- infer_observation_datetime(
    dat$monitoring_time_raw,
    dat$collected_at
  )
  dat$observation_year_inferred <- suppressWarnings(
    as.integer(format(dat$observation_datetime, "%Y"))
  )

  key_cols <- intersect(
    c("area", "river_basin", "monitoring_section", "observation_datetime"),
    names(dat)
  )

  # Include the inferred full timestamp in the row hash because the public
  # source time omits year; otherwise identical values one year apart could
  # incorrectly collapse into the same published row version.
  value_cols <- unique(c(intersect(source_cols, names(dat)), "observation_datetime"))

  dat$observation_key_hash <- vapply(
    seq_len(nrow(dat)),
    function(i) {
      vals <- unlist(dat[i, key_cols, drop = FALSE], use.names = FALSE)
      digest::digest(paste(vals, collapse = "\u001F"), algo = "xxhash64", serialize = FALSE)
    },
    character(1)
  )

  dat$row_hash <- vapply(
    seq_len(nrow(dat)),
    function(i) {
      vals <- unlist(dat[i, value_cols, drop = FALSE], use.names = FALSE)
      labelled_vals <- paste(value_cols, vals, sep = "=")
      digest::digest(paste(labelled_vals, collapse = "\u001F"), algo = "xxhash64", serialize = FALSE)
    },
    character(1)
  )

  dat
}

# -----------------------------
# 4. HTTP requests
# -----------------------------
add_cookie_jar <- function(req, cookie_file = NULL) {
  if (!is.null(cookie_file) && length(cookie_file) == 1L && nzchar(cookie_file)) {
    req <- httr2::req_cookie_preserve(req, cookie_file)
  }
  req
}

base_request <- function(cookie_file = NULL) {
  req <- httr2::request(ENDPOINT) |>
    httr2::req_user_agent(BROWSER_USER_AGENT) |>
    httr2::req_headers(
      Referer = MAIN_URL,
      Origin = ORIGIN,
      Accept = "application/json, text/javascript, */*; q=0.01",
      `Accept-Language` = "zh-CN,zh;q=0.9,en;q=0.8",
      `X-Requested-With` = "XMLHttpRequest",
      `Cache-Control` = "no-cache",
      Pragma = "no-cache"
    )

  req <- add_cookie_jar(req, cookie_file)

  req |>
    httr2::req_timeout(HTTP_TIMEOUT_SECONDS) |>
    httr2::req_retry(max_tries = HTTP_MAX_TRIES, retry_on_failure = TRUE)
}

main_page_request <- function(cookie_file = NULL) {
  req <- httr2::request(MAIN_URL) |>
    httr2::req_user_agent(BROWSER_USER_AGENT) |>
    httr2::req_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
      `Accept-Language` = "zh-CN,zh;q=0.9,en;q=0.8",
      `Cache-Control` = "no-cache",
      Pragma = "no-cache"
    )

  req <- add_cookie_jar(req, cookie_file)

  req |>
    httr2::req_timeout(HTTP_TIMEOUT_SECONDS) |>
    httr2::req_retry(max_tries = HTTP_MAX_TRIES, retry_on_failure = TRUE)
}

perform_request_or_stop <- function(req, context) {
  tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cnemc_stop(
        classify_collection_error(e),
        paste0(context, ": ", conditionMessage(e))
      )
    }
  )
}

warm_cnemc_application <- function(cookie_file, snapshot_attempt = NA_integer_) {
  log_msg("Initializing CNEMC application: Main.html -> getArea_RiverDic")

  main_resp <- perform_request_or_stop(
    main_page_request(cookie_file),
    "CNEMC Main.html warm-up failed"
  )
  main_status <- httr2::resp_status(main_resp)
  main_body <- httr2::resp_body_raw(main_resp)
  main_type <- safe_chr(httr2::resp_header(main_resp, "content-type"))
  main_date <- safe_chr(httr2::resp_header(main_resp, "date"))

  if (main_status < 200L || main_status >= 300L) {
    save_failed_response(
      raw_body = main_body,
      failure_type = "warmup_main_http_error",
      page_index = NA_integer_,
      snapshot_attempt = snapshot_attempt,
      status = main_status,
      content_type = main_type,
      server_date = main_date
    )
    cnemc_stop(
      "warmup_main_http_error",
      paste0("CNEMC Main.html warm-up returned HTTP ", main_status)
    )
  }

  dic_req <- base_request(cookie_file) |>
    httr2::req_body_form(action = "getArea_RiverDic")

  dic_resp <- perform_request_or_stop(
    dic_req,
    "CNEMC getArea_RiverDic warm-up failed"
  )
  dic_status <- httr2::resp_status(dic_resp)
  dic_body <- httr2::resp_body_raw(dic_resp)
  dic_type <- safe_chr(httr2::resp_header(dic_resp, "content-type"))
  dic_date <- safe_chr(httr2::resp_header(dic_resp, "date"))

  if (dic_status < 200L || dic_status >= 300L) {
    save_failed_response(
      raw_body = dic_body,
      failure_type = "warmup_dictionary_http_error",
      page_index = NA_integer_,
      snapshot_attempt = snapshot_attempt,
      status = dic_status,
      content_type = dic_type,
      server_date = dic_date
    )
    cnemc_stop(
      "warmup_dictionary_http_error",
      paste0("CNEMC getArea_RiverDic warm-up returned HTTP ", dic_status)
    )
  }

  dic_text <- tryCatch(
    raw_to_utf8(dic_body),
    error = function(e) {
      save_failed_response(
        dic_body, "warmup_dictionary_invalid_body", NA_integer_, snapshot_attempt,
        dic_status, dic_type, dic_date
      )
      cnemc_stop(
        "warmup_dictionary_invalid_body",
        paste0("Could not decode getArea_RiverDic response: ", conditionMessage(e))
      )
    }
  )

  dic_parsed <- tryCatch(
    jsonlite::fromJSON(dic_text, simplifyVector = FALSE),
    error = function(e) {
      save_failed_response(
        dic_body, "warmup_dictionary_invalid_json", NA_integer_, snapshot_attempt,
        dic_status, dic_type, dic_date
      )
      cnemc_stop(
        "warmup_dictionary_invalid_json",
        paste0("Could not parse getArea_RiverDic JSON: ", conditionMessage(e))
      )
    }
  )

  if (!is.list(dic_parsed)) {
    save_failed_response(
      dic_body, "warmup_dictionary_unexpected_payload", NA_integer_, snapshot_attempt,
      dic_status, dic_type, dic_date
    )
    cnemc_stop(
      "warmup_dictionary_unexpected_payload",
      paste0(
        "Unexpected getArea_RiverDic response; body preview: ",
        body_preview(dic_body)
      )
    )
  }

  log_msg(
    "CNEMC application initialized: Main.html status=", main_status,
    "; area-river entries=", length(dic_parsed)
  )

  list(
    main_status = main_status,
    area_river_status = dic_status,
    area_river_body = dic_body,
    area_river_md5 = md5_raw(dic_body)
  )
}

fetch_page_raw <- function(
  page_index, page_size = PAGE_SIZE, snapshot_attempt = NA_integer_,
  cookie_file = NULL
) {
  req <- base_request(cookie_file) |>
    httr2::req_body_form(
      AreaID = AREA_ID,
      RiverID = RIVER_ID,
      MNName = MN_NAME,
      PageIndex = as.character(page_index),
      PageSize = as.character(page_size),
      action = "getRealDatas"
    )

  resp <- perform_request_or_stop(
    req,
    paste0("CNEMC getRealDatas page ", page_index, " request failed")
  )

  status <- httr2::resp_status(resp)
  raw_body <- httr2::resp_body_raw(resp)
  content_type <- safe_chr(httr2::resp_header(resp, "content-type"))
  server_date <- safe_chr(httr2::resp_header(resp, "date"))

  if (status < 200L || status >= 300L) {
    save_failed_response(
      raw_body = raw_body,
      failure_type = "http_error",
      page_index = page_index,
      snapshot_attempt = snapshot_attempt,
      status = status,
      content_type = content_type,
      server_date = server_date
    )
    cnemc_stop("http_error", paste0("HTTP ", status, " while retrieving page ", page_index))
  }

  list(
    page_index = as.integer(page_index),
    status = status,
    body = raw_body,
    content_type = content_type,
    server_date = server_date
  )
}

parse_api_page <- function(
  raw_body, page_index = NA_integer_, response_meta = NULL, snapshot_attempt = NA_integer_
) {
  status <- if (is.null(response_meta$status)) NA_integer_ else response_meta$status
  content_type <- if (is.null(response_meta$content_type)) NA_character_ else response_meta$content_type
  server_date <- if (is.null(response_meta$server_date)) NA_character_ else response_meta$server_date

  txt <- tryCatch(
    raw_to_utf8(raw_body),
    error = function(e) {
      save_failed_response(
        raw_body, "invalid_body_encoding", page_index, snapshot_attempt,
        status, content_type, server_date
      )
      cnemc_stop(
        "invalid_body_encoding",
        paste0("Could not decode API response on page ", page_index, ": ", conditionMessage(e))
      )
    }
  )

  out <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) {
      save_failed_response(
        raw_body, "invalid_json", page_index, snapshot_attempt,
        status, content_type, server_date
      )
      cnemc_stop(
        "invalid_json",
        paste0("Could not parse API JSON on page ", page_index, ": ", conditionMessage(e))
      )
    }
  )

  if (!is.list(out)) {
    save_failed_response(
      raw_body, "unexpected_payload", page_index, snapshot_attempt,
      status, content_type, server_date
    )
    cnemc_stop(
      "unexpected_payload",
      paste0(
        "Unexpected API response type on page ", page_index,
        "; body preview: ", body_preview(raw_body)
      )
    )
  }

  # The public JavaScript tests data.result before using the response.
  result_value <- out$result
  result_text <- if (is.null(result_value) || length(result_value) == 0L) {
    NA_character_
  } else {
    tolower(as.character(result_value[[1]]))
  }
  result_ok <- !is.na(result_text) && !result_text %in% c("0", "false")

  if (!result_ok) {
    save_failed_response(
      raw_body, "api_result_false", page_index, snapshot_attempt,
      status, content_type, server_date
    )
    cnemc_stop(
      "api_result_false",
      paste0("API returned result=0/false/missing on page ", page_index)
    )
  }

  headers <- unlist(out$thead, use.names = FALSE)
  rows <- out$tbody
  total_pages <- suppressWarnings(as.integer(unlist(out$total, use.names = FALSE)[1]))
  if (is.na(total_pages) || total_pages < 1L) total_pages <- 1L
  reported_records <- suppressWarnings(as.integer(unlist(out$records, use.names = FALSE)[1]))
  if (length(reported_records) == 0L || is.na(reported_records)) reported_records <- NA_integer_

  list(
    headers = as.character(headers),
    rows = rows,
    total_pages = total_pages,
    reported_records = reported_records,
    result = result_value
  )
}

fetch_snapshot <- function(snapshot_attempt = NA_integer_) {
  run_time <- Sys.time()
  cookie_file <- tempfile("cnemc_cookie_jar_", fileext = ".txt")
  on.exit(unlink(cookie_file), add = TRUE)

  log_msg("Checking live nationwide surface-water endpoint")
  log_msg(
    "Request filter: AreaID='", AREA_ID, "'; RiverID='", RIVER_ID,
    "'; MNName='", MN_NAME, "'; PageSize=", PAGE_SIZE
  )

  # CNEMC currently requires this browser-like application initialization.
  # A cold getRealDatas request returns HTTP 200 text/plain with body "-1".
  warmup <- warm_cnemc_application(
    cookie_file = cookie_file,
    snapshot_attempt = snapshot_attempt
  )

  first <- fetch_page_raw(
    1L,
    snapshot_attempt = snapshot_attempt,
    cookie_file = cookie_file
  )
  first_parsed <- parse_api_page(
    first$body, 1L, response_meta = first, snapshot_attempt = snapshot_attempt
  )
  total_pages <- first_parsed$total_pages
  reported_records <- first_parsed$reported_records

  log_msg(
    "Endpoint reports ", total_pages, " page(s) at PageSize=", PAGE_SIZE,
    if (!is.na(reported_records)) paste0("; records=", reported_records) else ""
  )

  pages <- vector("list", total_pages)
  pages[[1]] <- first

  if (total_pages > 1L) {
    for (i in 2:total_pages) {
      if (PAGE_DELAY_SECONDS > 0) Sys.sleep(PAGE_DELAY_SECONDS)
      log_msg("Fetching page ", i, "/", total_pages)
      pages[[i]] <- fetch_page_raw(
        i,
        snapshot_attempt = snapshot_attempt,
        cookie_file = cookie_file
      )
    }
  }

  page_md5 <- vapply(pages, function(x) md5_raw(x$body), character(1))
  snapshot_md5 <- md5_text(paste(page_md5, collapse = "|"))

  list(
    collected_at = run_time,
    main_url = MAIN_URL,
    endpoint = ENDPOINT,
    request = list(
      AreaID = AREA_ID,
      RiverID = RIVER_ID,
      MNName = MN_NAME,
      PageSize = PAGE_SIZE,
      initialization = c("Main.html", "getArea_RiverDic", "getRealDatas")
    ),
    warmup = warmup,
    total_pages = total_pages,
    reported_records = reported_records,
    page_md5 = page_md5,
    snapshot_md5 = snapshot_md5,
    snapshot_attempt = snapshot_attempt,
    pages = pages
  )
}

# -----------------------------
# 5. Header and row normalization
# -----------------------------
html_to_text <- function(x) {
  x <- as.character(x)
  x <- gsub("(?i)<br\\s*/?>", " ", x, perl = TRUE)
  x <- gsub("<[^>]+>", "", x, perl = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&mu;", "µ", x, fixed = TRUE)
  x <- gsub("&sup3;", "³", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

indicator_name_from_header <- function(header_text, position) {
  h <- gsub("\\s+", "", header_text, perl = TRUE)

  # First five meanings are explicit in RealDatas.js through the rendering
  # order: area, river basin, monitoring section, monitoring time, WQ class.
  if (position == 1L) return("area")
  if (position == 2L) return("river_basin")
  if (position == 3L) return("monitoring_section")
  if (position == 4L) return("monitoring_time_raw")
  if (position == 5L) return("water_quality_class_code")

  # Conservative mapping of common automatic-monitoring parameters. Values
  # remain character strings exactly as published; numeric cleaning belongs
  # in a later analytical script.
  if (grepl("水温", h, fixed = TRUE)) return("water_temperature_c_raw")
  if (grepl("^pH|pH", h, ignore.case = TRUE)) return("ph_raw")
  if (grepl("溶解氧", h, fixed = TRUE)) return("dissolved_oxygen_mg_l_raw")
  if (grepl("电导率", h, fixed = TRUE)) return("conductivity_raw")
  if (grepl("浊度", h, fixed = TRUE)) return("turbidity_ntu_raw")
  if (grepl("高锰酸盐", h, fixed = TRUE)) return("permanganate_index_mg_l_raw")
  if (grepl("氨氮", h, fixed = TRUE)) return("ammonia_nitrogen_mg_l_raw")
  if (grepl("总磷", h, fixed = TRUE)) return("total_phosphorus_mg_l_raw")
  if (grepl("总氮", h, fixed = TRUE)) return("total_nitrogen_mg_l_raw")
  if (grepl("总有机碳|TOC", h, ignore.case = TRUE)) return("toc_mg_l_raw")
  if (grepl("叶绿素", h, fixed = TRUE)) return("chlorophyll_a_raw")
  if (grepl("藻密度", h, fixed = TRUE)) return("algal_density_raw")

  sprintf("parameter_%02d", position - 5L)
}

make_header_dictionary <- function(headers) {
  source_text <- html_to_text(headers)
  standardized <- vapply(
    seq_along(headers),
    function(i) indicator_name_from_header(source_text[[i]], i),
    character(1)
  )
  standardized <- make.unique(standardized, sep = "_")

  tibble::tibble(
    column_position = seq_along(headers),
    standardized_name = standardized,
    source_header_html = as.character(headers),
    source_header_text = source_text
  )
}

pad_row <- function(x, n) {
  vals <- unlist(x, recursive = TRUE, use.names = FALSE)
  vals <- as.character(vals)
  if (length(vals) < n) vals <- c(vals, rep(NA_character_, n - length(vals)))
  if (length(vals) > n) vals <- vals[seq_len(n)]
  vals
}

decode_water_class <- function(x) {
  key <- c("1" = "Ⅰ", "2" = "Ⅱ", "3" = "Ⅲ", "4" = "Ⅳ", "5" = "Ⅴ", "6" = "劣Ⅴ")
  original <- as.character(x)
  out <- unname(key[original])
  keep_original <- is.na(out) & !is.na(original) & nzchar(original)
  out[keep_original] <- original[keep_original]
  out
}

parse_snapshot <- function(bundle) {
  parsed <- lapply(
    bundle$pages,
    function(pg) parse_api_page(
      pg$body, pg$page_index, response_meta = pg,
      snapshot_attempt = bundle$snapshot_attempt
    )
  )

  headers <- parsed[[1]]$headers
  if (length(headers) == 0L) {
    save_failed_response(
      bundle$pages[[1]]$body, "schema_error", 1L, bundle$snapshot_attempt,
      bundle$pages[[1]]$status, bundle$pages[[1]]$content_type, bundle$pages[[1]]$server_date
    )
    cnemc_stop("schema_error", "API supplied no table headers")
  }

  # Verify that all pages describe the same table schema.
  same_headers <- vapply(
    parsed,
    function(x) {
      length(x$headers) == 0L || identical(as.character(x$headers), as.character(headers))
    },
    logical(1)
  )
  if (!all(same_headers)) {
    cnemc_stop(
      "schema_error",
      "Table headers changed between pages during the same collection run"
    )
  }

  dictionary <- make_header_dictionary(headers)
  n_cols <- nrow(dictionary)

  page_frames <- vector("list", length(parsed))
  for (p in seq_along(parsed)) {
    rows <- parsed[[p]]$rows
    if (is.null(rows) || length(rows) == 0L) {
      empty <- as.data.frame(matrix(nrow = 0L, ncol = n_cols), stringsAsFactors = FALSE)
      names(empty) <- dictionary$standardized_name
      empty$source_page <- integer(0)
      page_frames[[p]] <- empty
      next
    }

    mat <- do.call(rbind, lapply(rows, pad_row, n = n_cols))
    if (is.null(dim(mat))) mat <- matrix(mat, nrow = 1L)
    df <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
    names(df) <- dictionary$standardized_name
    df$source_page <- as.integer(p)
    page_frames[[p]] <- df
  }

  dat <- dplyr::bind_rows(page_frames)

  # The endpoint also exposes a `records` field. Its semantics may include
  # registered sections that are not represented in the current tbody, so a
  # difference is logged for provenance but is not automatically treated as a
  # failed snapshot.
  reported_records <- if (!is.null(bundle$reported_records)) {
    suppressWarnings(as.integer(bundle$reported_records[[1]]))
  } else {
    NA_integer_
  }
  if (!is.na(reported_records) && reported_records != nrow(dat)) {
    log_msg(
      "NOTICE CNEMC records/tbody difference: API records=", reported_records,
      "; returned tbody rows=", nrow(dat),
      "; difference=", reported_records - nrow(dat)
    )
  }

  # A nationwide request should never become canonical when a structurally
  # valid response contains zero data rows. Preserve the response and retry the
  # complete transaction rather than overwriting the current archive.
  if (nrow(dat) == 0L) {
    for (pg in bundle$pages) {
      save_failed_response(
        pg$body, "valid_zero_rows", pg$page_index, bundle$snapshot_attempt,
        pg$status, pg$content_type, pg$server_date
      )
    }
    cnemc_stop(
      "valid_zero_rows",
      "CNEMC API returned a successful response with zero data rows; treating this as a transient invalid snapshot"
    )
  }

  # Ensure all source columns are character even if JSON simplification varies.
  source_cols <- dictionary$standardized_name
  dat <- dat |>
    dplyr::mutate(dplyr::across(dplyr::all_of(source_cols), as.character))

  if ("water_quality_class_code" %in% names(dat)) {
    dat$water_quality_class <- decode_water_class(dat$water_quality_class_code)
  }

  dat$collected_at <- as.POSIXct(bundle$collected_at, tz = "Asia/Shanghai")
  dat$snapshot_md5 <- bundle$snapshot_md5

  # Build a year-safe observation identity. CNEMC publishes MM-DD HH:MM only,
  # so observation_datetime adds an explicitly inferred year for longitudinal
  # archiving while monitoring_time_raw remains untouched.
  dat <- recompute_observation_hashes(dat, source_cols)
  dat$collector_id <- COLLECTOR_ID
  dat$github_run_id <- if (nzchar(GITHUB_RUN_ID)) GITHUB_RUN_ID else NA_character_
  dat$github_run_attempt <- if (nzchar(GITHUB_RUN_ATTEMPT)) GITHUB_RUN_ATTEMPT else NA_character_
  dat$scraper_code_commit <- if (nzchar(GITHUB_SHA)) GITHUB_SHA else NA_character_

  list(data = tibble::as_tibble(dat), dictionary = dictionary)
}

collect_valid_snapshot <- function() {
  last_error <- NULL

  for (attempt in seq_len(SNAPSHOT_MAX_ATTEMPTS)) {
    if (attempt > 1L) {
      delay_index <- min(attempt - 1L, length(SNAPSHOT_RETRY_SECONDS))
      delay <- if (length(SNAPSHOT_RETRY_SECONDS) > 0L) {
        SNAPSHOT_RETRY_SECONDS[[delay_index]]
      } else {
        0
      }

      if (is.finite(delay) && delay > 0) {
        log_msg(
          "Retrying full CNEMC snapshot transaction after ", delay,
          " second(s); attempt ", attempt, "/", SNAPSHOT_MAX_ATTEMPTS
        )
        Sys.sleep(delay)
      } else {
        log_msg(
          "Retrying full CNEMC snapshot transaction immediately; attempt ",
          attempt, "/", SNAPSHOT_MAX_ATTEMPTS
        )
      }
    }

    attempt_result <- tryCatch(
      {
        bundle <- fetch_snapshot(snapshot_attempt = attempt)
        parsed <- parse_snapshot(bundle)

        log_msg(
          "Validated CNEMC snapshot: rows=", nrow(parsed$data),
          "; pages=", bundle$total_pages,
          "; attempt=", attempt, "/", SNAPSHOT_MAX_ATTEMPTS
        )

        list(ok = TRUE, bundle = bundle, parsed = parsed, error = NULL)
      },
      error = function(e) {
        list(ok = FALSE, bundle = NULL, parsed = NULL, error = e)
      }
    )

    if (isTRUE(attempt_result$ok)) return(attempt_result)

    last_error <- attempt_result$error
    failure_type <- classify_collection_error(last_error)
    append_failure_manifest(attempt, failure_type, conditionMessage(last_error))
    log_msg(
      "WARNING CNEMC snapshot attempt ", attempt, "/", SNAPSHOT_MAX_ATTEMPTS,
      " failed [", failure_type, "]: ", conditionMessage(last_error)
    )
  }

  cnemc_stop(
    classify_collection_error(last_error),
    paste0(
      "CNEMC snapshot failed after ", SNAPSHOT_MAX_ATTEMPTS,
      " full attempt(s): ", conditionMessage(last_error)
    )
  )
}

# -----------------------------
# 6. Source archive
# -----------------------------
archive_snapshot_if_changed <- function(bundle) {
  old_md5 <- NA_character_
  if (file.exists(current_meta_path)) {
    old_meta <- tryCatch(readRDS(current_meta_path), error = function(e) NULL)
    if (!is.null(old_meta$snapshot_md5)) old_md5 <- as.character(old_meta$snapshot_md5)
  }

  changed <- is.na(old_md5) || !identical(old_md5, bundle$snapshot_md5)

  if (changed) {
    stamp <- format(bundle$collected_at, "%Y%m%d_%H%M%S", tz = COLLECTOR_TZ)
    year_dir <- file.path(ARCHIVE_DIR, format(bundle$collected_at, "%Y", tz = COLLECTOR_TZ))
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    archive_path <- file.path(year_dir, sprintf("surfacewater_%s_raw.rds", stamp))
    saveRDS(bundle, archive_path, compress = "xz")
    log_msg(
      "Surface-water snapshot changed; md5=", bundle$snapshot_md5,
      "; archived as ", basename(archive_path)
    )
  } else {
    log_msg("Surface-water snapshot unchanged; md5=", bundle$snapshot_md5)
  }

  # Current source bundle is updated on every successful run so checked time
  # and server-response metadata remain current. Page bodies are raw vectors.
  saveRDS(bundle, current_raw_path, compress = "xz")
  saveRDS(
    list(
      checked_at = bundle$collected_at,
      snapshot_md5 = bundle$snapshot_md5,
      page_md5 = bundle$page_md5,
      total_pages = bundle$total_pages,
      page_size = PAGE_SIZE,
      changed = changed
    ),
    current_meta_path
  )

  changed
}

archive_area_river_dictionary <- function(raw_body) {
  if (is.null(raw_body) || length(raw_body) == 0L) {
    stop("Area-river dictionary body is empty")
  }

  new_md5 <- md5_raw(raw_body)
  old_md5 <- if (file.exists(area_river_path)) unname(tools::md5sum(area_river_path)) else NA_character_

  if (is.na(old_md5) || !identical(old_md5, new_md5)) {
    now <- Sys.time()
    stamp <- format(now, "%Y%m%d_%H%M%S", tz = COLLECTOR_TZ)
    year_dir <- file.path(ARCHIVE_DIR, format(now, "%Y", tz = COLLECTOR_TZ))
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    archive_path <- file.path(year_dir, sprintf("area_river_%s.json", stamp))
    write_raw_atomic(raw_body, archive_path)
    write_raw_atomic(raw_body, area_river_path)
    log_msg("Area-river dictionary changed; md5=", new_md5)
  } else {
    log_msg("Area-river dictionary unchanged; md5=", new_md5)
  }

  invisible(new_md5)
}

# -----------------------------
# 7. Processed current snapshot and cumulative history
# -----------------------------
write_current_outputs <- function(parsed) {
  dat <- parsed$data
  dict <- parsed$dictionary

  saveRDS(dat, current_rds_path, compress = "xz")
  readr::write_csv(dat, current_csv_path, na = "")
  readr::write_csv(dict, header_dictionary_path, na = "")

  log_msg("Current processed snapshot: ", nrow(dat), " rows")
  log_msg("Current RDS: ", current_rds_path)
  log_msg("Header dictionary: ", header_dictionary_path)
}

master_needs_hash_migration <- function(master) {
  required <- c(
    "observation_datetime", "observation_year_inferred",
    "observation_key_hash", "row_hash"
  )

  if (!all(required %in% names(master))) return(TRUE)
  if (any(is.na(master$observation_key_hash) | !nzchar(as.character(master$observation_key_hash)))) return(TRUE)
  if (any(is.na(master$row_hash) | !nzchar(as.character(master$row_hash)))) return(TRUE)
  FALSE
}

master_csv_is_due <- function(path, refresh_hours) {
  if (!file.exists(path)) return(TRUE)
  if (is.infinite(refresh_hours)) return(FALSE)
  if (refresh_hours <= 0) return(TRUE)

  info <- file.info(path)
  if (is.na(info$mtime)) return(TRUE)
  age_hours <- as.numeric(difftime(Sys.time(), info$mtime, units = "hours"))
  is.na(age_hours) || age_hours >= refresh_hours
}

set_revision_counts <- function(master, affected_keys = NULL, force_full = FALSE) {
  has_revision_count <- "revision_count" %in% names(master)

  if (force_full || !has_revision_count) {
    counts <- table(master$observation_key_hash, useNA = "no")
    master$revision_count <- as.integer(
      counts[match(master$observation_key_hash, names(counts))]
    )
    return(master)
  }

  master$revision_count <- as.integer(master$revision_count)

  if (is.null(affected_keys) || length(affected_keys) == 0L) {
    return(master)
  }

  affected_keys <- unique(as.character(affected_keys))
  affected <- master$observation_key_hash %in% affected_keys
  if (!any(affected)) return(master)

  counts <- table(master$observation_key_hash[affected], useNA = "no")
  master$revision_count[affected] <- as.integer(
    counts[match(master$observation_key_hash[affected], names(counts))]
  )
  master
}

update_cumulative_master <- function(snapshot_data, run_time, source_cols) {
  update_started <- Sys.time()

  # Count exact duplicate source rows within this snapshot rather than silently
  # discarding the information that duplicates occurred.
  snap_counts <- snapshot_data |>
    dplyr::count(.data$row_hash, name = "snapshot_occurrences")

  snap_unique <- snapshot_data |>
    dplyr::distinct(.data$row_hash, .keep_all = TRUE) |>
    dplyr::left_join(snap_counts, by = "row_hash") |>
    dplyr::mutate(
      first_seen = as.POSIXct(run_time, tz = "Asia/Shanghai"),
      last_seen = as.POSIXct(run_time, tz = "Asia/Shanghai"),
      times_seen = as.integer(.data$snapshot_occurrences)
    )

  full_revision_rebuild <- FALSE
  affected_revision_keys <- character()
  new_row_versions_n <- nrow(snap_unique)

  if (!file.exists(master_rds_path)) {
    master <- snap_unique
    full_revision_rebuild <- TRUE
    log_msg("Initializing cumulative CNEMC master from first processed snapshot")
  } else {
    read_started <- Sys.time()
    master <- readRDS(master_rds_path)
    log_msg(
      "Loaded cumulative CNEMC master: ", nrow(master), " rows in ",
      sprintf("%.2f", as.numeric(difftime(Sys.time(), read_started, units = "secs"))),
      " second(s)"
    )

    # The v1 -> v2 hash migration is expensive (~2 hashes x every historical
    # row), so perform it only when the archive actually lacks the v2 schema.
    # Previous versions recomputed these hashes on every 20-minute run.
    if (master_needs_hash_migration(master)) {
      migration_started <- Sys.time()
      log_msg("Migrating legacy cumulative hash schema once ...")
      master <- recompute_observation_hashes(master, source_cols)
      full_revision_rebuild <- TRUE
      log_msg(
        "Legacy hash migration complete in ",
        sprintf("%.2f", as.numeric(difftime(Sys.time(), migration_started, units = "secs"))),
        " second(s)"
      )
    }

    matched <- match(snap_unique$row_hash, master$row_hash)
    existing_idx <- which(!is.na(matched))
    new_idx <- which(is.na(matched))
    new_row_versions_n <- length(new_idx)

    if (length(existing_idx) > 0L) {
      mi <- matched[existing_idx]
      master$last_seen[mi] <- as.POSIXct(run_time, tz = "Asia/Shanghai")
      master$times_seen[mi] <- as.integer(master$times_seen[mi]) +
        as.integer(snap_unique$snapshot_occurrences[existing_idx])
    }

    if (length(new_idx) > 0L) {
      new_rows <- snap_unique[new_idx, , drop = FALSE]
      affected_revision_keys <- unique(new_rows$observation_key_hash)
      master <- dplyr::bind_rows(master, new_rows)
    }
  }

  # revision_count changes only when a new published row version is introduced.
  # Recount the full ~million-row archive only for first initialization/schema
  # migration; otherwise update just the observation keys touched by new rows.
  master <- set_revision_counts(
    master,
    affected_keys = affected_revision_keys,
    force_full = full_revision_rebuild
  )

  # Appended rows already preserve first-seen chronology, so a full-table sort
  # on every run is unnecessary. Avoiding it saves another O(n log n) pass.

  rds_started <- Sys.time()
  saveRDS(master, master_rds_path, compress = "gzip")
  log_msg(
    "Cumulative RDS updated with gzip compression in ",
    sprintf("%.2f", as.numeric(difftime(Sys.time(), rds_started, units = "secs"))),
    " second(s)"
  )

  if (master_csv_is_due(master_csv_path, MASTER_CSV_REFRESH_HOURS)) {
    csv_started <- Sys.time()
    readr::write_csv(master, master_csv_path, na = "")
    log_msg(
      "Cumulative CSV refreshed: ", master_csv_path, " in ",
      sprintf("%.2f", as.numeric(difftime(Sys.time(), csv_started, units = "secs"))),
      " second(s)"
    )
  } else {
    log_msg(
      "Cumulative CSV refresh skipped; interval=", MASTER_CSV_REFRESH_HOURS,
      " hour(s). RDS remains current. Set option ",
      "nmemc.surfacewater.master_csv_refresh_hours=0 to force every run."
    )
  }

  duplicate_source_n <- sum(snap_counts$snapshot_occurrences - 1L)
  revised_keys_n <- sum(master$revision_count > 1L, na.rm = TRUE)

  log_msg(
    "Cumulative archive: ", nrow(master), " unique published row version(s); ",
    "new row versions this run: ", new_row_versions_n, "; ",
    "exact duplicate rows in current snapshot: ", duplicate_source_n
  )
  log_msg("Rows belonging to revised observation keys: ", revised_keys_n)
  log_msg("Cumulative RDS: ", master_rds_path)
  log_msg(
    "Cumulative archive update time: ",
    sprintf("%.2f", as.numeric(difftime(Sys.time(), update_started, units = "secs"))),
    " second(s)"
  )

  invisible(master)
}

append_run_manifest <- function(bundle, parsed, changed) {
  entry <- tibble::tibble(
    collector_id = COLLECTOR_ID,
    github_run_id = if (nzchar(GITHUB_RUN_ID)) GITHUB_RUN_ID else NA_character_,
    github_run_attempt = if (nzchar(GITHUB_RUN_ATTEMPT)) GITHUB_RUN_ATTEMPT else NA_character_,
    scraper_code_commit = if (nzchar(GITHUB_SHA)) GITHUB_SHA else NA_character_,
    collected_at = as.POSIXct(bundle$collected_at, tz = COLLECTOR_TZ),
    snapshot_md5 = bundle$snapshot_md5,
    changed = isTRUE(changed),
    total_pages = as.integer(bundle$total_pages),
    page_size = as.integer(PAGE_SIZE),
    reported_records = if (!is.null(bundle$reported_records)) as.integer(bundle$reported_records) else NA_integer_,
    rows = nrow(parsed$data),
    unique_row_hashes = dplyr::n_distinct(parsed$data$row_hash),
    area_id = AREA_ID,
    river_id = RIVER_ID,
    mn_name = MN_NAME,
    endpoint = ENDPOINT
  )

  if (file.exists(run_manifest_path)) {
    old <- suppressWarnings(readr::read_csv(run_manifest_path, show_col_types = FALSE))
    manifest <- dplyr::bind_rows(old, entry)
  } else {
    manifest <- entry
  }

  readr::write_csv(manifest, run_manifest_path, na = "")
}

# -----------------------------
# 8. Run
# -----------------------------
log_msg("START CNEMC surface-water archive")
log_msg("Source page: ", MAIN_URL)
log_msg(
  "Retry budget: HTTP timeout=", HTTP_TIMEOUT_SECONDS, "s; HTTP tries=",
  HTTP_MAX_TRIES, "; snapshot attempts=", SNAPSHOT_MAX_ATTEMPTS,
  "; snapshot waits=", paste(SNAPSHOT_RETRY_SECONDS, collapse = ","), "s"
)
log_msg("Request initialization: Main.html -> getArea_RiverDic -> getRealDatas")

collection <- tryCatch(
  collect_valid_snapshot(),
  error = function(e) {
    log_msg(
      "FATAL CNEMC collection error [", classify_collection_error(e), "]: ",
      conditionMessage(e)
    )
    stop(e)
  }
)

bundle <- collection$bundle
parsed <- collection$parsed

# Archive only after fetch + parse + zero-row validation have succeeded.
# This prevents transient empty/invalid responses from becoming canonical raw
# snapshots or replacing the current source bundle.
changed <- archive_snapshot_if_changed(bundle)
write_current_outputs(parsed)
update_cumulative_master(
  parsed$data,
  bundle$collected_at,
  parsed$dictionary$standardized_name
)
append_run_manifest(bundle, parsed, changed)

# The successful snapshot attempt already fetched the dictionary during the
# required application warm-up. Archive that exact response instead of making
# another network request after collection.
tryCatch(
  archive_area_river_dictionary(bundle$warmup$area_river_body),
  error = function(e) log_msg("WARNING area-river dictionary archive failed: ", conditionMessage(e))
)

log_msg(
  "END CNEMC surface-water archive; rows=", nrow(parsed$data),
  "; pages=", bundle$total_pages,
  "; snapshot_md5=", bundle$snapshot_md5
)