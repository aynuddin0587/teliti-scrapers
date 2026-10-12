# ============================================================================
# CNEMC nationwide surface-water archive - portable GitHub / Windows edition
# ============================================================================
# Built from the supplied 02_nmemc_surfacewater_archive.R (August 2026).
# Revised 2026-10-12: resilient request transaction, strict collection QA,
# failure reporting, text-safe GitHub identifiers, cumulative rows and deltas.
#
# Input: public CNEMC page and Ajax/Publish.ashx endpoint.
# Published measurements and qualifiers are kept as source text.
#
# GitHub Actions: Rscript 02_nmemc_surfacewater_archive.R
# Local Windows:  Rscript 02_nmemc_surfacewater_archive.R
#
# Output root selection, in order:
#   1. CNEMC_SURFACE_DIR environment variable
#   2. $GITHUB_WORKSPACE/cnemc_surfacewater (GitHub-hosted workflow)
#   3. D:/# R Project/penelitian/nmemc/data/surfacewater (local legacy layout)
#   4. ./cnemc_surfacewater (portable fallback)
#
# Outputs (within the selected root):
#   source/surfacewater_current_raw.rds      latest complete raw bundle
#   source/surfacewater_current_meta.rds
#   source/area_river_current.json
#   archive/YYYY/                        changed raw response archives
#   processed/nmemc_surfacewater_current.{rds,csv.gz}
#   processed/nmemc_surfacewater_observations.{rds,csv.gz}
#   processed/nmemc_surfacewater_header_dictionary.csv
#   processed/nmemc_surfacewater_run_manifest.csv
#   deltas/YYYY/*.csv.gz                  new source row versions per run
#   manifests/delta_manifest.csv          incremental archive record
#   manifests/attempt_manifest.csv        successful / failed attempts
#   log/nmemc_surfacewater_YYYYMMDD.log
#
# Importantly: a failed fetch never overwrites a valid source snapshot.
# A website outage still produces a nonzero Rscript exit code after retries;
# failure logging is best effort and cannot hide the original network error.
#
# Note: the uploaded original is older than the GitHub code shown in your logs.
# This is a self-contained replacement, not a byte-identical update of that
# missing GitHub version. Back up the active file and existing archive first.
# ============================================================================
options(stringsAsFactors = FALSE, timeout = 120, httr2_progress = FALSE)

# -----------------------------
# 1. Configuration
# -----------------------------
env_int <- function(name, default, minimum = 1L) {
  x <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
  if (is.na(x) || x < minimum) as.integer(default) else x
}
env_num <- function(name, default, minimum = 0) {
  x <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
  if (is.na(x) || x < minimum) as.numeric(default) else x
}
env_flag <- function(name, default = FALSE) {
  x <- tolower(trimws(Sys.getenv(name, if (default) "true" else "false")))
  x %in% c("1", "true", "yes", "on")
}

CN_TZ <- "Asia/Shanghai"
MAIN_URL <- "https://szzdjc.cnemc.cn:8070/GJZ/Business/Publish/Main.html"
ENDPOINT <- "https://szzdjc.cnemc.cn:8070/GJZ/Ajax/Publish.ashx"
ORIGIN <- "https://szzdjc.cnemc.cn:8070"

workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
local_dir <- "D:/# R Project/penelitian/nmemc"
SURFACE_DIR <- Sys.getenv("CNEMC_SURFACE_DIR", unset = "")
if (!nzchar(SURFACE_DIR)) {
  if (nzchar(workspace)) {
    SURFACE_DIR <- file.path(workspace, "cnemc_surfacewater")
  } else if (dir.exists(local_dir)) {
    SURFACE_DIR <- file.path(local_dir, "data", "surfacewater")
  } else {
    SURFACE_DIR <- file.path(getwd(), "cnemc_surfacewater")
  }
}
BASE_DIR <- dirname(SURFACE_DIR)
SOURCE_DIR <- file.path(SURFACE_DIR, "source")
ARCHIVE_DIR <- file.path(SURFACE_DIR, "archive")
PROCESSED_DIR <- file.path(SURFACE_DIR, "processed")
MANIFEST_DIR <- file.path(SURFACE_DIR, "manifests")
DELTA_DIR <- file.path(SURFACE_DIR, "deltas")
LOG_DIR <- if (nzchar(workspace)) file.path(SURFACE_DIR, "log") else {
  if (dir.exists(local_dir) && identical(normalizePath(SURFACE_DIR, mustWork = FALSE),
                                        normalizePath(file.path(local_dir, "data", "surfacewater"), mustWork = FALSE))) {
    file.path(local_dir, "log")
  } else file.path(SURFACE_DIR, "log")
}

for (d in c(SURFACE_DIR, SOURCE_DIR, ARCHIVE_DIR, PROCESSED_DIR,
            MANIFEST_DIR, DELTA_DIR, LOG_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

PAGE_SIZE <- env_int("CNEMC_PAGE_SIZE", getOption("nmemc.surfacewater.page_size", 2000L))
AREA_ID <- Sys.getenv("CNEMC_AREA_ID", unset = as.character(getOption("nmemc.surfacewater.area_id", "")))
RIVER_ID <- Sys.getenv("CNEMC_RIVER_ID", unset = as.character(getOption("nmemc.surfacewater.river_id", "")))
MN_NAME <- Sys.getenv("CNEMC_MN_NAME", unset = as.character(getOption("nmemc.surfacewater.mn_name", "")))
PAGE_DELAY_SECONDS <- env_num("CNEMC_PAGE_DELAY_SEC", getOption("nmemc.surfacewater.page_delay", 0.25))
MAX_PAGES <- env_int("CNEMC_MAX_PAGES", 50L)
HTTP_TIMEOUT_SEC <- env_int("CNEMC_HTTP_TIMEOUT_SEC", 30L)
HTTP_TRIES <- env_int("CNEMC_HTTP_TRIES", 2L)
SNAPSHOT_ATTEMPTS <- env_int("CNEMC_SNAPSHOT_ATTEMPTS", 3L)
SKIP_WARMUP <- env_flag("CNEMC_SKIP_WARMUP", FALSE)
ALLOW_DIRECT_FALLBACK <- env_flag("CNEMC_ALLOW_DIRECT_FALLBACK", TRUE)

GITHUB_RUN_ID <- as.character(Sys.getenv("GITHUB_RUN_ID", unset = ""))
GITHUB_RUN_ATTEMPT <- as.character(Sys.getenv("GITHUB_RUN_ATTEMPT", unset = ""))
RUN_STARTED_AT <- Sys.time()
RUN_STAMP <- format(RUN_STARTED_AT, "%Y%m%d_%H%M%S", tz = CN_TZ)
COLLECTION_KEY <- if (nzchar(GITHUB_RUN_ID)) {
  paste0(GITHUB_RUN_ID, "-", if (nzchar(GITHUB_RUN_ATTEMPT)) GITHUB_RUN_ATTEMPT else "1")
} else {
  paste0("local-", RUN_STAMP, "-", Sys.getpid())
}

log_file <- file.path(
  LOG_DIR, sprintf("nmemc_surfacewater_%s.log", format(Sys.Date(), "%Y%m%d"))
)
log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = CN_TZ),
                 " | ", paste0(..., collapse = ""))
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


# Manifest handling deliberately reads *every* CSV field as character.
# GitHub identifiers are labels, not numbers. This fixes the recurrent
# bind_rows(<double>, <character>) failure without losing identifier digits.
atomic_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ext <- if (grepl("\\.gz$", path)) ".csv.gz" else ".csv"
  tmp <- tempfile(pattern = ".cnemc_write_", tmpdir = dirname(path), fileext = ext)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  readr::write_csv(data, tmp, na = "")
  if (!file.rename(tmp, path)) {
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not write: ", path)
  }
  invisible(path)
}
atomic_rds <- function(data, path, compress = "xz") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = ".cnemc_write_", tmpdir = dirname(path), fileext = ".rds")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  saveRDS(data, tmp, compress = compress, version = 3)
  if (!file.rename(tmp, path)) {
    if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not write: ", path)
  }
  invisible(path)
}

append_csv_manifest <- function(path, entry) {
  stopifnot(is.data.frame(entry))
  # Cast all new values, not just github_run_id, to a stable schema.
  entry[] <- lapply(entry, as.character)
  if (file.exists(path) && isTRUE(file.info(path)$size > 0)) {
    old <- suppressWarnings(readr::read_csv(
      path, show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character()),
      name_repair = "minimal"
    ))
    old[] <- lapply(old, as.character)
    combined <- dplyr::bind_rows(old, entry)
  } else {
    combined <- entry
  }
  atomic_csv(combined, path)
  invisible(combined)
}

attempt_manifest_path <- file.path(MANIFEST_DIR, "attempt_manifest.csv")
delta_manifest_path <- file.path(MANIFEST_DIR, "delta_manifest.csv")

classify_collection_error <- function(e) {
  message <- tolower(conditionMessage(e))
  if (grepl("timeout|timed out", message)) return("transport_timeout")
  if (grepl("couldn.t connect|failed to connect|could not resolve|name resolution|connection refused|failed to perform http request", message)) {
    return("transport_failure")
  }
  if (grepl("http [0-9]{3}|status [0-9]{3}", message)) return("http_error")
  if (grepl("json|schema|page|response|invalid", message)) return("invalid_snapshot")
  "unclassified_error"
}

record_attempt <- function(attempt, status, error_class = "", message = "",
                           snapshot_md5 = "", rows = NA_integer_) {
  entry <- tibble::tibble(
    collection_key = COLLECTION_KEY,
    github_run_id = GITHUB_RUN_ID,
    github_run_attempt = GITHUB_RUN_ATTEMPT,
    recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S%z", tz = CN_TZ),
    attempt = as.integer(attempt),
    status = status,
    error_class = error_class,
    error_message = substr(message, 1L, 3000L),
    snapshot_md5 = snapshot_md5,
    rows = rows
  )
  tryCatch(append_csv_manifest(attempt_manifest_path, entry),
           error = function(e) {
             # Never transform an initial network failure into a CSV type error.
             log_msg("WARNING could not record attempt manifest: ", conditionMessage(e))
             invisible(NULL)
           })
}

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
base_request <- function(url = ENDPOINT, html = FALSE) {
  req <- httr2::request(url) |>
    httr2::req_user_agent("Mozilla/5.0 (compatible; academic-research/CNEMC-surface-water-archive)") |>
    httr2::req_timeout(HTTP_TIMEOUT_SEC) |>
    httr2::req_retry(max_tries = HTTP_TRIES, retry_on_failure = TRUE)

  if (html) {
    return(httr2::req_headers(req, Accept = "text/html,application/xhtml+xml,*/*;q=0.8"))
  }
  httr2::req_headers(
    req,
    Referer = MAIN_URL,
    Origin = ORIGIN,
    Accept = "application/json, text/javascript, */*; q=0.01",
    `X-Requested-With` = "XMLHttpRequest"
  )
}

fetch_area_river_raw <- function() {
  response <- base_request() |>
    httr2::req_body_form(action = "getArea_RiverDic") |>
    httr2::req_perform()
  status <- httr2::resp_status(response)
  if (status < 200L || status >= 300L) {
    stop("Area-river dictionary returned HTTP ", status, call. = FALSE)
  }
  body <- httr2::resp_body_raw(response)
  if (length(body) == 0L) stop("Area-river dictionary was empty", call. = FALSE)
  body
}

initialize_application <- function() {
  # Some environments require visiting Main.html first, but if Main.html is
  # blocked while the public API remains accessible we allow direct fallback.
  if (!SKIP_WARMUP) {
    log_msg("Initializing CNEMC application: Main.html -> getArea_RiverDic")
    warm <- tryCatch(
      httr2::req_perform(base_request(MAIN_URL, html = TRUE)),
      error = function(e) e
    )
    if (inherits(warm, "error")) {
      log_msg("WARNING CNEMC Main.html warm-up failed: ", conditionMessage(warm))
      if (!ALLOW_DIRECT_FALLBACK) stop(warm)
      log_msg("Attempting direct public API fallback despite warm-up failure")
    } else {
      status <- httr2::resp_status(warm)
      if (status < 200L || status >= 300L) {
        if (!ALLOW_DIRECT_FALLBACK) stop("Main.html returned HTTP ", status)
        log_msg("WARNING Main.html returned HTTP ", status, "; attempting API fallback")
      }
    }
  } else {
    log_msg("CNEMC_SKIP_WARMUP=true; using public API directly")
  }

  # Useful metadata; failure must not prevent a complete data collection.
  tryCatch(fetch_area_river_raw(), error = function(e) {
    log_msg("WARNING getArea_RiverDic request failed: ", conditionMessage(e))
    NULL
  })
}

fetch_page_raw <- function(page_index, page_size = PAGE_SIZE) {
  req <- base_request() |>
    httr2::req_body_form(
      AreaID = AREA_ID,
      RiverID = RIVER_ID,
      MNName = MN_NAME,
      PageIndex = as.character(page_index),
      PageSize = as.character(page_size),
      action = "getRealDatas"
    )
  response <- httr2::req_perform(req)
  status <- httr2::resp_status(response)
  if (status < 200L || status >= 300L) {
    stop("HTTP ", status, " while retrieving page ", page_index, call. = FALSE)
  }
  body <- httr2::resp_body_raw(response)
  if (length(body) == 0L) stop("Empty response on page ", page_index)
  list(
    page_index = as.integer(page_index),
    status = status,
    body = body,
    content_type = safe_chr(httr2::resp_header(response, "content-type")),
    server_date = safe_chr(httr2::resp_header(response, "date"))
  )
}

parse_api_page <- function(raw_body, page_index = NA_integer_) {
  txt <- raw_to_utf8(raw_body)

  out <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) {
      stop("Could not parse API JSON on page ", page_index, ": ", conditionMessage(e))
    }
  )

  if (!is.list(out)) stop("Unexpected API response type on page ", page_index)

  # The public JavaScript tests data.result before using the response.
  result_value <- out$result
  result_ok <- !is.null(result_value) && length(result_value) > 0L &&
    !(tolower(as.character(result_value[[1L]])) %in% c("0", "false", "no"))

  if (!result_ok) {
    stop("API returned result=0/false on page ", page_index)
  }

  headers <- unlist(out$thead, use.names = FALSE)
  rows <- out$tbody
  total_raw <- unlist(out$total, use.names = FALSE)
  total_pages <- if (length(total_raw)) {
    suppressWarnings(as.integer(total_raw[[1L]]))
  } else NA_integer_
  if (is.na(total_pages) || total_pages < 1L) total_pages <- 1L

  list(
    headers = as.character(headers),
    rows = rows,
    total_pages = total_pages,
    result = result_value
  )
}

fetch_snapshot <- function() {
  run_time <- Sys.time()
  log_msg("Checking live nationwide surface-water endpoint")
  log_msg(
    "Request filter: AreaID='", AREA_ID, "'; RiverID='", RIVER_ID,
    "'; MNName='", MN_NAME, "'; PageSize=", PAGE_SIZE
  )

  dictionary_raw <- initialize_application()
  first <- fetch_page_raw(1L)
  first_parsed <- parse_api_page(first$body, 1L)
  total_pages <- first_parsed$total_pages
  if (total_pages > MAX_PAGES) {
    stop("Refusing ", total_pages, " backend pages (MAX_PAGES=", MAX_PAGES, ")")
  }
  if (length(first_parsed$headers) == 0L) stop("CNEMC returned no table headers")

  log_msg("Endpoint reports ", total_pages, " page(s) at PageSize=", PAGE_SIZE)
  pages <- vector("list", total_pages)
  pages[[1]] <- first

  if (total_pages > 1L) {
    for (i in seq.int(2L, total_pages)) {
      if (PAGE_DELAY_SECONDS > 0) Sys.sleep(PAGE_DELAY_SECONDS)
      log_msg("Fetching page ", i, "/", total_pages)
      page <- fetch_page_raw(i)
      check <- parse_api_page(page$body, i)
      if (check$total_pages != total_pages) {
        stop("CNEMC page count changed during snapshot; rejecting incomplete transaction")
      }
      if (length(check$headers) > 0L &&
          !identical(as.character(check$headers), as.character(first_parsed$headers))) {
        stop("CNEMC column headers changed during snapshot")
      }
      pages[[i]] <- page
    }
  }

  page_md5 <- vapply(pages, function(x) md5_raw(x$body), character(1))
  snapshot_md5 <- md5_text(paste(page_md5, collapse = "|"))
  list(
    collected_at = run_time,
    main_url = MAIN_URL,
    endpoint = ENDPOINT,
    request = list(AreaID = AREA_ID, RiverID = RIVER_ID,
                   MNName = MN_NAME, PageSize = PAGE_SIZE),
    total_pages = total_pages,
    page_md5 = page_md5,
    snapshot_md5 = snapshot_md5,
    pages = pages,
    area_river_raw = dictionary_raw
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
  parsed <- lapply(bundle$pages, function(pg) parse_api_page(pg$body, pg$page_index))

  headers <- parsed[[1]]$headers
  if (length(headers) == 0L) stop("API supplied no table headers")

  # Verify that all pages describe the same table schema.
  same_headers <- vapply(
    parsed,
    function(x) {
      length(x$headers) == 0L || identical(as.character(x$headers), as.character(headers))
    },
    logical(1)
  )
  if (!all(same_headers)) {
    stop("Table headers changed between pages during the same collection run")
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

  list(data = tibble::as_tibble(dat), dictionary = dictionary)
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
    stamp <- format(bundle$collected_at, "%Y%m%d_%H%M%S", tz = CN_TZ)
    year_dir <- file.path(ARCHIVE_DIR, format(bundle$collected_at, "%Y", tz = CN_TZ))
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    archive_path <- file.path(year_dir, sprintf("surfacewater_%s_raw.rds", stamp))
    atomic_rds(bundle, archive_path)
    log_msg(
      "Surface-water snapshot changed; md5=", bundle$snapshot_md5,
      "; archived as ", basename(archive_path)
    )
  } else {
    log_msg("Surface-water snapshot unchanged; md5=", bundle$snapshot_md5)
  }

  # Current source bundle is updated on every successful run so checked time
  # and server-response metadata remain current. Page bodies are raw vectors.
  atomic_rds(bundle, current_raw_path)
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

fetch_area_river_dictionary <- function(raw_body = NULL) {
  if (is.null(raw_body)) raw_body <- fetch_area_river_raw()
  new_md5 <- md5_raw(raw_body)
  old_md5 <- if (file.exists(area_river_path)) unname(tools::md5sum(area_river_path)) else NA_character_

  if (is.na(old_md5) || !identical(old_md5, new_md5)) {
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    year_dir <- file.path(ARCHIVE_DIR, format(Sys.Date(), "%Y"))
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

  atomic_rds(dat, current_rds_path)
  atomic_csv(dat, current_csv_path)
  atomic_csv(dict, header_dictionary_path)

  log_msg("Current processed snapshot: ", nrow(dat), " rows")
  log_msg("Current RDS: ", current_rds_path)
  log_msg("Header dictionary: ", header_dictionary_path)
}

update_cumulative_master <- function(snapshot_data, run_time, source_cols) {
  snap_counts <- snapshot_data |>
    dplyr::count(.data$row_hash, name = "snapshot_occurrences")

  snap_unique <- snapshot_data |>
    dplyr::distinct(.data$row_hash, .keep_all = TRUE) |>
    dplyr::left_join(snap_counts, by = "row_hash") |>
    dplyr::mutate(
      first_seen = as.POSIXct(run_time, tz = CN_TZ),
      last_seen = as.POSIXct(run_time, tz = CN_TZ),
      times_seen = as.integer(.data$snapshot_occurrences)
    )

  had_master <- file.exists(master_rds_path)
  if (!had_master) {
    master <- snap_unique
    new_versions <- snap_unique
  } else {
    master <- readRDS(master_rds_path)
    if (!inherits(master, "data.frame")) stop("Existing master RDS is not tabular")
    # The original archive already stores year-safe row hashes. Preserve them
    # rather than recomputing with a possibly changed dynamic header schema.
    if (!all(c("row_hash", "observation_key_hash", "observation_datetime") %in% names(master)) ||
        anyNA(master$row_hash) || anyNA(master$observation_key_hash)) {
      master <- recompute_observation_hashes(master, source_cols)
    }
    matched <- match(snap_unique$row_hash, master$row_hash)
    existing_idx <- which(!is.na(matched))
    new_idx <- which(is.na(matched))
    new_versions <- snap_unique[new_idx, , drop = FALSE]

    if (length(existing_idx) > 0L) {
      mi <- matched[existing_idx]
      master$last_seen[mi] <- as.POSIXct(run_time, tz = CN_TZ)
      if (!"times_seen" %in% names(master)) master$times_seen <- rep(1L, nrow(master))
      master$times_seen[mi] <- as.integer(master$times_seen[mi]) +
        as.integer(snap_unique$snapshot_occurrences[existing_idx])
    }
    if (length(new_idx) > 0L) {
      # Dynamic public headers may appear or disappear across runs. Values for
      # source indicators must always remain character, never inferred numeric.
      source_shared <- intersect(source_cols, names(master))
      for (nm in source_shared) master[[nm]] <- as.character(master[[nm]])
      master <- dplyr::bind_rows(master, new_versions)
    }
  }

  revision_summary <- master |>
    dplyr::count(.data$observation_key_hash, name = "revision_count")
  master <- master |>
    dplyr::select(-dplyr::any_of("revision_count")) |>
    dplyr::left_join(revision_summary, by = "observation_key_hash") |>
    dplyr::arrange(.data$first_seen, .data$area, .data$monitoring_section)

  # Persistence happens *after* delta creation. If delta writing fails,
  # do not mark new row versions as archived in the master index.
  duplicate_counts <- snapshot_data |> dplyr::count(.data$row_hash, name = "n")
  duplicate_source_n <- sum(duplicate_counts$n - 1L)
  revised_keys_n <- sum(master$revision_count > 1L, na.rm = TRUE)
  log_msg("Cumulative archive: ", nrow(master), " unique published row version(s); ",
          "new versions this run: ", nrow(new_versions),
          "; exact duplicates in current snapshot: ", duplicate_source_n)
  log_msg("Rows belonging to revised observation keys: ", revised_keys_n)
  list(master = master, new_versions = new_versions, bootstrap = !had_master)
}

persist_cumulative_master <- function(master) {
  atomic_rds(master, master_rds_path)
  atomic_csv(master, master_csv_path)
  log_msg("Cumulative master persisted: ", nrow(master), " unique versions")
  invisible(master)
}

save_delta_and_manifest <- function(bundle, parsed, update_result) {
  new_versions <- update_result$new_versions
  delta_rel <- ""
  if (nrow(new_versions) > 0L) {
    subdir <- file.path(DELTA_DIR, format(bundle$collected_at, "%Y", tz = CN_TZ))
    dir.create(subdir, recursive = TRUE, showWarnings = FALSE)
    safe_key <- gsub("[^A-Za-z0-9_-]", "_", COLLECTION_KEY)
    file_name <- paste0("surfacewater_delta_", RUN_STAMP, "_", safe_key, ".csv.gz")
    delta_path <- file.path(subdir, file_name)
    atomic_csv(new_versions, delta_path)
    delta_rel <- file.path("deltas", format(bundle$collected_at, "%Y", tz = CN_TZ), file_name)
    log_msg("New source row versions saved: ", delta_rel)
  }

  entry <- tibble::tibble(
    collection_key = COLLECTION_KEY,
    collected_at = format(bundle$collected_at, "%Y-%m-%d %H:%M:%S%z", tz = CN_TZ),
    snapshot_md5 = bundle$snapshot_md5,
    retention_bucket = format(bundle$collected_at, "%Y%m%d_%H", tz = CN_TZ),
    delta_bootstrap = update_result$bootstrap,
    # No historical external index is imported by this standalone edition.
    historic_index_seeded = 0L,
    new_row_versions = nrow(new_versions),
    delta_file = delta_rel,
    total_rows = nrow(parsed$data),
    github_run_id = GITHUB_RUN_ID,
    github_run_attempt = GITHUB_RUN_ATTEMPT
  )
  append_csv_manifest(delta_manifest_path, entry)
  invisible(delta_rel)
}

append_run_manifest <- function(bundle, parsed, changed, n_new_versions) {
  entry <- tibble::tibble(
    collection_key = COLLECTION_KEY,
    github_run_id = GITHUB_RUN_ID,
    github_run_attempt = GITHUB_RUN_ATTEMPT,
    collected_at = format(bundle$collected_at, "%Y-%m-%d %H:%M:%S%z", tz = CN_TZ),
    snapshot_md5 = bundle$snapshot_md5,
    changed = isTRUE(changed),
    total_pages = as.integer(bundle$total_pages),
    page_size = as.integer(PAGE_SIZE),
    rows = nrow(parsed$data),
    unique_row_hashes = dplyr::n_distinct(parsed$data$row_hash),
    new_row_versions = n_new_versions,
    area_id = AREA_ID,
    river_id = RIVER_ID,
    mn_name = MN_NAME,
    endpoint = ENDPOINT
  )
  append_csv_manifest(run_manifest_path, entry)
}

# -----------------------------
# 8. Run: retry a whole snapshot transaction, not partial pages
# -----------------------------
log_msg("START CNEMC surface-water archive")
log_msg("Source page: ", MAIN_URL)
log_msg("Output root: ", SURFACE_DIR)
log_msg("Collection key: ", COLLECTION_KEY)
log_msg("Retry budget: HTTP timeout=", HTTP_TIMEOUT_SEC, "s; HTTP tries=", HTTP_TRIES,
        "; snapshot attempts=", SNAPSHOT_ATTEMPTS)
log_msg("Request initialization: Main.html -> getArea_RiverDic -> getRealDatas")

bundle <- NULL
parsed <- NULL
last_collection_error <- NULL

for (attempt in seq_len(SNAPSHOT_ATTEMPTS)) {
  result <- tryCatch({
    candidate <- fetch_snapshot()
    candidate_parsed <- parse_snapshot(candidate)
    if (nrow(candidate_parsed$data) < 1L) {
      stop("CNEMC snapshot contained zero monitoring rows; preserving prior archive")
    }
    list(bundle = candidate, parsed = candidate_parsed)
  }, error = function(e) e)

  if (!inherits(result, "error")) {
    bundle <- result$bundle
    parsed <- result$parsed
    break
  }

  last_collection_error <- result
  error_class <- classify_collection_error(result)
  log_msg("WARNING CNEMC snapshot attempt ", attempt, "/", SNAPSHOT_ATTEMPTS,
          " failed [", error_class, "]: ", conditionMessage(result))
  record_attempt(attempt, "failed", error_class, conditionMessage(result))

  if (attempt < SNAPSHOT_ATTEMPTS) {
    wait <- if (attempt == 1L) 10 else 30
    log_msg("Retrying complete CNEMC transaction after ", wait,
            " seconds; next attempt ", attempt + 1L, "/", SNAPSHOT_ATTEMPTS)
    Sys.sleep(wait)
  }
}

if (is.null(bundle)) {
  error_class <- classify_collection_error(last_collection_error)
  message <- conditionMessage(last_collection_error)
  log_msg("FATAL CNEMC collection error [", error_class, "]: ", message)
  stop(paste0("CNEMC collection failed after ", SNAPSHOT_ATTEMPTS,
              " complete attempts [", error_class, "]: ", message), call. = FALSE)
}

# All collection and parsing QA passed. Only now may the canonical data change.
tryCatch({
  changed <- archive_snapshot_if_changed(bundle)
  write_current_outputs(parsed)
  update <- update_cumulative_master(
    parsed$data, bundle$collected_at, parsed$dictionary$standardized_name
  )
  save_delta_and_manifest(bundle, parsed, update)
  persist_cumulative_master(update$master)
  append_run_manifest(bundle, parsed, changed, nrow(update$new_versions))

  # Metadata dictionary is noncritical to the measurements archive.
  tryCatch(
    fetch_area_river_dictionary(bundle$area_river_raw),
    error = function(e) log_msg("WARNING dictionary archival failed: ", conditionMessage(e))
  )

  record_attempt(0L, "success", snapshot_md5 = bundle$snapshot_md5,
                 rows = nrow(parsed$data))
  log_msg("END CNEMC surface-water archive; rows=", nrow(parsed$data),
          "; new row versions=", nrow(update$new_versions),
          "; pages=", bundle$total_pages,
          "; snapshot_md5=", bundle$snapshot_md5)
}, error = function(e) {
  error_class <- classify_collection_error(e)
  log_msg("FATAL CNEMC archival error [", error_class, "]: ", conditionMessage(e))
  record_attempt(0L, "archive_failed", error_class, conditionMessage(e),
                 snapshot_md5 = bundle$snapshot_md5, rows = nrow(parsed$data))
  stop(e)
})
