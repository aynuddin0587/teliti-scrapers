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

BASE_DIR <- file.path(
  TELITI_DATA_ROOT,
  "nmemc"
)

# Collector/source time and provenance
COLLECTOR_TZ <- Sys.getenv(
  "TELITI_TIMEZONE",
  unset = "Asia/Taipei"
)

SOURCE_TZ <- "Asia/Shanghai"

COLLECTOR_ID <- Sys.getenv(
  "TELITI_COLLECTOR_ID",
  unset = "local_pc"
)

GITHUB_RUN_ID <- Sys.getenv("GITHUB_RUN_ID", unset = "")
GITHUB_RUN_ATTEMPT <- Sys.getenv("GITHUB_RUN_ATTEMPT", unset = "")
GITHUB_SHA <- Sys.getenv("GITHUB_SHA", unset = "")

MAIN_URL <- "https://szzdjc.cnemc.cn:8070/GJZ/Business/Publish/Main.html"
ENDPOINT <- "https://szzdjc.cnemc.cn:8070/GJZ/Ajax/Publish.ashx"
ORIGIN   <- "https://szzdjc.cnemc.cn:8070"

SURFACE_DIR   <- file.path(BASE_DIR, "data", "surfacewater")
SOURCE_DIR    <- file.path(SURFACE_DIR, "source")
ARCHIVE_DIR   <- file.path(SURFACE_DIR, "archive")
PROCESSED_DIR <- file.path(SURFACE_DIR, "processed")
LOG_DIR       <- file.path(BASE_DIR, "log")
SCRIPT_DIR    <- file.path(BASE_DIR, "script")

for (d in c(SURFACE_DIR, SOURCE_DIR, ARCHIVE_DIR, PROCESSED_DIR, LOG_DIR, SCRIPT_DIR)) {
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

sprintf(
  "nmemc_surfacewater_%s.log",
  format(Sys.time(), "%Y%m%d", tz = COLLECTOR_TZ)
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
base_request <- function() {
  httr2::request(ENDPOINT) |>
    httr2::req_user_agent("Mozilla/5.0 (compatible; academic-research/CNEMC-surface-water-archive)") |>
    httr2::req_headers(
      Referer = MAIN_URL,
      Origin = ORIGIN,
      Accept = "application/json, text/javascript, */*; q=0.01",
      `X-Requested-With` = "XMLHttpRequest"
    ) |>
    httr2::req_timeout(90) |>
    httr2::req_retry(max_tries = 4, retry_on_failure = TRUE)
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

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status < 200L || status >= 300L) {
    stop("HTTP ", status, " while retrieving page ", page_index)
  }

  list(
    page_index = as.integer(page_index),
    status = status,
    body = httr2::resp_body_raw(resp),
    content_type = safe_chr(httr2::resp_header(resp, "content-type")),
    server_date = safe_chr(httr2::resp_header(resp, "date"))
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
    !identical(as.character(result_value[[1]]), "0") &&
    !identical(as.character(result_value[[1]]), "false")

  if (!result_ok) {
    stop("API returned result=0/false on page ", page_index)
  }

  headers <- unlist(out$thead, use.names = FALSE)
  rows <- out$tbody
  total_pages <- suppressWarnings(as.integer(unlist(out$total, use.names = FALSE)[1]))
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

  first <- fetch_page_raw(1L)
  first_parsed <- parse_api_page(first$body, 1L)
  total_pages <- first_parsed$total_pages

  log_msg("Endpoint reports ", total_pages, " page(s) at PageSize=", PAGE_SIZE)

  pages <- vector("list", total_pages)
  pages[[1]] <- first

  if (total_pages > 1L) {
    for (i in 2:total_pages) {
      if (PAGE_DELAY_SECONDS > 0) Sys.sleep(PAGE_DELAY_SECONDS)
      log_msg("Fetching page ", i, "/", total_pages)
      pages[[i]] <- fetch_page_raw(i)
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
      PageSize = PAGE_SIZE
    ),
    total_pages = total_pages,
    page_md5 = page_md5,
    snapshot_md5 = snapshot_md5,
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
  dat$collector_id <- COLLECTOR_ID

dat$github_run_id <- if (nzchar(GITHUB_RUN_ID)) {
  GITHUB_RUN_ID
} else {
  NA_character_
}

dat$github_run_attempt <- if (nzchar(GITHUB_RUN_ATTEMPT)) {
  GITHUB_RUN_ATTEMPT
} else {
  NA_character_
}

dat$scraper_code_commit <- if (nzchar(GITHUB_SHA)) {
  GITHUB_SHA
} else {
  NA_character_
}

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
stamp <- format(
  bundle$collected_at,
  "%Y%m%d_%H%M%S",
  tz = COLLECTOR_TZ
)

year_dir <- file.path(
  ARCHIVE_DIR,
  format(bundle$collected_at, "%Y", tz = COLLECTOR_TZ)
)
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

fetch_area_river_dictionary <- function() {
  req <- base_request() |>
    httr2::req_body_form(action = "getArea_RiverDic")

  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) < 200L || httr2::resp_status(resp) >= 300L) {
    stop("Area-river dictionary HTTP status ", httr2::resp_status(resp))
  }

  raw_body <- httr2::resp_body_raw(resp)
  new_md5 <- md5_raw(raw_body)
  old_md5 <- if (file.exists(area_river_path)) unname(tools::md5sum(area_river_path)) else NA_character_

  if (is.na(old_md5) || !identical(old_md5, new_md5)) {
now <- Sys.time()

stamp <- format(
  now,
  "%Y%m%d_%H%M%S",
  tz = COLLECTOR_TZ
)

year_dir <- file.path(
  ARCHIVE_DIR,
  format(now, "%Y", tz = COLLECTOR_TZ)
)
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

update_cumulative_master <- function(snapshot_data, run_time, source_cols) {
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

  if (!file.exists(master_rds_path)) {
    master <- snap_unique
  } else {
    master <- readRDS(master_rds_path)

    # Schema migration for archives created by v1: recompute hashes using the
    # inferred full observation timestamp. This prevents cross-year collisions
    # and avoids duplicating existing rows when upgrading the script.
    master <- recompute_observation_hashes(master, source_cols)

    matched <- match(snap_unique$row_hash, master$row_hash)
    existing_idx <- which(!is.na(matched))
    new_idx <- which(is.na(matched))

    if (length(existing_idx) > 0L) {
      mi <- matched[existing_idx]
      master$last_seen[mi] <- as.POSIXct(run_time, tz = "Asia/Shanghai")
      master$times_seen[mi] <- as.integer(master$times_seen[mi]) +
        as.integer(snap_unique$snapshot_occurrences[existing_idx])
    }

    if (length(new_idx) > 0L) {
      master <- dplyr::bind_rows(master, snap_unique[new_idx, , drop = FALSE])
    }
  }

  # Count distinct published versions for each station/time observation key.
  revision_summary <- master |>
    dplyr::count(.data$observation_key_hash, name = "revision_count")

  master <- master |>
    dplyr::select(-dplyr::any_of("revision_count")) |>
    dplyr::left_join(revision_summary, by = "observation_key_hash") |>
    dplyr::arrange(.data$first_seen, .data$area, .data$monitoring_section)

  saveRDS(master, master_rds_path, compress = "xz")
  readr::write_csv(master, master_csv_path, na = "")

  duplicate_counts <- snapshot_data |> dplyr::count(.data$row_hash, name = "n")
  duplicate_source_n <- sum(duplicate_counts$n - 1L)
  revised_keys_n <- sum(master$revision_count > 1L, na.rm = TRUE)

  log_msg(
    "Cumulative archive: ", nrow(master), " unique published row version(s); ",
    "exact duplicate rows in current snapshot: ", duplicate_source_n
  )
  log_msg("Rows belonging to revised observation keys: ", revised_keys_n)
  log_msg("Cumulative RDS: ", master_rds_path)

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

bundle <- tryCatch(
  fetch_snapshot(),
  error = function(e) {
    log_msg("FATAL data fetch error: ", conditionMessage(e))
    stop(e)
  }
)

changed <- archive_snapshot_if_changed(bundle)
parsed <- parse_snapshot(bundle)
write_current_outputs(parsed)
update_cumulative_master(
  parsed$data,
  bundle$collected_at,
  parsed$dictionary$standardized_name
)
append_run_manifest(bundle, parsed, changed)

# Metadata dictionary is useful but should not cause the main data run to fail.
tryCatch(
  fetch_area_river_dictionary(),
  error = function(e) log_msg("WARNING area-river dictionary fetch failed: ", conditionMessage(e))
)

log_msg(
  "END CNEMC surface-water archive; rows=", nrow(parsed$data),
  "; pages=", bundle$total_pages,
  "; snapshot_md5=", bundle$snapshot_md5
)