# ONLIMO rolling detailed-data archive
# Strategy:
# 1) Fetch the ONLIMO main page once and parse the embedded JavaScript dataMap.
# 2) Save the latest detailed record for every currently active station.
# 3) If the local archive shows a short gap, use ajaxGetData only for missing dates.
# 4) Append, deduplicate by station_id + date, and keep a retrieval log.

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(readr)

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

TELITI_DATA_ROOT <- Sys.getenv(
  "TELITI_DATA_ROOT",
  unset = "D:/# R Project/penelitian"
)

PROJECT_DIR <- file.path(
  TELITI_DATA_ROOT,
  "onlimo"
)

DATA_DIR    <- file.path(PROJECT_DIR, "data")
LOG_DIR     <- file.path(PROJECT_DIR, "log")

BASE_PAGE <- "https://onlimo.kemenlh.go.id/app/"
BASE_API  <- "https://onlimo.kemenlh.go.id/app/index"

# Recommended default: collect every station that currently exposes recent data.
# Alternative: set COLLECT_MODE <- "selected_das".
COLLECT_MODE <- "all_active"

SELECTED_DAS <- c(
  "Ciliwung",
  "Cisadane",
  "Citarum",
  "Bengawan Solo",
  "Musi"
)

# The public detailed-data window is approximately one week.
# Recovery is attempted only when a station already exists in the local archive.
RECOVERY_DAYS <- 8L
MAX_RECOVERY_REQUESTS_PER_RUN <- 600L
RECOVERY_PAUSE_MIN_SEC <- 1.0
RECOVERY_PAUSE_MAX_SEC <- 1.8

ARCHIVE_FILE <- file.path(DATA_DIR, "onlimo_daily_parameters_archive.csv")
CATALOG_FILE <- file.path(DATA_DIR, "onlimo_station_catalog.csv")

# -----------------------------------------------------------------------------
# Setup and logging
# -----------------------------------------------------------------------------

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR,  recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(
  LOG_DIR,
  paste0("daily_archive_", format(Sys.Date(), "%Y-%m"), ".log")
)

log_msg <- function(...) {
  msg <- paste0(..., collapse = "")
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", msg)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

retrieved_at_now <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")
}

# -----------------------------------------------------------------------------
# Small safe extraction helpers
# -----------------------------------------------------------------------------

pluck_chr <- function(x, ...) {
  z <- purrr::pluck(x, ..., .default = NULL)
  if (is.null(z) || length(z) == 0) return(NA_character_)
  as.character(z[[1]])
}

pluck_num <- function(x, ...) {
  z <- purrr::pluck(x, ..., .default = NULL)
  if (is.null(z) || length(z) == 0) return(NA_real_)
  suppressWarnings(as.numeric(z[[1]]))
}

pluck_lgl <- function(x, ...) {
  z <- purrr::pluck(x, ..., .default = FALSE)
  isTRUE(z)
}

# -----------------------------------------------------------------------------
# HTTP helpers
# -----------------------------------------------------------------------------

perform_request <- function(url, ajax = FALSE) {
  req <- request(url) |>
    req_headers(
      Accept = if (ajax) {
        "application/json, text/javascript, */*; q=0.01"
      } else {
        "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
      }
    ) |>
    req_retry(max_tries = 3) |>
    req_timeout(seconds = 45)

  if (ajax) {
    req <- req |>
      req_headers(
        `X-Requested-With` = "XMLHttpRequest",
        Referer = BASE_PAGE
      )
  }

  req_perform(req)
}

# -----------------------------------------------------------------------------
# Parse JavaScript variable: var dataMap = [...];
# -----------------------------------------------------------------------------

extract_data_map_json <- function(html_text) {
  # ONLIMO currently writes the assignment as, for example:
  #   var dataMap = [...] ;
  # Note the optional whitespace before the semicolon. Do not depend on the
  # name of the JavaScript variable that follows dataMap.
  pattern <- "(?s)var\\s+dataMap\\s*=\\s*(\\[.*?\\])\\s*;"

  hit <- regexec(pattern, html_text, perl = TRUE)
  parts <- regmatches(html_text, hit)[[1]]

  if (length(parts) < 2) {
    stop("Could not locate JavaScript variable dataMap on the ONLIMO page.")
  }

  parts[[2]]
}

fetch_data_map <- function() {
  resp <- perform_request(BASE_PAGE, ajax = FALSE)
  html <- resp_body_string(resp)

  log_msg(
    "Fetched main page: ", nchar(html),
    " characters; dataMap marker present: ",
    grepl("dataMap", html, fixed = TRUE)
  )

  json_txt <- tryCatch(
    extract_data_map_json(html),
    error = function(e) {
      diagnostic_file <- file.path(
        LOG_DIR,
        paste0(
          "failed_main_page_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".html"
        )
      )
      writeLines(html, diagnostic_file, useBytes = TRUE)
      log_msg("Saved failed main-page HTML to: ", diagnostic_file)
      stop(e)
    }
  )

  fromJSON(
    json_txt,
    simplifyVector = FALSE
  )
}

# -----------------------------------------------------------------------------
# Station metadata catalogue
# -----------------------------------------------------------------------------

station_to_catalog_row <- function(x) {
  tibble(
    station_id = pluck_chr(x, "IDStasiun"),
    station_name = pluck_chr(x, "NamaStasiun"),
    river = pluck_chr(x, "nama_sungai"),
    watershed = pluck_chr(x, "nama_das"),
    location_category = pluck_chr(x, "KategoriLokasi"),
    river_segment = pluck_chr(x, "segmen_sungai"),
    station_category = pluck_chr(x, "KategoriStasiun"),
    kabupaten_kota = pluck_chr(x, "kabkota"),
    province = pluck_chr(x, "provinsi"),
    latitude = suppressWarnings(as.numeric(pluck_chr(x, "latitude"))),
    longitude = suppressWarnings(as.numeric(pluck_chr(x, "longitude"))),
    weeks = pluck_lgl(x, "weeks"),
    latest_detail_date = as.Date(pluck_chr(x, "tgl_data_default")),
    catalog_retrieved_at = retrieved_at_now()
  )
}

# -----------------------------------------------------------------------------
# Parse one detailed station-date record
# -----------------------------------------------------------------------------

station_to_detail_row <- function(x, date, source_method) {
  result <- purrr::pluck(x, "result", .default = NULL)

  if (!is.list(result)) {
    return(tibble())
  }

  sm <- purrr::pluck(result, "sm", .default = list())

  status_colour <- if (length(sm) >= 1) as.character(sm[[1]]) else NA_character_
  pollution_status <- if (length(sm) >= 2) as.character(sm[[2]]) else NA_character_

  tibble(
    station_id = pluck_chr(x, "IDStasiun"),
    date = as.Date(date),

    station_name = pluck_chr(x, "NamaStasiun"),
    river = pluck_chr(x, "nama_sungai"),
    watershed = pluck_chr(x, "nama_das"),
    location_category = pluck_chr(x, "KategoriLokasi"),
    river_segment = pluck_chr(x, "segmen_sungai"),
    station_category = pluck_chr(x, "KategoriStasiun"),
    kabupaten_kota = pluck_chr(x, "kabkota"),
    province = pluck_chr(x, "provinsi"),
    latitude = suppressWarnings(as.numeric(pluck_chr(x, "latitude"))),
    longitude = suppressWarnings(as.numeric(pluck_chr(x, "longitude"))),

    # Raw daily mean values reported in result$rata
    ph = pluck_num(x, "result", "rata", "PH"),
    do = pluck_num(x, "result", "rata", "DO"),
    tds = pluck_num(x, "result", "rata", "TDS"),
    cod = pluck_num(x, "result", "rata", "COD"),
    bod = pluck_num(x, "result", "rata", "BOD"),
    tss = pluck_num(x, "result", "rata", "TSS"),
    nitrate = pluck_num(x, "result", "rata", "Nitrat"),
    ammonia = pluck_num(x, "result", "rata", "Amonia"),
    temperature = pluck_num(x, "result", "rata", "Suhu"),

    # Parameter-level index values in result$avg
    index_ph = pluck_num(x, "result", "avg", "PH"),
    index_do = pluck_num(x, "result", "avg", "DO"),
    index_tds = pluck_num(x, "result", "avg", "TDS"),
    index_cod = pluck_num(x, "result", "avg", "COD"),
    index_bod = pluck_num(x, "result", "avg", "BOD"),
    index_tss = pluck_num(x, "result", "avg", "TSS"),
    index_nitrate = pluck_num(x, "result", "avg", "Nitrat"),
    index_ammonia = pluck_num(x, "result", "avg", "Amonia"),

    mean_parameter_index = pluck_num(x, "result", "avgAll"),
    maximum_parameter_index = pluck_num(x, "result", "max", 2),
    critical_parameter = pluck_chr(x, "result", "kritis"),
    pollution_index = pluck_num(x, "result", "ip"),
    pollution_status = pollution_status,
    status_colour = status_colour,

    source_method = source_method,
    retrieved_at = retrieved_at_now()
  )
}

# -----------------------------------------------------------------------------
# ajaxGetData recovery request
# -----------------------------------------------------------------------------

get_ajax_detail <- function(station, date) {
  date <- as.Date(date)

  url <- sprintf(
    "%s/ajaxGetData/id/%s/tanggal/%s",
    BASE_API,
    station,
    format(date, "%Y-%m-%d")
  )

  resp <- perform_request(url, ajax = TRUE)

  x <- resp_body_json(
    resp,
    simplifyVector = FALSE
  )

  if (length(x) == 0 || !is.list(purrr::pluck(x, "result", .default = NULL))) {
    return(tibble())
  }

  station_to_detail_row(
    x,
    date = date,
    source_method = "ajaxGetData_recovery"
  )
}

# -----------------------------------------------------------------------------
# Archive helpers
# -----------------------------------------------------------------------------

read_archive <- function() {
  if (!file.exists(ARCHIVE_FILE)) return(tibble())

  read_csv(
    ARCHIVE_FILE,
    show_col_types = FALSE,
    col_types = cols(
      date = col_date(),
      # Keep retrieval timestamps as ISO text. If left to col_guess(),
      # readr may parse existing CSV values as POSIXct while newly
      # downloaded rows are character, which breaks bind_rows().
      retrieved_at = col_character(),
      .default = col_guess()
    )
  ) |>
    mutate(
      date = as.Date(date),
      retrieved_at = as.character(retrieved_at)
    )
}

select_catalog_scope <- function(catalog) {
  if (identical(COLLECT_MODE, "all_active")) {
    return(catalog |> filter(weeks))
  }

  if (identical(COLLECT_MODE, "selected_das")) {
    return(
      catalog |>
        filter(
          weeks,
          watershed %in% SELECTED_DAS
        )
    )
  }

  stop("COLLECT_MODE must be 'all_active' or 'selected_das'.")
}

build_recovery_plan <- function(catalog_scope, archive) {
  if (nrow(archive) == 0) return(tibble())

  last_local <- archive |>
    filter(!is.na(date)) |>
    group_by(station_id) |>
    summarise(last_archived_date = max(date), .groups = "drop")

  plan <- catalog_scope |>
    select(station_id, latest_detail_date) |>
    inner_join(last_local, by = "station_id") |>
    filter(
      !is.na(latest_detail_date),
      latest_detail_date > last_archived_date + 1
    )

  if (nrow(plan) == 0) return(tibble())

  map_dfr(
    seq_len(nrow(plan)),
    function(i) {
      station <- plan$station_id[[i]]
      latest  <- plan$latest_detail_date[[i]]
      last    <- plan$last_archived_date[[i]]

      earliest_public_candidate <- latest - (RECOVERY_DAYS - 1L)
      start_date <- max(last + 1L, earliest_public_candidate)
      end_date   <- latest - 1L

      if (start_date > end_date) return(tibble())

      tibble(
        station_id = station,
        date = seq(start_date, end_date, by = "day")
      )
    }
  ) |>
    distinct(station_id, date) |>
    slice_head(n = MAX_RECOVERY_REQUESTS_PER_RUN)
}

# -----------------------------------------------------------------------------
# Main run
# -----------------------------------------------------------------------------

log_msg("START ONLIMO daily archive")
log_msg("Collection mode: ", COLLECT_MODE)

# One main-page request supplies station metadata and the latest detailed record.
data_map <- tryCatch(
  fetch_data_map(),
  error = function(e) {
    log_msg("FATAL main-page fetch failed: ", conditionMessage(e))
    stop(e)
  }
)

catalog <- map_dfr(data_map, station_to_catalog_row)
write_csv(catalog, CATALOG_FILE, na = "")

catalog_scope <- select_catalog_scope(catalog)

log_msg("Station catalogue entries: ", nrow(catalog))
log_msg("Current weeks=true stations: ", sum(catalog$weeks, na.rm = TRUE))
log_msg("Stations in current collection scope: ", nrow(catalog_scope))

# Latest embedded records from dataMap: no per-station HTTP requests needed.
latest_rows <- data_map |>
  keep(function(x) {
    station <- pluck_chr(x, "IDStasiun")
    in_scope <- station %in% catalog_scope$station_id
    has_detail <- isTRUE(purrr::pluck(x, "weeks", .default = FALSE)) &&
      is.list(purrr::pluck(x, "result", .default = NULL)) &&
      !is.na(as.Date(pluck_chr(x, "tgl_data_default")))

    in_scope && has_detail
  }) |>
  map_dfr(function(x) {
    station_to_detail_row(
      x,
      date = as.Date(pluck_chr(x, "tgl_data_default")),
      source_method = "dataMap_latest"
    )
  })

log_msg("Latest detailed rows parsed from dataMap: ", nrow(latest_rows))

old_archive <- read_archive()
recovery_plan <- build_recovery_plan(catalog_scope, old_archive)

log_msg("Recovery requests planned: ", nrow(recovery_plan))

recovered_rows <- tibble()

if (nrow(recovery_plan) > 0) {
  recovered_rows <- map_dfr(
    seq_len(nrow(recovery_plan)),
    function(i) {
      station <- recovery_plan$station_id[[i]]
      date    <- recovery_plan$date[[i]]

      log_msg(
        "Recovery request ", i, "/", nrow(recovery_plan),
        ": ", station, " ", as.character(date)
      )

      out <- tryCatch(
        get_ajax_detail(station, date),
        error = function(e) {
          log_msg(
            "Recovery failed: ", station, " ", as.character(date),
            " | ", conditionMessage(e)
          )
          tibble()
        }
      )

      Sys.sleep(runif(1, RECOVERY_PAUSE_MIN_SEC, RECOVERY_PAUSE_MAX_SEC))
      out
    }
  )
}

new_rows <- bind_rows(latest_rows, recovered_rows) |>
  mutate(
    date = as.Date(date),
    retrieved_at = as.character(retrieved_at)
  )

# Normalize the reloaded CSV before merging. This makes repeated scheduled
# runs idempotent even if readr previously inferred retrieved_at as POSIXct.
if (nrow(old_archive) > 0) {
  old_archive <- old_archive |>
    mutate(
      date = as.Date(date),
      retrieved_at = as.character(retrieved_at)
    )
}

if (nrow(new_rows) == 0 && nrow(old_archive) == 0) {
  log_msg("No detailed records were retrieved; archive not written.")
} else {
  # bind_rows() preserves input order: old archive first, current retrievals last.
  # Therefore slice_tail() implements a simple last-write-wins rule for each
  # station-date without relying on timestamp classes or timezone parsing.
  archive <- bind_rows(old_archive, new_rows) |>
    mutate(
      date = as.Date(date),
      retrieved_at = as.character(retrieved_at)
    ) |>
    filter(!is.na(station_id), !is.na(date)) |>
    group_by(station_id, date) |>
    slice_tail(n = 1) |>
    ungroup() |>
    arrange(station_id, date)

  write_csv(archive, ARCHIVE_FILE, na = "")

  log_msg("New rows this run: ", nrow(new_rows))
  log_msg("Archive rows after deduplication: ", nrow(archive))
  log_msg("Archive stations represented: ", n_distinct(archive$station_id))
}

log_msg("FINISH ONLIMO daily archive")