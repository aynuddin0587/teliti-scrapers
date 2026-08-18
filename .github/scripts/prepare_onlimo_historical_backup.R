# ============================================================
# Prepare ONLIMO historical Pollution Index worker output
# for persistent Git backup.
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

DATA_ROOT <- Sys.getenv("TELITI_DATA_ROOT", unset = "")
BACKUP_REPO <- Sys.getenv("TELITI_BACKUP_REPO", unset = "")
TIMEZONE <- Sys.getenv("TELITI_TIMEZONE", unset = "Asia/Taipei")

if (!nzchar(DATA_ROOT)) {
  stop("TELITI_DATA_ROOT is not defined.")
}

if (!nzchar(BACKUP_REPO)) {
  stop("TELITI_BACKUP_REPO is not defined.")
}

if (!dir.exists(file.path(BACKUP_REPO, ".git"))) {
  stop("TELITI_BACKUP_REPO is not a Git repository: ", BACKUP_REPO)
}

DATA_DIR <- file.path(DATA_ROOT, "onlimo", "data")

LEDGER_FILE <- file.path(
  DATA_DIR,
  "onlimo_pollution_index_request_ledger.csv"
)

RUN_ROWS_FILE <- file.path(
  DATA_DIR,
  "onlimo_pollution_index_run_rows.csv.gz"
)

RUN_SUMMARY_FILE <- file.path(
  DATA_DIR,
  "onlimo_pollution_index_run_summary.csv"
)

BACKUP_ROOT <- file.path(
  BACKUP_REPO,
  "onlimo_pollution_index"
)

STATE_DIR <- file.path(BACKUP_ROOT, "state")
MANIFEST_DIR <- file.path(BACKUP_ROOT, "manifests")

for (x in c(STATE_DIR, MANIFEST_DIR)) {
  dir.create(x, recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

md5_file <- function(path) {
  unname(as.character(tools::md5sum(path)))
}

read_first_row <- function(path) {
  x <- read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )

  if (nrow(x) == 0L) {
    stop("Expected at least one row in: ", path)
  }

  x[1, , drop = FALSE]
}

field_chr <- function(x, name, default = "") {
  if (!name %in% names(x)) return(default)

  value <- as.character(x[[name]][1])
  if (is.na(value)) default else value
}

field_num <- function(x, name, default = NA_real_) {
  if (!name %in% names(x)) return(default)

  value <- suppressWarnings(as.numeric(x[[name]][1]))
  if (is.na(value)) default else value
}

canonical_rows_md5 <- function(data) {
  stable <- data |>
    select(-any_of("retrieved_at")) |>
    arrange(station_id, date)

  stable <- stable[, sort(names(stable)), drop = FALSE]

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  write_csv(stable, tmp, na = "")
  md5_file(tmp)
}

# -----------------------------------------------------------------------------
# 1. Persist request ledger state.
# -----------------------------------------------------------------------------

persistent_ledger <- file.path(
  STATE_DIR,
  "request_ledger.csv"
)

ledger_changed <- FALSE

if (file.exists(LEDGER_FILE)) {
  old_md5 <- if (file.exists(persistent_ledger)) {
    md5_file(persistent_ledger)
  } else {
    NA_character_
  }

  new_md5 <- md5_file(LEDGER_FILE)

  ledger_changed <- (
    is.na(old_md5) ||
      !identical(old_md5, new_md5)
  )

  ok <- file.copy(
    LEDGER_FILE,
    persistent_ledger,
    overwrite = TRUE
  )

  if (!isTRUE(ok)) {
    stop("Failed to persist historical Pollution Index request ledger.")
  }

  message(
    "Persistent request ledger updated; changed=",
    ledger_changed
  )
} else {
  message("No request ledger was produced by this run.")
}

# -----------------------------------------------------------------------------
# 2. Read worker summary when available.
# -----------------------------------------------------------------------------

summary_row <- if (file.exists(RUN_SUMMARY_FILE)) {
  read_first_row(RUN_SUMMARY_FILE)
} else {
  NULL
}

requests_attempted <- if (is.null(summary_row)) {
  0
} else {
  field_num(summary_row, "requests_attempted", 0)
}

errors_this_run <- if (is.null(summary_row)) {
  0
} else {
  field_num(summary_row, "errors", 0)
}

# -----------------------------------------------------------------------------
# 3. Add an immutable observation partition when this run retrieved data.
# -----------------------------------------------------------------------------

payload_md5 <- ""
snapshot_relative_path <- ""
run_rows_n <- 0L

if (file.exists(RUN_ROWS_FILE)) {
  run_rows <- read_csv(
    RUN_ROWS_FILE,
    show_col_types = FALSE,
    col_types = cols(
      station_id = col_character(),
      date = col_date(),
      .default = col_guess()
    )
  )

  run_rows_n <- nrow(run_rows)

  if (run_rows_n > 0L) {
    payload_md5 <- canonical_rows_md5(run_rows)

    if (!is.null(summary_row)) {
      stamp_source <- field_chr(
        summary_row,
        "run_started_at",
        ""
      )
    } else {
      stamp_source <- ""
    }

    stamp_time <- suppressWarnings(
      as.POSIXct(
        stamp_source,
        format = "%Y-%m-%d %H:%M:%S%z",
        tz = TIMEZONE
      )
    )

    if (is.na(stamp_time)) {
      stamp_time <- Sys.time()
    }

    stamp <- format(
      stamp_time,
      "%Y%m%d_%H%M%S",
      tz = TIMEZONE
    )

    year <- format(stamp_time, "%Y", tz = TIMEZONE)
    month <- format(stamp_time, "%m", tz = TIMEZONE)
    day <- format(stamp_time, "%d", tz = TIMEZONE)

    snapshot_relative_dir <- file.path(
      "onlimo_pollution_index",
      "snapshots",
      year,
      month,
      day
    )

    snapshot_filename <- paste0(
      "onlimo_pollution_index_",
      stamp,
      "_",
      substr(payload_md5, 1L, 12L),
      ".csv.gz"
    )

    snapshot_relative_path <- file.path(
      snapshot_relative_dir,
      snapshot_filename
    )

    snapshot_destination <- file.path(
      BACKUP_REPO,
      snapshot_relative_path
    )

    dir.create(
      dirname(snapshot_destination),
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (!file.exists(snapshot_destination)) {
      ok <- file.copy(
        RUN_ROWS_FILE,
        snapshot_destination,
        overwrite = FALSE
      )

      if (!isTRUE(ok)) {
        stop("Failed to add ONLIMO historical observation partition.")
      }

      message(
        "Added ONLIMO historical observation partition: ",
        snapshot_relative_path
      )
    } else {
      message(
        "Observation partition already exists: ",
        snapshot_relative_path
      )
    }
  } else {
    message("This worker run retrieved zero Pollution Index rows.")
  }
} else {
  message("No per-run Pollution Index row file was produced.")
}

# -----------------------------------------------------------------------------
# 4. Append meaningful worker-run provenance.
# -----------------------------------------------------------------------------

meaningful_run <- (
  requests_attempted > 0 ||
    errors_this_run > 0 ||
    ledger_changed ||
    run_rows_n > 0L
)

if (meaningful_run && !is.null(summary_row)) {
  worker_manifest <- file.path(
    MANIFEST_DIR,
    "worker_runs.csv"
  )

  manifest_entry <- summary_row |>
    mutate(
      payload_md5 = payload_md5,
      snapshot_file = snapshot_relative_path
    )

  should_append <- TRUE

  if (file.exists(worker_manifest)) {
    old_manifest <- read_csv(
      worker_manifest,
      show_col_types = FALSE,
      col_types = cols(.default = col_character())
    )

    run_id <- field_chr(summary_row, "github_run_id", "")

    if (
      nzchar(run_id) &&
        "github_run_id" %in% names(old_manifest) &&
        run_id %in% old_manifest$github_run_id
    ) {
      should_append <- FALSE
      message("This GitHub run is already present in worker_runs.csv.")
    }
  }

  if (should_append) {
    write.table(
      manifest_entry,
      file = worker_manifest,
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(worker_manifest),
      append = file.exists(worker_manifest),
      quote = TRUE,
      qmethod = "double",
      na = "",
      fileEncoding = "UTF-8"
    )

    message("Updated worker-run manifest: ", worker_manifest)
  }

  latest_summary <- file.path(
    STATE_DIR,
    "latest_run_summary.csv"
  )

  ok <- file.copy(
    RUN_SUMMARY_FILE,
    latest_summary,
    overwrite = TRUE
  )

  if (!isTRUE(ok)) {
    stop("Failed to update latest historical worker summary.")
  }
}

message("")
message("ONLIMO historical Pollution Index backup preparation complete.")
message("Ledger changed: ", ledger_changed)
message("Run rows: ", run_rows_n)
message("Meaningful run: ", meaningful_run)
message("Backup repository: ", BACKUP_REPO)