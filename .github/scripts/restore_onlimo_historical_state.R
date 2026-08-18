# ============================================================
# Restore ONLIMO historical Pollution Index worker state
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

DATA_ROOT <- Sys.getenv("TELITI_DATA_ROOT", unset = "")
BACKUP_REPO <- Sys.getenv("TELITI_BACKUP_REPO", unset = "")

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
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Restore the historical request ledger, if one already exists.
# -----------------------------------------------------------------------------

persistent_ledger <- file.path(
  BACKUP_REPO,
  "onlimo_pollution_index",
  "state",
  "request_ledger.csv"
)

runtime_ledger <- file.path(
  DATA_DIR,
  "onlimo_pollution_index_request_ledger.csv"
)

if (file.exists(persistent_ledger)) {
  ok <- file.copy(
    persistent_ledger,
    runtime_ledger,
    overwrite = TRUE
  )

  if (!isTRUE(ok)) {
    stop("Failed to restore historical Pollution Index request ledger.")
  }

  ledger_rows <- nrow(
    read_csv(
      runtime_ledger,
      show_col_types = FALSE
    )
  )

  message(
    "Restored historical Pollution Index request ledger: ",
    ledger_rows,
    " rows."
  )
} else {
  message(
    "No persistent historical Pollution Index request ledger exists yet."
  )
}

# -----------------------------------------------------------------------------
# 2. Resolve the latest ONLIMO station catalogue from the daily-backup manifest.
# -----------------------------------------------------------------------------

catalog_manifest <- file.path(
  BACKUP_REPO,
  "onlimo_daily",
  "manifests",
  "snapshot_manifest.csv"
)

if (!file.exists(catalog_manifest)) {
  stop(
    "ONLIMO daily snapshot manifest not found: ",
    catalog_manifest,
    "\nRun the ONLIMO daily GitHub collector first."
  )
}

manifest <- read_csv(
  catalog_manifest,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

if (!"station_catalog_md5" %in% names(manifest)) {
  stop("ONLIMO daily manifest has no station_catalog_md5 column.")
}

catalog_hashes <- manifest$station_catalog_md5
catalog_hashes <- catalog_hashes[
  !is.na(catalog_hashes) & nzchar(catalog_hashes)
]

if (length(catalog_hashes) == 0L) {
  stop("ONLIMO daily manifest contains no usable station catalogue hash.")
}

catalog_md5 <- tail(catalog_hashes, 1L)

catalog_source <- file.path(
  BACKUP_REPO,
  "onlimo_daily",
  "catalogs",
  paste0("station_catalog_", catalog_md5, ".csv")
)

if (!file.exists(catalog_source)) {
  stop(
    "Station catalogue referenced by the ONLIMO daily manifest is missing: ",
    catalog_source
  )
}

catalog_destination <- file.path(
  DATA_DIR,
  "onlimo_station_catalog.csv"
)

ok <- file.copy(
  catalog_source,
  catalog_destination,
  overwrite = TRUE
)

if (!isTRUE(ok)) {
  stop("Failed to restore latest ONLIMO station catalogue.")
}

catalog_rows <- nrow(
  read_csv(
    catalog_destination,
    show_col_types = FALSE
  )
)

message("Restored latest ONLIMO station catalogue.")
message("Station catalogue md5: ", catalog_md5)
message("Station catalogue rows: ", catalog_rows)