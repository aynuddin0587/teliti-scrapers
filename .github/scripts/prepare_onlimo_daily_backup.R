# ============================================================
# Prepare ONLIMO daily output for persistent Git backup
# ============================================================

options(stringsAsFactors = FALSE)

library(readr)
library(dplyr)

# ------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------

DATA_ROOT <- Sys.getenv("TELITI_DATA_ROOT", unset = "")
BACKUP_REPO <- Sys.getenv("TELITI_BACKUP_REPO", unset = "")
TIMEZONE <- Sys.getenv("TELITI_TIMEZONE", unset = "Asia/Taipei")

if (!nzchar(DATA_ROOT)) {
  stop("TELITI_DATA_ROOT is not defined.")
}

if (!nzchar(BACKUP_REPO)) {
  stop("TELITI_BACKUP_REPO is not defined.")
}

if (!dir.exists(BACKUP_REPO)) {
  stop("Backup repository does not exist: ", BACKUP_REPO)
}

if (!dir.exists(file.path(BACKUP_REPO, ".git"))) {
  stop("TELITI_BACKUP_REPO is not a Git repository: ", BACKUP_REPO)
}

ONLIMO_DATA_DIR <- file.path(
  DATA_ROOT,
  "onlimo",
  "data"
)

RUN_ROWS_FILE <- file.path(
  ONLIMO_DATA_DIR,
  "onlimo_daily_run_rows.csv.gz"
)

CATALOG_FILE <- file.path(
  ONLIMO_DATA_DIR,
  "onlimo_station_catalog.csv"
)

STATE_FILE <- file.path(
  ONLIMO_DATA_DIR,
  "onlimo_daily_station_state.csv"
)

required_files <- c(
  RUN_ROWS_FILE,
  CATALOG_FILE,
  STATE_FILE
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Required ONLIMO output file(s) missing:\n",
    paste(missing_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 2. Helpers
# ------------------------------------------------------------

md5_file <- function(path) {
  unname(as.character(tools::md5sum(path)))
}

canonical_table_md5 <- function(data, volatile_columns = character()) {
  stable <- data |>
    select(-any_of(volatile_columns))

  if ("station_id" %in% names(stable)) {
    if ("date" %in% names(stable)) {
      stable <- stable |>
        arrange(station_id, date)
    } else {
      stable <- stable |>
        arrange(station_id)
    }
  }

  stable <- stable[, sort(names(stable)), drop = FALSE]

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  utils::write.table(
    stable,
    file = tmp,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    qmethod = "double",
    na = "",
    fileEncoding = "UTF-8"
  )

  md5_file(tmp)
}

copy_if_missing <- function(source, destination) {
  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (file.exists(destination)) {
    message("Already present: ", destination)
    return(FALSE)
  }

  ok <- file.copy(
    source,
    destination,
    overwrite = FALSE
  )

  if (!isTRUE(ok)) {
    stop("Failed to copy ", source, " to ", destination)
  }

  message("Added: ", destination)
  TRUE
}

read_first_line <- function(path) {
  if (!file.exists(path)) return(NA_character_)

  x <- readLines(path, warn = FALSE)
  if (length(x) == 0L) return(NA_character_)

  trimws(x[[1]])
}

# ------------------------------------------------------------
# 3. Read current run outputs
# ------------------------------------------------------------

run_rows <- read_csv(
  RUN_ROWS_FILE,
  show_col_types = FALSE,
  col_types = cols(
    date = col_date(),
    .default = col_guess()
  )
)

catalog <- read_csv(
  CATALOG_FILE,
  show_col_types = FALSE
)

station_state <- read_csv(
  STATE_FILE,
  show_col_types = FALSE,
  col_types = cols(
    station_id = col_character(),
    last_archived_date = col_date()
  )
)

if (nrow(run_rows) == 0L) {
  stop("ONLIMO current-run delta contains zero rows.")
}

volatile_run_columns <- c(
  "retrieved_at",
  "collector_id",
  "github_run_id",
  "github_run_attempt",
  "scraper_code_commit",
  "source_method"
)

volatile_catalog_columns <- c(
  "catalog_retrieved_at",
  "collector_id",
  "github_run_id",
  "github_run_attempt",
  "scraper_code_commit"
)

payload_md5 <- canonical_table_md5(
  run_rows,
  volatile_columns = volatile_run_columns
)

catalog_md5 <- canonical_table_md5(
  catalog,
  volatile_columns = volatile_catalog_columns
)

message("ONLIMO run payload md5: ", payload_md5)
message("ONLIMO station catalogue md5: ", catalog_md5)
message("ONLIMO run rows: ", nrow(run_rows))

# ------------------------------------------------------------
# 4. Persistent backup directories
# ------------------------------------------------------------

BACKUP_ROOT <- file.path(
  BACKUP_REPO,
  "onlimo_daily"
)

STATE_DIR <- file.path(
  BACKUP_ROOT,
  "state"
)

MANIFEST_DIR <- file.path(
  BACKUP_ROOT,
  "manifests"
)

CATALOG_DIR <- file.path(
  BACKUP_ROOT,
  "catalogs"
)

dir.create(STATE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MANIFEST_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CATALOG_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 5. Store station catalogue by scientific content hash
# ------------------------------------------------------------

catalog_destination <- file.path(
  CATALOG_DIR,
  paste0("station_catalog_", catalog_md5, ".csv")
)

copy_if_missing(
  CATALOG_FILE,
  catalog_destination
)

# ------------------------------------------------------------
# 6. Restore/update compact recovery state
# ------------------------------------------------------------

persistent_state_file <- file.path(
  STATE_DIR,
  "station_state.csv"
)

ok <- file.copy(
  STATE_FILE,
  persistent_state_file,
  overwrite = TRUE
)

if (!isTRUE(ok)) {
  stop("Failed to update persistent ONLIMO station state.")
}

message("Updated persistent station state: ", persistent_state_file)

# ------------------------------------------------------------
# 7. Detect whether scientific run content is already backed up
# ------------------------------------------------------------

LATEST_PAYLOAD_FILE <- file.path(
  STATE_DIR,
  "latest_payload_md5.txt"
)

previous_payload_md5 <- read_first_line(
  LATEST_PAYLOAD_FILE
)

payload_changed <- (
  is.na(previous_payload_md5) ||
    !identical(previous_payload_md5, payload_md5)
)

if (!payload_changed) {
  message("ONLIMO run payload is unchanged; no new observation partition needed.")
} else {
  now <- Sys.time()

  stamp <- format(
    now,
    "%Y%m%d_%H%M%S",
    tz = TIMEZONE
  )

  year <- format(now, "%Y", tz = TIMEZONE)
  month <- format(now, "%m", tz = TIMEZONE)
  day <- format(now, "%d", tz = TIMEZONE)

  hash_short <- substr(payload_md5, 1L, 12L)

  snapshot_relative_dir <- file.path(
    "onlimo_daily",
    "snapshots",
    year,
    month,
    day
  )

  snapshot_filename <- paste0(
    "onlimo_daily_",
    stamp,
    "_",
    hash_short,
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

  copy_if_missing(
    RUN_ROWS_FILE,
    snapshot_destination
  )

  manifest_path <- file.path(
    MANIFEST_DIR,
    "snapshot_manifest.csv"
  )

  min_date <- suppressWarnings(min(run_rows$date, na.rm = TRUE))
  max_date <- suppressWarnings(max(run_rows$date, na.rm = TRUE))

  if (!is.finite(as.numeric(min_date))) min_date <- as.Date(NA)
  if (!is.finite(as.numeric(max_date))) max_date <- as.Date(NA)

  manifest_entry <- data.frame(
    backed_up_at = format(
      now,
      "%Y-%m-%d %H:%M:%S%z",
      tz = TIMEZONE
    ),
    payload_md5 = payload_md5,
    rows = nrow(run_rows),
    min_observation_date = as.character(min_date),
    max_observation_date = as.character(max_date),
    collector_id = Sys.getenv(
      "TELITI_COLLECTOR_ID",
      unset = "github_actions"
    ),
    github_run_id = Sys.getenv(
      "GITHUB_RUN_ID",
      unset = ""
    ),
    github_run_attempt = Sys.getenv(
      "GITHUB_RUN_ATTEMPT",
      unset = ""
    ),
    scraper_code_commit = Sys.getenv(
      "GITHUB_SHA",
      unset = ""
    ),
    snapshot_file = snapshot_relative_path,
    station_catalog_md5 = catalog_md5,
    stringsAsFactors = FALSE
  )

  manifest_exists <- file.exists(manifest_path)

  utils::write.table(
    manifest_entry,
    file = manifest_path,
    sep = ",",
    row.names = FALSE,
    col.names = !manifest_exists,
    append = manifest_exists,
    quote = TRUE,
    qmethod = "double",
    na = "",
    fileEncoding = "UTF-8"
  )

  writeLines(
    payload_md5,
    con = LATEST_PAYLOAD_FILE,
    useBytes = TRUE
  )

  message("Added ONLIMO observation partition: ", snapshot_relative_path)
  message("Updated ONLIMO snapshot manifest: ", manifest_path)
  message("Updated latest payload state: ", LATEST_PAYLOAD_FILE)
}

message("")
message("ONLIMO persistent-backup preparation complete.")
message("Payload changed: ", payload_changed)
message("Backup repository: ", BACKUP_REPO)