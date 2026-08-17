# ============================================================
# Prepare CNEMC surface-water output for persistent Git backup
# ============================================================

options(stringsAsFactors = FALSE)

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

SURFACE_DIR <- file.path(
  DATA_ROOT,
  "nmemc",
  "data",
  "surfacewater"
)

SOURCE_DIR <- file.path(
  SURFACE_DIR,
  "source"
)

PROCESSED_DIR <- file.path(
  SURFACE_DIR,
  "processed"
)

CURRENT_META <- file.path(
  SOURCE_DIR,
  "surfacewater_current_meta.rds"
)

CURRENT_RAW <- file.path(
  SOURCE_DIR,
  "surfacewater_current_raw.rds"
)

CURRENT_PROCESSED <- file.path(
  PROCESSED_DIR,
  "nmemc_surfacewater_current.csv.gz"
)

RUN_MANIFEST <- file.path(
  PROCESSED_DIR,
  "nmemc_surfacewater_run_manifest.csv"
)

AREA_RIVER_CURRENT <- file.path(
  SOURCE_DIR,
  "area_river_current.json"
)

HEADER_DICTIONARY <- file.path(
  PROCESSED_DIR,
  "nmemc_surfacewater_header_dictionary.csv"
)

required_files <- c(
  CURRENT_META,
  CURRENT_RAW,
  CURRENT_PROCESSED,
  RUN_MANIFEST,
  AREA_RIVER_CURRENT,
  HEADER_DICTIONARY
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Required CNEMC output file(s) missing:\n",
    paste(missing_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 2. Helpers
# ------------------------------------------------------------

md5_file <- function(path) {
  unname(as.character(tools::md5sum(path)))
}

copy_new_file <- function(source, destination) {

  if (!file.exists(source)) {
    stop("Source file does not exist: ", source)
  }

  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (file.exists(destination)) {
    message(
      "Already present: ",
      destination
    )
    return(FALSE)
  }

  ok <- file.copy(
    from = source,
    to = destination,
    overwrite = FALSE
  )

  if (!isTRUE(ok)) {
    stop(
      "Failed to copy:\n",
      source,
      "\n→ ",
      destination
    )
  }

  message(
    "Added: ",
    destination
  )

  TRUE
}

read_last_manifest_row <- function(path) {

  x <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (nrow(x) == 0L) {
    stop("CNEMC run manifest contains no rows.")
  }

  x[nrow(x), , drop = FALSE]
}

get_field <- function(x, name, default = NA) {

  if (!name %in% names(x)) {
    return(default)
  }

  value <- x[[name]][1]

  if (length(value) == 0L) {
    return(default)
  }

  value
}

# ------------------------------------------------------------
# 3. Read current CNEMC metadata
# ------------------------------------------------------------

meta <- readRDS(CURRENT_META)

snapshot_md5 <- as.character(
  meta$snapshot_md5
)

if (
  length(snapshot_md5) != 1L ||
  is.na(snapshot_md5) ||
  !nzchar(snapshot_md5)
) {
  stop("Invalid snapshot_md5 in surfacewater_current_meta.rds.")
}

checked_at <- meta$checked_at

if (
  length(checked_at) != 1L ||
  is.na(checked_at)
) {
  stop("Invalid checked_at in surfacewater_current_meta.rds.")
}

checked_at <- as.POSIXct(
  checked_at,
  origin = "1970-01-01",
  tz = TIMEZONE
)

stamp <- format(
  checked_at,
  "%Y%m%d_%H%M%S",
  tz = TIMEZONE
)

year <- format(
  checked_at,
  "%Y",
  tz = TIMEZONE
)

month <- format(
  checked_at,
  "%m",
  tz = TIMEZONE
)

day <- format(
  checked_at,
  "%d",
  tz = TIMEZONE
)

hash_short <- substr(
  snapshot_md5,
  1L,
  12L
)

message(
  "CNEMC snapshot md5: ",
  snapshot_md5
)

message(
  "CNEMC collected at: ",
  format(
    checked_at,
    "%Y-%m-%d %H:%M:%S %Z",
    tz = TIMEZONE
  )
)

# ------------------------------------------------------------
# 4. Persistent backup directories
# ------------------------------------------------------------

BACKUP_ROOT <- file.path(
  BACKUP_REPO,
  "cnemc_surfacewater"
)

STATE_DIR <- file.path(
  BACKUP_ROOT,
  "state"
)

MANIFEST_DIR <- file.path(
  BACKUP_ROOT,
  "manifests"
)

DICTIONARY_DIR <- file.path(
  BACKUP_ROOT,
  "dictionaries"
)

SNAPSHOT_REL_DIR <- file.path(
  "cnemc_surfacewater",
  "snapshots",
  year,
  month,
  day
)

SNAPSHOT_DIR <- file.path(
  BACKUP_REPO,
  SNAPSHOT_REL_DIR
)

dir.create(
  STATE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  MANIFEST_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  DICTIONARY_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  SNAPSHOT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 5. Content-addressed dictionaries
# ------------------------------------------------------------

area_river_md5 <- md5_file(
  AREA_RIVER_CURRENT
)

area_river_name <- paste0(
  "area_river_",
  area_river_md5,
  ".json"
)

area_river_destination <- file.path(
  DICTIONARY_DIR,
  area_river_name
)

copy_new_file(
  AREA_RIVER_CURRENT,
  area_river_destination
)

header_dictionary_md5 <- md5_file(
  HEADER_DICTIONARY
)

header_dictionary_name <- paste0(
  "header_dictionary_",
  header_dictionary_md5,
  ".csv"
)

header_dictionary_destination <- file.path(
  DICTIONARY_DIR,
  header_dictionary_name
)

copy_new_file(
  HEADER_DICTIONARY,
  header_dictionary_destination
)

# ------------------------------------------------------------
# 6. Detect whether this snapshot is already backed up
# ------------------------------------------------------------

LATEST_HASH_FILE <- file.path(
  STATE_DIR,
  "latest_snapshot_md5.txt"
)

previous_snapshot_md5 <- NA_character_

if (file.exists(LATEST_HASH_FILE)) {

  previous_lines <- readLines(
    LATEST_HASH_FILE,
    warn = FALSE
  )

  if (length(previous_lines) > 0L) {
    previous_snapshot_md5 <- trimws(
      previous_lines[1L]
    )
  }
}

snapshot_changed <- (
  is.na(previous_snapshot_md5) ||
    !identical(
      previous_snapshot_md5,
      snapshot_md5
    )
)

if (!snapshot_changed) {

  message(
    "Snapshot already backed up; no new snapshot files required."
  )

} else {

  message(
    "New CNEMC snapshot detected."
  )

  # ----------------------------------------------------------
  # 7. Append-only raw and processed snapshot files
  # ----------------------------------------------------------

  snapshot_stem <- paste0(
    "cnemc_surfacewater_",
    stamp,
    "_",
    hash_short
  )

  raw_filename <- paste0(
    snapshot_stem,
    "_raw.rds"
  )

  processed_filename <- paste0(
    snapshot_stem,
    "_processed.csv.gz"
  )

  raw_relative_path <- file.path(
    SNAPSHOT_REL_DIR,
    raw_filename
  )

  processed_relative_path <- file.path(
    SNAPSHOT_REL_DIR,
    processed_filename
  )

  raw_destination <- file.path(
    BACKUP_REPO,
    raw_relative_path
  )

  processed_destination <- file.path(
    BACKUP_REPO,
    processed_relative_path
  )

  if (
    file.exists(raw_destination) ||
    file.exists(processed_destination)
  ) {
    stop(
      paste0(
        "Snapshot destination already exists while state hash differs.\n",
        "Refusing to overwrite an append-only backup."
      )
    )
  }

  copy_new_file(
    CURRENT_RAW,
    raw_destination
  )

  copy_new_file(
    CURRENT_PROCESSED,
    processed_destination
  )

  # ----------------------------------------------------------
  # 8. Append persistent snapshot manifest
  # ----------------------------------------------------------

  current_run <- read_last_manifest_row(
    RUN_MANIFEST
  )

  snapshot_manifest_path <- file.path(
    MANIFEST_DIR,
    "snapshot_manifest.csv"
  )

  manifest_entry <- data.frame(
    collected_at = format(
      checked_at,
      "%Y-%m-%d %H:%M:%S%z",
      tz = TIMEZONE
    ),
    snapshot_md5 = snapshot_md5,
    rows = get_field(
      current_run,
      "rows",
      NA_integer_
    ),
    total_pages = get_field(
      current_run,
      "total_pages",
      NA_integer_
    ),
    page_size = get_field(
      current_run,
      "page_size",
      NA_integer_
    ),
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
    raw_file = raw_relative_path,
    processed_file = processed_relative_path,
    area_river_md5 = area_river_md5,
    header_dictionary_md5 = header_dictionary_md5,
    stringsAsFactors = FALSE
  )

  manifest_exists <- file.exists(
    snapshot_manifest_path
  )

  utils::write.table(
    manifest_entry,
    file = snapshot_manifest_path,
    sep = ",",
    row.names = FALSE,
    col.names = !manifest_exists,
    append = manifest_exists,
    quote = TRUE,
    qmethod = "double",
    na = "",
    fileEncoding = "UTF-8"
  )

  message(
    "Updated snapshot manifest: ",
    snapshot_manifest_path
  )

  # ----------------------------------------------------------
  # 9. Update latest snapshot state
  # ----------------------------------------------------------

  writeLines(
    snapshot_md5,
    con = LATEST_HASH_FILE,
    useBytes = TRUE
  )

  message(
    "Updated latest snapshot state: ",
    LATEST_HASH_FILE
  )
}

# ------------------------------------------------------------
# 10. Summary
# ------------------------------------------------------------

message("")
message("CNEMC persistent-backup preparation complete.")
message("Snapshot changed: ", snapshot_changed)
message("Backup repository: ", BACKUP_REPO)