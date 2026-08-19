# ============================================================
# Prepare CNEMC surface-water output for persistent Git backup
#
# Storage / provenance policy
# ---------------------------
# 1. Record EVERY GitHub collection in collection_manifest.csv.
# 2. Record every changed source state in snapshot_manifest.csv.
# 3. Keep at most one full raw + processed checkpoint per 6-hour bucket.
# 4. Preserve every previously unseen scientific row version in compact,
#    append-only daily row-version delta files.
# 5. Preserve row-membership add/remove events between successive source
#    states, allowing the exact changing source state to be audited from a
#    baseline plus membership events.
# 6. Maintain a lightweight append-only row-version index and a current
#    row-membership state file for efficient future comparisons.
#
# The first run of this version creates a one-time delta baseline and seeds
# the row-version index from already retained processed checkpoints so that
# historic row versions are not unnecessarily duplicated.
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

SURFACE_DIR <- file.path(DATA_ROOT, "nmemc", "data", "surfacewater")
SOURCE_DIR <- file.path(SURFACE_DIR, "source")
PROCESSED_DIR <- file.path(SURFACE_DIR, "processed")

CURRENT_META <- file.path(SOURCE_DIR, "surfacewater_current_meta.rds")
CURRENT_RAW <- file.path(SOURCE_DIR, "surfacewater_current_raw.rds")
CURRENT_PROCESSED_RDS <- file.path(
  PROCESSED_DIR,
  "nmemc_surfacewater_current.rds"
)
CURRENT_PROCESSED <- file.path(
  PROCESSED_DIR,
  "nmemc_surfacewater_current.csv.gz"
)
RUN_MANIFEST <- file.path(
  PROCESSED_DIR,
  "nmemc_surfacewater_run_manifest.csv"
)
AREA_RIVER_CURRENT <- file.path(SOURCE_DIR, "area_river_current.json")
HEADER_DICTIONARY <- file.path(
  PROCESSED_DIR,
  "nmemc_surfacewater_header_dictionary.csv"
)

required_files <- c(
  CURRENT_META,
  CURRENT_RAW,
  CURRENT_PROCESSED_RDS,
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

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(destination)) {
    message("Already present: ", destination)
    return(FALSE)
  }

  ok <- file.copy(
    from = source,
    to = destination,
    overwrite = FALSE
  )

  if (!isTRUE(ok)) {
    stop("Failed to copy:\n", source, "\n-> ", destination)
  }

  message("Added: ", destination)
  TRUE
}

read_last_manifest_row <- function(path) {
  x <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
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

read_first_line <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  x <- readLines(path, warn = FALSE)

  if (length(x) == 0L) {
    return(NA_character_)
  }

  value <- trimws(x[1L])
  if (!nzchar(value)) NA_character_ else value
}

append_csv_utf8 <- function(x, path) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  if (nrow(x) == 0L) {
    return(invisible(FALSE))
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  exists <- file.exists(path)

  utils::write.table(
    x,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = !exists,
    append = exists,
    quote = TRUE,
    qmethod = "double",
    na = "",
    fileEncoding = "UTF-8"
  )

  invisible(TRUE)
}

read_csv_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  suppressMessages(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE
    )
  )
}

clean_hash_table <- function(x) {
  if (is.null(x) || nrow(x) == 0L) {
    return(data.frame(
      row_hash = character(),
      observation_key_hash = character(),
      stringsAsFactors = FALSE
    ))
  }

  required <- c("row_hash", "observation_key_hash")
  missing <- setdiff(required, names(x))

  if (length(missing) > 0L) {
    stop(
      "Hash table is missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }

  out <- data.frame(
    row_hash = as.character(x$row_hash),
    observation_key_hash = as.character(x$observation_key_hash),
    stringsAsFactors = FALSE
  )

  out <- out[
    !is.na(out$row_hash) & nzchar(out$row_hash) &
      !is.na(out$observation_key_hash) & nzchar(out$observation_key_hash),
    ,
    drop = FALSE
  ]

  out <- out[!duplicated(out$row_hash), , drop = FALSE]
  out[order(out$row_hash), , drop = FALSE]
}

empty_row_version_index <- function() {
  data.frame(
    row_hash = character(),
    observation_key_hash = character(),
    first_archived_at = character(),
    first_snapshot_md5 = character(),
    first_collection_key = character(),
    archive_origin = character(),
    row_versions_file = character(),
    stringsAsFactors = FALSE
  )
}

seed_index_from_retained_checkpoints <- function(
  snapshot_manifest_path,
  backup_repo
) {
  index <- empty_row_version_index()

  if (!file.exists(snapshot_manifest_path)) {
    message("No existing snapshot manifest available for row-index seeding.")
    return(index)
  }

  manifest <- suppressMessages(
    readr::read_csv(
      snapshot_manifest_path,
      show_col_types = FALSE,
      progress = FALSE
    )
  )

  if (
    nrow(manifest) == 0L ||
      !"processed_file" %in% names(manifest)
  ) {
    return(index)
  }

  manifest$processed_file <- as.character(manifest$processed_file)
  keep <- !is.na(manifest$processed_file) & nzchar(manifest$processed_file)
  manifest <- manifest[keep, , drop = FALSE]

  if (nrow(manifest) == 0L) {
    return(index)
  }

  seen <- character()
  pieces <- vector("list", nrow(manifest))
  piece_n <- 0L

  message(
    "Seeding row-version index from ",
    nrow(manifest),
    " retained processed checkpoint(s)."
  )

  for (i in seq_len(nrow(manifest))) {
    rel_path <- manifest$processed_file[[i]]
    full_path <- file.path(backup_repo, rel_path)

    if (!file.exists(full_path)) {
      warning("Retained processed checkpoint is missing: ", full_path)
      next
    }

    dat <- suppressMessages(
      readr::read_csv(
        full_path,
        show_col_types = FALSE,
        progress = FALSE
      )
    )

    hashes <- clean_hash_table(dat)

    if (nrow(hashes) == 0L) {
      next
    }

    new_mask <- !(hashes$row_hash %in% seen)
    new_hashes <- hashes[new_mask, , drop = FALSE]

    if (nrow(new_hashes) == 0L) {
      next
    }

    piece_n <- piece_n + 1L

    pieces[[piece_n]] <- data.frame(
      row_hash = new_hashes$row_hash,
      observation_key_hash = new_hashes$observation_key_hash,
      first_archived_at = if (
        "collected_at" %in% names(manifest)
      ) as.character(manifest$collected_at[[i]]) else "",
      first_snapshot_md5 = if (
        "snapshot_md5" %in% names(manifest)
      ) as.character(manifest$snapshot_md5[[i]]) else "",
      first_collection_key = "",
      archive_origin = "retained_checkpoint",
      row_versions_file = rel_path,
      stringsAsFactors = FALSE
    )

    seen <- c(seen, new_hashes$row_hash)
  }

  if (piece_n == 0L) {
    return(index)
  }

  index <- do.call(rbind, pieces[seq_len(piece_n)])
  index <- index[!duplicated(index$row_hash), , drop = FALSE]
  rownames(index) <- NULL

  message(
    "Seeded ",
    nrow(index),
    " unique historic row version(s) from retained checkpoints."
  )

  index
}

# ------------------------------------------------------------
# 3. Read current CNEMC metadata and processed rows
# ------------------------------------------------------------

meta <- readRDS(CURRENT_META)

snapshot_md5 <- as.character(meta$snapshot_md5)

if (
  length(snapshot_md5) != 1L ||
    is.na(snapshot_md5) ||
    !nzchar(snapshot_md5)
) {
  stop("Invalid snapshot_md5 in surfacewater_current_meta.rds.")
}

checked_at <- meta$checked_at

if (length(checked_at) != 1L || is.na(checked_at)) {
  stop("Invalid checked_at in surfacewater_current_meta.rds.")
}

checked_at <- as.POSIXct(
  checked_at,
  origin = "1970-01-01",
  tz = TIMEZONE
)

stamp <- format(checked_at, "%Y%m%d_%H%M%S", tz = TIMEZONE)
year <- format(checked_at, "%Y", tz = TIMEZONE)
month <- format(checked_at, "%m", tz = TIMEZONE)
day <- format(checked_at, "%d", tz = TIMEZONE)
hour <- as.integer(format(checked_at, "%H", tz = TIMEZONE))
hash_short <- substr(snapshot_md5, 1L, 12L)

bucket_hour <- floor(hour / 6L) * 6L
retention_bucket <- sprintf(
  "%s_%02d",
  format(checked_at, "%Y%m%d", tz = TIMEZONE),
  bucket_hour
)

current_data <- readRDS(CURRENT_PROCESSED_RDS)

required_hash_cols <- c("row_hash", "observation_key_hash")
missing_hash_cols <- setdiff(required_hash_cols, names(current_data))

if (length(missing_hash_cols) > 0L) {
  stop(
    "Current processed CNEMC data are missing required hash column(s): ",
    paste(missing_hash_cols, collapse = ", ")
  )
}

current_data$row_hash <- as.character(current_data$row_hash)
current_data$observation_key_hash <- as.character(
  current_data$observation_key_hash
)

if (
  any(is.na(current_data$row_hash)) ||
    any(!nzchar(current_data$row_hash)) ||
    any(is.na(current_data$observation_key_hash)) ||
    any(!nzchar(current_data$observation_key_hash))
) {
  stop("Current processed CNEMC data contain invalid row/key hashes.")
}

current_unique <- current_data[
  !duplicated(current_data$row_hash),
  ,
  drop = FALSE
]

current_membership <- clean_hash_table(current_unique)

message("CNEMC snapshot md5: ", snapshot_md5)
message(
  "CNEMC collected at: ",
  format(checked_at, "%Y-%m-%d %H:%M:%S %Z", tz = TIMEZONE)
)
message("CNEMC retention bucket: ", retention_bucket)
message("Current unique row versions: ", nrow(current_unique))

# ------------------------------------------------------------
# 4. Persistent backup directories and paths
# ------------------------------------------------------------

BACKUP_ROOT <- file.path(BACKUP_REPO, "cnemc_surfacewater")
STATE_DIR <- file.path(BACKUP_ROOT, "state")
MANIFEST_DIR <- file.path(BACKUP_ROOT, "manifests")
DICTIONARY_DIR <- file.path(BACKUP_ROOT, "dictionaries")
INDEX_DIR <- file.path(BACKUP_ROOT, "indexes")
DELTA_ROOT <- file.path(BACKUP_ROOT, "deltas")
BASELINE_DIR <- file.path(DELTA_ROOT, "baseline")

SNAPSHOT_REL_DIR <- file.path(
  "cnemc_surfacewater",
  "snapshots",
  year,
  month,
  day
)
SNAPSHOT_DIR <- file.path(BACKUP_REPO, SNAPSHOT_REL_DIR)

DELTA_REL_DIR <- file.path(
  "cnemc_surfacewater",
  "deltas",
  year,
  month,
  day
)
DELTA_DIR <- file.path(BACKUP_REPO, DELTA_REL_DIR)

for (d in c(
  STATE_DIR,
  MANIFEST_DIR,
  DICTIONARY_DIR,
  INDEX_DIR,
  DELTA_ROOT,
  BASELINE_DIR,
  SNAPSHOT_DIR,
  DELTA_DIR
)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

LATEST_HASH_FILE <- file.path(STATE_DIR, "latest_snapshot_md5.txt")
LATEST_FULL_BUCKET_FILE <- file.path(
  STATE_DIR,
  "latest_full_checkpoint_bucket.txt"
)
LATEST_MEMBERSHIP_FILE <- file.path(
  STATE_DIR,
  "latest_row_membership.csv"
)
DELTA_BASELINE_STATE_FILE <- file.path(
  STATE_DIR,
  "delta_baseline_path.txt"
)

ROW_VERSION_INDEX <- file.path(
  INDEX_DIR,
  "row_version_index.csv"
)

COLLECTION_MANIFEST <- file.path(
  MANIFEST_DIR,
  "collection_manifest.csv"
)
SNAPSHOT_MANIFEST <- file.path(
  MANIFEST_DIR,
  "snapshot_manifest.csv"
)
DELTA_MANIFEST <- file.path(
  MANIFEST_DIR,
  "delta_manifest.csv"
)

ROW_VERSION_DAILY_REL <- file.path(
  DELTA_REL_DIR,
  "row_versions.csv"
)
ROW_VERSION_DAILY <- file.path(
  BACKUP_REPO,
  ROW_VERSION_DAILY_REL
)

MEMBERSHIP_DAILY_REL <- file.path(
  DELTA_REL_DIR,
  "membership_events.csv"
)
MEMBERSHIP_DAILY <- file.path(
  BACKUP_REPO,
  MEMBERSHIP_DAILY_REL
)

# ------------------------------------------------------------
# 5. Content-addressed dictionaries
# ------------------------------------------------------------

area_river_md5 <- md5_file(AREA_RIVER_CURRENT)
area_river_name <- paste0("area_river_", area_river_md5, ".json")
area_river_destination <- file.path(DICTIONARY_DIR, area_river_name)
copy_new_file(AREA_RIVER_CURRENT, area_river_destination)

header_dictionary_md5 <- md5_file(HEADER_DICTIONARY)
header_dictionary_name <- paste0(
  "header_dictionary_",
  header_dictionary_md5,
  ".csv"
)
header_dictionary_destination <- file.path(
  DICTIONARY_DIR,
  header_dictionary_name
)
copy_new_file(HEADER_DICTIONARY, header_dictionary_destination)

# ------------------------------------------------------------
# 6. Detect source changes and full-checkpoint retention status
# ------------------------------------------------------------

previous_snapshot_md5 <- read_first_line(LATEST_HASH_FILE)
previous_full_bucket <- read_first_line(LATEST_FULL_BUCKET_FILE)

snapshot_changed <- (
  is.na(previous_snapshot_md5) ||
    !identical(previous_snapshot_md5, snapshot_md5)
)

retain_full_checkpoint <- (
  snapshot_changed &&
    (
      is.na(previous_full_bucket) ||
        !identical(previous_full_bucket, retention_bucket)
    )
)

message(
  "Previous snapshot md5: ",
  ifelse(is.na(previous_snapshot_md5), "<none>", previous_snapshot_md5)
)
message("Snapshot changed: ", snapshot_changed)
message(
  "Previous full checkpoint bucket: ",
  ifelse(is.na(previous_full_bucket), "<none>", previous_full_bucket)
)
message("Retain full checkpoint this run: ", retain_full_checkpoint)

# ------------------------------------------------------------
# 7. Collection identity and current scraper run metadata
# ------------------------------------------------------------

current_run <- read_last_manifest_row(RUN_MANIFEST)

github_run_id <- Sys.getenv("GITHUB_RUN_ID", unset = "")
github_run_attempt <- Sys.getenv("GITHUB_RUN_ATTEMPT", unset = "")
collector_id <- Sys.getenv("TELITI_COLLECTOR_ID", unset = "github_actions")

collection_key <- if (nzchar(github_run_id)) {
  paste0(github_run_id, "-", github_run_attempt)
} else {
  paste0(stamp, "-", hash_short)
}

collected_at_text <- format(
  checked_at,
  "%Y-%m-%d %H:%M:%S%z",
  tz = TIMEZONE
)

# ------------------------------------------------------------
# 8. Initialize / load row-version archive index
# ------------------------------------------------------------

index_missing <- !file.exists(ROW_VERSION_INDEX)

if (index_missing) {
  row_index <- seed_index_from_retained_checkpoints(
    SNAPSHOT_MANIFEST,
    BACKUP_REPO
  )
  historic_seed_n <- nrow(row_index)
} else {
  row_index <- suppressMessages(
    readr::read_csv(
      ROW_VERSION_INDEX,
      show_col_types = FALSE,
      progress = FALSE
    )
  )
  row_index <- as.data.frame(row_index, stringsAsFactors = FALSE)
  historic_seed_n <- 0L
}

index_required <- c("row_hash", "observation_key_hash")
index_missing_cols <- setdiff(index_required, names(row_index))

if (length(index_missing_cols) > 0L) {
  stop(
    "Row-version index is missing required column(s): ",
    paste(index_missing_cols, collapse = ", ")
  )
}

row_index$row_hash <- as.character(row_index$row_hash)
row_index$observation_key_hash <- as.character(
  row_index$observation_key_hash
)

known_row_hashes_before <- unique(row_index$row_hash)
known_observation_keys_before <- unique(row_index$observation_key_hash)

# ------------------------------------------------------------
# 9. One-time delta baseline
# ------------------------------------------------------------

baseline_relative_path <- read_first_line(DELTA_BASELINE_STATE_FILE)
baseline_missing <- is.na(baseline_relative_path)

if (baseline_missing) {
  baseline_filename <- paste0(
    "cnemc_delta_baseline_",
    stamp,
    "_",
    hash_short,
    ".csv.gz"
  )

  baseline_relative_path <- file.path(
    "cnemc_surfacewater",
    "deltas",
    "baseline",
    baseline_filename
  )

  baseline_destination <- file.path(
    BACKUP_REPO,
    baseline_relative_path
  )

  copy_new_file(CURRENT_PROCESSED, baseline_destination)

  writeLines(
    baseline_relative_path,
    con = DELTA_BASELINE_STATE_FILE,
    useBytes = TRUE
  )

  message("Created one-time CNEMC delta baseline: ", baseline_relative_path)
}

membership_missing <- !file.exists(LATEST_MEMBERSHIP_FILE)
delta_bootstrap <- index_missing || baseline_missing || membership_missing

# ------------------------------------------------------------
# 10. Identify previously unseen row versions
# ------------------------------------------------------------

new_version_mask <- !(
  current_unique$row_hash %in% known_row_hashes_before
)
new_rows <- current_unique[new_version_mask, , drop = FALSE]

if (nrow(new_rows) > 0L) {
  new_rows$delta_archived_at <- collected_at_text
  new_rows$delta_snapshot_md5 <- snapshot_md5
  new_rows$delta_collection_key <- collection_key
  new_rows$delta_class <- ifelse(
    new_rows$observation_key_hash %in% known_observation_keys_before,
    "revised_observation_version",
    "new_observation_key"
  )

  metadata_cols <- c(
    "delta_archived_at",
    "delta_snapshot_md5",
    "delta_collection_key",
    "delta_class"
  )

  new_rows <- new_rows[
    c(metadata_cols, setdiff(names(new_rows), metadata_cols)),
    drop = FALSE
  ]
}

new_observation_keys_n <- if (nrow(new_rows) > 0L) {
  length(unique(
    new_rows$observation_key_hash[
      !(new_rows$observation_key_hash %in% known_observation_keys_before)
    ]
  ))
} else {
  0L
}

revised_row_versions_n <- if (nrow(new_rows) > 0L) {
  sum(
    new_rows$observation_key_hash %in% known_observation_keys_before
  )
} else {
  0L
}

row_versions_written_rel <- ""

if (nrow(new_rows) > 0L) {
  if (delta_bootstrap) {
    # The one-time baseline already contains the complete current rows.
    row_versions_written_rel <- baseline_relative_path
    message(
      "Delta bootstrap: ",
      nrow(new_rows),
      " current row version(s) not represented in retained historic checkpoints; ",
      "their full rows are already preserved in the delta baseline."
    )
  } else {
    append_csv_utf8(new_rows, ROW_VERSION_DAILY)
    row_versions_written_rel <- ROW_VERSION_DAILY_REL

    message(
      "Appended ",
      nrow(new_rows),
      " previously unseen row version(s) to ",
      ROW_VERSION_DAILY_REL
    )
  }
}

# ------------------------------------------------------------
# 11. Append newly represented versions to row-version index
# ------------------------------------------------------------

new_index_rows <- empty_row_version_index()

if (nrow(new_rows) > 0L) {
  new_index_rows <- data.frame(
    row_hash = as.character(new_rows$row_hash),
    observation_key_hash = as.character(new_rows$observation_key_hash),
    first_archived_at = collected_at_text,
    first_snapshot_md5 = snapshot_md5,
    first_collection_key = collection_key,
    archive_origin = if (delta_bootstrap) {
      "delta_baseline"
    } else {
      "row_version_delta"
    },
    row_versions_file = row_versions_written_rel,
    stringsAsFactors = FALSE
  )

  new_index_rows <- new_index_rows[
    !duplicated(new_index_rows$row_hash),
    ,
    drop = FALSE
  ]
}

if (index_missing) {
  combined_index <- rbind(row_index, new_index_rows)
  combined_index <- combined_index[
    !duplicated(combined_index$row_hash),
    ,
    drop = FALSE
  ]

  readr::write_csv(combined_index, ROW_VERSION_INDEX, na = "")

  message(
    "Initialized row-version index with ",
    nrow(combined_index),
    " unique row version(s)."
  )
} else if (nrow(new_index_rows) > 0L) {
  append_csv_utf8(new_index_rows, ROW_VERSION_INDEX)
  message(
    "Extended row-version index by ",
    nrow(new_index_rows),
    " row version(s)."
  )
}

known_row_versions_after <- length(unique(c(
  known_row_hashes_before,
  new_index_rows$row_hash
)))

# ------------------------------------------------------------
# 12. Row-membership changes between successive source states
# ------------------------------------------------------------

previous_membership <- read_csv_if_exists(LATEST_MEMBERSHIP_FILE)

if (is.null(previous_membership)) {
  previous_membership <- data.frame(
    row_hash = character(),
    observation_key_hash = character(),
    stringsAsFactors = FALSE
  )
  membership_bootstrap <- TRUE
} else {
  previous_membership <- clean_hash_table(previous_membership)
  membership_bootstrap <- FALSE
}

membership_events <- data.frame(
  collected_at = character(),
  snapshot_md5 = character(),
  collection_key = character(),
  event_type = character(),
  row_hash = character(),
  observation_key_hash = character(),
  version_status = character(),
  stringsAsFactors = FALSE
)

membership_added_n <- 0L
membership_removed_n <- 0L

if (!membership_bootstrap && snapshot_changed) {
  added_hashes <- setdiff(
    current_membership$row_hash,
    previous_membership$row_hash
  )
  removed_hashes <- setdiff(
    previous_membership$row_hash,
    current_membership$row_hash
  )

  membership_added_n <- length(added_hashes)
  membership_removed_n <- length(removed_hashes)

  if (membership_added_n > 0L) {
    added <- current_membership[
      match(added_hashes, current_membership$row_hash),
      ,
      drop = FALSE
    ]

    added_events <- data.frame(
      collected_at = collected_at_text,
      snapshot_md5 = snapshot_md5,
      collection_key = collection_key,
      event_type = "added_to_current_state",
      row_hash = added$row_hash,
      observation_key_hash = added$observation_key_hash,
      version_status = ifelse(
        added$row_hash %in% known_row_hashes_before,
        "reappeared_known_version",
        "new_row_version"
      ),
      stringsAsFactors = FALSE
    )

    membership_events <- rbind(membership_events, added_events)
  }

  if (membership_removed_n > 0L) {
    removed <- previous_membership[
      match(removed_hashes, previous_membership$row_hash),
      ,
      drop = FALSE
    ]

    removed_events <- data.frame(
      collected_at = collected_at_text,
      snapshot_md5 = snapshot_md5,
      collection_key = collection_key,
      event_type = "removed_from_current_state",
      row_hash = removed$row_hash,
      observation_key_hash = removed$observation_key_hash,
      version_status = "known_version_removed",
      stringsAsFactors = FALSE
    )

    membership_events <- rbind(membership_events, removed_events)
  }
}

membership_events_written_rel <- ""

if (nrow(membership_events) > 0L) {
  append_csv_utf8(membership_events, MEMBERSHIP_DAILY)
  membership_events_written_rel <- MEMBERSHIP_DAILY_REL

  message(
    "Appended ",
    nrow(membership_events),
    " membership event(s): added=",
    membership_added_n,
    "; removed=",
    membership_removed_n
  )
}

if (membership_bootstrap || snapshot_changed) {
  readr::write_csv(
    current_membership,
    LATEST_MEMBERSHIP_FILE,
    na = ""
  )

  if (membership_bootstrap) {
    message(
      "Initialized latest row-membership state with ",
      nrow(current_membership),
      " row version(s)."
    )
  } else {
    message("Updated latest row-membership state.")
  }
}

# ------------------------------------------------------------
# 13. Append lightweight collection manifest for EVERY run
# ------------------------------------------------------------

collection_entry <- data.frame(
  collection_key = collection_key,
  collected_at = collected_at_text,
  snapshot_md5 = snapshot_md5,
  changed = snapshot_changed,
  rows = get_field(current_run, "rows", NA_integer_),
  unique_row_hashes = get_field(current_run, "unique_row_hashes", NA_integer_),
  total_pages = get_field(current_run, "total_pages", NA_integer_),
  page_size = get_field(current_run, "page_size", NA_integer_),
  collector_id = collector_id,
  github_run_id = github_run_id,
  github_run_attempt = github_run_attempt,
  scraper_code_commit = Sys.getenv("GITHUB_SHA", unset = ""),
  retention_bucket = retention_bucket,
  full_checkpoint_retained = retain_full_checkpoint,
  stringsAsFactors = FALSE
)

collection_exists <- file.exists(COLLECTION_MANIFEST)
collection_already_recorded <- FALSE

if (collection_exists) {
  old_collection <- suppressMessages(
    readr::read_csv(
      COLLECTION_MANIFEST,
      show_col_types = FALSE,
      progress = FALSE
    )
  )

  if ("collection_key" %in% names(old_collection)) {
    collection_already_recorded <- collection_key %in%
      as.character(old_collection$collection_key)
  }
}

if (!collection_already_recorded) {
  append_csv_utf8(collection_entry, COLLECTION_MANIFEST)
  message("Updated collection manifest: ", COLLECTION_MANIFEST)
} else {
  message(
    "Collection manifest already contains key ",
    collection_key,
    "; no duplicate row appended."
  )
}

# ------------------------------------------------------------
# 14. Handle full checkpoint for changed source state
# ------------------------------------------------------------

raw_relative_path <- NA_character_
processed_relative_path <- NA_character_

if (!snapshot_changed) {
  message("Snapshot already represented in backup state; no new snapshot-manifest row required.")
} else {
  if (retain_full_checkpoint) {
    snapshot_stem <- paste0(
      "cnemc_surfacewater_",
      stamp,
      "_",
      hash_short
    )

    raw_filename <- paste0(snapshot_stem, "_raw.rds")
    processed_filename <- paste0(snapshot_stem, "_processed.csv.gz")

    raw_relative_path <- file.path(SNAPSHOT_REL_DIR, raw_filename)
    processed_relative_path <- file.path(
      SNAPSHOT_REL_DIR,
      processed_filename
    )

    raw_destination <- file.path(BACKUP_REPO, raw_relative_path)
    processed_destination <- file.path(
      BACKUP_REPO,
      processed_relative_path
    )

    if (file.exists(raw_destination) || file.exists(processed_destination)) {
      stop(
        paste0(
          "Full checkpoint destination already exists while state differs.\n",
          "Refusing to overwrite an append-only backup."
        )
      )
    }

    copy_new_file(CURRENT_RAW, raw_destination)
    copy_new_file(CURRENT_PROCESSED, processed_destination)

    writeLines(
      retention_bucket,
      con = LATEST_FULL_BUCKET_FILE,
      useBytes = TRUE
    )

    message(
      "Stored full CNEMC checkpoint for retention bucket ",
      retention_bucket
    )
  } else {
    message(
      "Changed CNEMC snapshot recorded without another full checkpoint; ",
      "row-version deltas and membership events preserve scientific changes."
    )
  }

  manifest_entry <- data.frame(
    collected_at = collected_at_text,
    snapshot_md5 = snapshot_md5,
    rows = get_field(current_run, "rows", NA_integer_),
    total_pages = get_field(current_run, "total_pages", NA_integer_),
    page_size = get_field(current_run, "page_size", NA_integer_),
    collector_id = collector_id,
    github_run_id = github_run_id,
    github_run_attempt = github_run_attempt,
    scraper_code_commit = Sys.getenv("GITHUB_SHA", unset = ""),
    raw_file = ifelse(is.na(raw_relative_path), "", raw_relative_path),
    processed_file = ifelse(
      is.na(processed_relative_path),
      "",
      processed_relative_path
    ),
    area_river_md5 = area_river_md5,
    header_dictionary_md5 = header_dictionary_md5,
    stringsAsFactors = FALSE
  )

  append_csv_utf8(manifest_entry, SNAPSHOT_MANIFEST)
  message("Updated snapshot manifest: ", SNAPSHOT_MANIFEST)

  writeLines(snapshot_md5, con = LATEST_HASH_FILE, useBytes = TRUE)
  message("Updated latest snapshot state: ", LATEST_HASH_FILE)
}

# ------------------------------------------------------------
# 15. Append row-delta manifest for bootstrap or changed state
# ------------------------------------------------------------

if (delta_bootstrap || snapshot_changed) {
  delta_entry <- data.frame(
    collection_key = collection_key,
    collected_at = collected_at_text,
    snapshot_md5 = snapshot_md5,
    retention_bucket = retention_bucket,
    delta_bootstrap = delta_bootstrap,
    historic_index_seeded = historic_seed_n,
    new_row_versions = nrow(new_rows),
    new_observation_keys = new_observation_keys_n,
    revised_row_versions = revised_row_versions_n,
    membership_added = membership_added_n,
    membership_removed = membership_removed_n,
    known_row_versions_after = known_row_versions_after,
    baseline_file = baseline_relative_path,
    row_versions_file = row_versions_written_rel,
    membership_events_file = membership_events_written_rel,
    stringsAsFactors = FALSE
  )

  append_csv_utf8(delta_entry, DELTA_MANIFEST)
  message("Updated row-delta manifest: ", DELTA_MANIFEST)
}

# ------------------------------------------------------------
# 16. Summary
# ------------------------------------------------------------

message("")
message("CNEMC persistent-backup preparation complete.")
message("Snapshot changed: ", snapshot_changed)
message("Full checkpoint retained: ", retain_full_checkpoint)
message("Retention bucket: ", retention_bucket)
message("Delta bootstrap: ", delta_bootstrap)
message("Historic row versions seeded: ", historic_seed_n)
message("New row versions archived this run: ", nrow(new_rows))
message("  New observation keys: ", new_observation_keys_n)
message("  Revised observation versions: ", revised_row_versions_n)
message("Membership events: +", membership_added_n, " / -", membership_removed_n)
message("Known row versions after run: ", known_row_versions_after)
message("Collection manifest: ", COLLECTION_MANIFEST)
message("Delta manifest: ", DELTA_MANIFEST)
message("Backup repository: ", BACKUP_REPO)