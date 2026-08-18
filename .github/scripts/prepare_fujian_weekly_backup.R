# ============================================================================
# Prepare Fujian weekly surface-water output for persistent Git backup
#
# Design:
# - Persist canonical year files + metadata as restart state.
# - Persist .checkpoints so an interrupted historical backfill can resume.
# - Store changed yearly snapshots append-only.
# - Do not persist the growing processed master; it is rebuildable from sources.
# ============================================================================

options(stringsAsFactors = FALSE)

DATA_ROOT <- Sys.getenv("TELITI_DATA_ROOT", unset = "")
BACKUP_REPO <- Sys.getenv("TELITI_BACKUP_REPO", unset = "")
TIMEZONE <- Sys.getenv("TELITI_TIMEZONE", unset = "Asia/Taipei")
COLLECTOR_ID <- Sys.getenv("TELITI_COLLECTOR_ID", unset = "github_actions")

if (!nzchar(DATA_ROOT)) {
  stop("TELITI_DATA_ROOT is not defined.")
}

if (!nzchar(BACKUP_REPO)) {
  stop("TELITI_BACKUP_REPO is not defined.")
}

if (!dir.exists(file.path(BACKUP_REPO, ".git"))) {
  stop("TELITI_BACKUP_REPO is not a Git repository: ", BACKUP_REPO)
}

PROJECT_DIR <- file.path(DATA_ROOT, "fujian_surfacewater")
SOURCE_DIR <- file.path(PROJECT_DIR, "data", "source")
ARCHIVE_DIR <- file.path(SOURCE_DIR, "archive")
CHECKPOINT_DIR <- file.path(SOURCE_DIR, ".checkpoints")
PROCESSED_DIR <- file.path(PROJECT_DIR, "data", "processed")
PROCESSED_MANIFEST <- file.path(
  PROCESSED_DIR,
  "fujian_weekly_surfacewater_manifest.csv"
)
BACKEND_DISCOVERY <- file.path(
  SOURCE_DIR,
  "fujian_weekly_backend_discovery.rds"
)

if (!dir.exists(SOURCE_DIR)) {
  stop("Fujian source directory does not exist: ", SOURCE_DIR)
}

BACKUP_ROOT <- file.path(BACKUP_REPO, "fujian_weekly_surfacewater")
STATE_DIR <- file.path(BACKUP_ROOT, "state")
STATE_SOURCE_DIR <- file.path(STATE_DIR, "source")
STATE_CHECKPOINT_DIR <- file.path(STATE_DIR, "checkpoints")
SNAPSHOT_ROOT <- file.path(BACKUP_ROOT, "snapshots")
MANIFEST_DIR <- file.path(BACKUP_ROOT, "manifests")
SNAPSHOT_MANIFEST <- file.path(MANIFEST_DIR, "snapshot_manifest.csv")

for (d in c(STATE_SOURCE_DIR, SNAPSHOT_ROOT, MANIFEST_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

md5_file <- function(path) {
  unname(as.character(tools::md5sum(path)))
}

copy_replace <- function(source, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  ok <- file.copy(
    from = source,
    to = destination,
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = FALSE
  )

  if (!isTRUE(ok)) {
    stop("Failed to copy: ", source, " -> ", destination)
  }

  invisible(destination)
}

copy_append_only <- function(source, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(destination)) {
    source_md5 <- md5_file(source)
    destination_md5 <- md5_file(destination)

    if (!identical(source_md5, destination_md5)) {
      stop(
        "Append-only destination already exists with different content: ",
        destination
      )
    }

    message("Already present: ", destination)
    return(FALSE)
  }

  ok <- file.copy(
    from = source,
    to = destination,
    overwrite = FALSE,
    copy.mode = TRUE,
    copy.date = FALSE
  )

  if (!isTRUE(ok)) {
    stop("Failed to copy: ", source, " -> ", destination)
  }

  message("Added: ", destination)
  TRUE
}

sync_directory <- function(source_dir, destination_dir) {
  if (dir.exists(destination_dir)) {
    unlink(destination_dir, recursive = TRUE, force = TRUE)
  }

  if (!dir.exists(source_dir)) {
    return(0L)
  }

  files <- list.files(
    source_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )

  if (length(files) == 0L) {
    return(0L)
  }

  source_prefix <- paste0(normalizePath(source_dir, winslash = "/"), "/")

  for (source in files) {
    source_norm <- normalizePath(source, winslash = "/", mustWork = TRUE)
    relative <- substring(source_norm, nchar(source_prefix) + 1L)
    destination <- file.path(destination_dir, relative)
    copy_replace(source, destination)
  }

  length(files)
}

append_manifest_row <- function(row) {
  exists <- file.exists(SNAPSHOT_MANIFEST)

  utils::write.table(
    row,
    file = SNAPSHOT_MANIFEST,
    sep = ",",
    row.names = FALSE,
    col.names = !exists,
    append = exists,
    quote = TRUE,
    qmethod = "double",
    na = "",
    fileEncoding = "UTF-8"
  )
}

# ----------------------------------------------------------------------------
# 1. Persist canonical state used by subsequent GitHub runs
# ----------------------------------------------------------------------------

canonical_files <- list.files(
  SOURCE_DIR,
  pattern = "^fujian_weekly_[0-9]{4}(_meta\\.rds|\\.rds)$",
  full.names = TRUE
)

for (source in sort(canonical_files)) {
  copy_replace(
    source,
    file.path(STATE_SOURCE_DIR, basename(source))
  )
}

message("Persistent Fujian canonical state files: ", length(canonical_files))

if (file.exists(BACKEND_DISCOVERY)) {
  copy_replace(
    BACKEND_DISCOVERY,
    file.path(STATE_DIR, "fujian_weekly_backend_discovery.rds")
  )
}

if (file.exists(PROCESSED_MANIFEST)) {
  copy_replace(
    PROCESSED_MANIFEST,
    file.path(STATE_DIR, "fujian_weekly_surfacewater_manifest.csv")
  )
}

checkpoint_count <- sync_directory(
  CHECKPOINT_DIR,
  STATE_CHECKPOINT_DIR
)

message("Persistent Fujian checkpoint files: ", checkpoint_count)

# ----------------------------------------------------------------------------
# 2. Persist changed source snapshots append-only
# ----------------------------------------------------------------------------

archive_files <- if (dir.exists(ARCHIVE_DIR)) {
  list.files(
    ARCHIVE_DIR,
    pattern = "^fujian_weekly_[0-9]{4}_[0-9]{8}_[0-9]{6}\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )
} else {
  character()
}

new_snapshot_count <- 0L

for (source in sort(archive_files)) {
  filename <- basename(source)

  match <- regexec(
    "^fujian_weekly_([0-9]{4})_([0-9]{8})_([0-9]{6})\\.rds$",
    filename
  )
  parts <- regmatches(filename, match)[[1]]

  if (length(parts) != 4L) {
    stop("Unexpected Fujian archive filename: ", filename)
  }

  year <- parts[[2]]
  date_part <- parts[[3]]
  time_part <- parts[[4]]

  meta_file <- file.path(
    SOURCE_DIR,
    sprintf("fujian_weekly_%s_meta.rds", year)
  )

  meta <- if (file.exists(meta_file)) {
    tryCatch(readRDS(meta_file), error = function(e) NULL)
  } else {
    NULL
  }

  source_snapshot_md5 <- if (!is.null(meta$snapshot_md5)) {
    as.character(meta$snapshot_md5)
  } else {
    NA_character_
  }

  archive_md5 <- md5_file(source)
  identity_hash <- if (
    length(source_snapshot_md5) == 1L &&
      !is.na(source_snapshot_md5) &&
      nzchar(source_snapshot_md5)
  ) {
    source_snapshot_md5
  } else {
    archive_md5
  }

  hash_short <- substr(identity_hash, 1L, 12L)

  persistent_name <- sprintf(
    "fujian_weekly_%s_%s_%s_%s.rds",
    year,
    date_part,
    time_part,
    hash_short
  )

  relative_path <- file.path(
    "fujian_weekly_surfacewater",
    "snapshots",
    year,
    persistent_name
  )

  destination <- file.path(BACKUP_REPO, relative_path)
  added <- copy_append_only(source, destination)

  if (isTRUE(added)) {
    new_snapshot_count <- new_snapshot_count + 1L

    collected_at <- as.POSIXct(
      paste0(date_part, time_part),
      format = "%Y%m%d%H%M%S",
      tz = TIMEZONE
    )

    row <- data.frame(
      collected_at = format(
        collected_at,
        "%Y-%m-%d %H:%M:%S%z",
        tz = TIMEZONE
      ),
      source_year = as.integer(year),
      rows = if (!is.null(meta$rows)) as.integer(meta$rows) else NA_integer_,
      source_snapshot_md5 = source_snapshot_md5,
      archive_file_md5 = archive_md5,
      source_mode = if (!is.null(meta$source_mode)) as.character(meta$source_mode) else NA_character_,
      backend_url = if (!is.null(meta$backend_url)) as.character(meta$backend_url) else NA_character_,
      collector_id = if (!is.null(meta$collector_id)) as.character(meta$collector_id) else COLLECTOR_ID,
      github_run_id = if (!is.null(meta$github_run_id)) as.character(meta$github_run_id) else Sys.getenv("GITHUB_RUN_ID", unset = ""),
      github_run_attempt = if (!is.null(meta$github_run_attempt)) as.character(meta$github_run_attempt) else Sys.getenv("GITHUB_RUN_ATTEMPT", unset = ""),
      scraper_code_commit = if (!is.null(meta$scraper_code_commit)) as.character(meta$scraper_code_commit) else Sys.getenv("GITHUB_SHA", unset = ""),
      snapshot_file = relative_path,
      stringsAsFactors = FALSE
    )

    append_manifest_row(row)
  }
}

message("New persistent Fujian yearly snapshots: ", new_snapshot_count)
message("Fujian persistent-backup preparation complete.")