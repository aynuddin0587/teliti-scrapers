# ============================================================
# Prepare NMEMC marine-water output for persistent Git backup
# ============================================================

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

RAW_DIR <- file.path(DATA_ROOT, "nmemc", "data", "raw")
ARCHIVE_DIR <- file.path(RAW_DIR, "archive")
PROCESSED_DIR <- file.path(DATA_ROOT, "nmemc", "data", "processed")
RUN_MANIFEST <- file.path(PROCESSED_DIR, "nmemc_water_manifest.csv")

if (!dir.exists(RAW_DIR)) {
  stop("NMEMC raw directory does not exist: ", RAW_DIR)
}

BACKUP_ROOT <- file.path(BACKUP_REPO, "nmemc_marine")
STATE_DIR <- file.path(BACKUP_ROOT, "state")
STATE_RAW_DIR <- file.path(STATE_DIR, "raw")
SNAPSHOT_ROOT <- file.path(BACKUP_ROOT, "snapshots")
MANIFEST_DIR <- file.path(BACKUP_ROOT, "manifests")
SNAPSHOT_MANIFEST <- file.path(MANIFEST_DIR, "snapshot_manifest.csv")

for (d in c(STATE_RAW_DIR, SNAPSHOT_ROOT, MANIFEST_DIR)) {
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

# ------------------------------------------------------------
# 1. Persist canonical state used by later GitHub runs
# ------------------------------------------------------------
canonical_files <- list.files(
  RAW_DIR,
  pattern = "^water[0-9]{4}(_meta\\.rds|\\.json)$",
  full.names = TRUE
)

if (length(canonical_files) == 0L) {
  stop("No NMEMC canonical state files were generated.")
}

for (source in canonical_files) {
  destination <- file.path(
    STATE_RAW_DIR,
    basename(source)
  )

  copy_replace(source, destination)
}

message(
  "Persistent canonical state files: ",
  length(canonical_files)
)

# Keep a lightweight processed manifest for inspection/recovery.
if (file.exists(RUN_MANIFEST)) {
  copy_replace(
    RUN_MANIFEST,
    file.path(STATE_DIR, "nmemc_water_manifest.csv")
  )
}

# ------------------------------------------------------------
# 2. Persist only new source snapshots from this runner
# ------------------------------------------------------------
archive_files <- if (dir.exists(ARCHIVE_DIR)) {
  list.files(
    ARCHIVE_DIR,
    pattern = "^water[0-9]{4}_[0-9]{8}_[0-9]{6}\\.json$",
    full.names = TRUE
  )
} else {
  character()
}

new_snapshot_count <- 0L

for (source in sort(archive_files)) {
  filename <- basename(source)

  match <- regexec(
    "^water([0-9]{4})_([0-9]{8})_([0-9]{6})\\.json$",
    filename
  )

  parts <- regmatches(filename, match)[[1]]

  if (length(parts) != 4L) {
    stop("Unexpected NMEMC archive filename: ", filename)
  }

  year <- parts[[2]]
  date_part <- parts[[3]]
  time_part <- parts[[4]]

  collected_at <- as.POSIXct(
    paste0(date_part, time_part),
    format = "%Y%m%d%H%M%S",
    tz = TIMEZONE
  )

  snapshot_md5 <- md5_file(source)
  hash_short <- substr(snapshot_md5, 1L, 12L)

  persistent_name <- sprintf(
    "water%s_%s_%s_%s.json",
    year,
    date_part,
    time_part,
    hash_short
  )

  relative_path <- file.path(
    "nmemc_marine",
    "snapshots",
    year,
    persistent_name
  )

  destination <- file.path(
    BACKUP_REPO,
    relative_path
  )

  added <- copy_append_only(source, destination)

  if (isTRUE(added)) {
    new_snapshot_count <- new_snapshot_count + 1L

    row <- data.frame(
      collected_at = format(
        collected_at,
        "%Y-%m-%d %H:%M:%S%z",
        tz = TIMEZONE
      ),
      source_year = as.integer(year),
      snapshot_md5 = snapshot_md5,
      bytes = file.info(source)$size,
      collector_id = COLLECTOR_ID,
      github_run_id = Sys.getenv("GITHUB_RUN_ID", unset = ""),
      github_run_attempt = Sys.getenv("GITHUB_RUN_ATTEMPT", unset = ""),
      scraper_code_commit = Sys.getenv("GITHUB_SHA", unset = ""),
      snapshot_file = relative_path,
      stringsAsFactors = FALSE
    )

    append_manifest_row(row)
  }
}

message("New persistent NMEMC marine snapshots: ", new_snapshot_count)
message("NMEMC marine persistent-backup preparation complete.")