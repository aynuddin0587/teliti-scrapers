# =============================================================================
# 08_reconcile_onlimo_historical.R
#
# Reconcile ONLIMO historical Pollution Index data collected independently by:
#   1) the Windows/local PC collector, and
#   2) the GitHub Actions backup collector.
#
# Design principles:
# - The scientific observation key is station_id + date.
# - The primary scientific payload is pollution_index.
# - GitHub may store append-only/partitioned files, so all compatible backup
#   partitions are discovered recursively and reconstructed before comparison.
# - The GitHub request ledger defines which station x 7-day blocks have actually
#   completed. PC-only rows are judged only inside those completed GitHub blocks;
#   unfinished GitHub catch-up is not treated as a collector failure.
# - A GitHub-only observation is reported as a recovery gain, not automatically
#   treated as failure. The critical failure mode is a PC observation missing from
#   a GitHub block that GitHub says completed successfully.
# - Request-ledger history itself need not be byte-for-byte identical between PC
#   and GitHub because the two collectors have different run histories.
#
# Expected local files:
#   D:/# R Project/penelitian/onlimo/data/
#     onlimo_pollution_index_historical.csv
#     onlimo_pollution_index_request_ledger.csv
#
# Expected GitHub backup root:
#   D:/# R Project/teliti-data-backup/onlimo_pollution_index/
#
# Outputs:
#   teliti_reconciliation/output/onlimo_historical/
#     onlimo_historical_github_file_inventory.csv
#     onlimo_historical_block_audit.csv
#     onlimo_historical_key_exceptions.csv
#     onlimo_historical_station_coverage.csv
#     onlimo_historical_reconciliation_summary.txt
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(tidyr)
})

options(stringsAsFactors = FALSE)

# =============================================================================
# 1. CONFIGURATION
# =============================================================================

PC_DATA_FILE <- Sys.getenv(
  "TELITI_ONLIMO_PC_HISTORICAL",
  unset = "D:/# R Project/penelitian/onlimo/data/onlimo_pollution_index_historical.csv"
)

PC_LEDGER_FILE <- Sys.getenv(
  "TELITI_ONLIMO_PC_HISTORICAL_LEDGER",
  unset = "D:/# R Project/penelitian/onlimo/data/onlimo_pollution_index_request_ledger.csv"
)

GITHUB_ROOT <- Sys.getenv(
  "TELITI_ONLIMO_GITHUB_HISTORICAL_ROOT",
  unset = "D:/# R Project/teliti-data-backup/onlimo_pollution_index"
)

OUTPUT_DIR <- Sys.getenv(
  "TELITI_RECON_OUTPUT_DIR",
  unset = "D:/# R Project/penelitian/teliti_reconciliation/output/onlimo_historical"
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

FILE_INVENTORY_OUT <- file.path(
  OUTPUT_DIR, "onlimo_historical_github_file_inventory.csv"
)
BLOCK_AUDIT_OUT <- file.path(
  OUTPUT_DIR, "onlimo_historical_block_audit.csv"
)
EXCEPTIONS_OUT <- file.path(
  OUTPUT_DIR, "onlimo_historical_key_exceptions.csv"
)
STATION_COVERAGE_OUT <- file.path(
  OUTPUT_DIR, "onlimo_historical_station_coverage.csv"
)
SUMMARY_OUT <- file.path(
  OUTPUT_DIR, "onlimo_historical_reconciliation_summary.txt"
)

# Pollution Index is numeric in the source response. Canonical comparison uses
# 12 significant digits, far finer than the source's practical reporting precision,
# while avoiding false differences caused only by CSV floating-point rendering.
IP_SIGNIFICANT_DIGITS <- 12L

# =============================================================================
# 2. HELPERS
# =============================================================================

msg <- function(...) cat(paste0(...), "\n", sep = "")

trim_na <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

safe_chr <- function(df, nm) {
  if (nm %in% names(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))
}

safe_num <- function(df, nm) {
  if (nm %in% names(df)) suppressWarnings(as.numeric(df[[nm]])) else rep(NA_real_, nrow(df))
}

safe_date <- function(df, nm) {
  if (nm %in% names(df)) suppressWarnings(as.Date(df[[nm]])) else as.Date(rep(NA_character_, nrow(df)))
}

canonical_ip_token <- function(x) {
  out <- rep("<NA>", length(x))
  ok <- !is.na(x)
  if (any(ok)) out[ok] <- sprintf(paste0("%.", IP_SIGNIFICANT_DIGITS, "g"), x[ok])
  out
}

classify_ip <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x <= 1 ~ "MEMENUHI BAKUMUTU",
    x < 5 ~ "CEMAR RINGAN",
    x < 10 ~ "CEMAR SEDANG",
    TRUE ~ "CEMAR BERAT"
  )
}

make_key <- function(station_id, date) {
  paste(station_id, format(as.Date(date), "%Y-%m-%d"), sep = "|")
}

is_supported_file <- function(path) {
  grepl("\\.(csv|csv\\.gz|rds)$", tolower(path))
}

read_tabular <- function(path, n_max = Inf) {
  if (grepl("\\.rds$", tolower(path))) {
    x <- readRDS(path)
    if (!inherits(x, "data.frame")) {
      stop("RDS object is not a data frame: ", path, call. = FALSE)
    }
    x <- tibble::as_tibble(x)
    if (is.finite(n_max)) x <- utils::head(x, n_max)
    return(x)
  }

  readr::read_csv(
    path,
    n_max = n_max,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
}

file_columns <- function(path) {
  tryCatch(
    names(read_tabular(path, n_max = 0L)),
    error = function(e) character()
  )
}

classify_backup_file <- function(path) {
  nms <- file_columns(path)

  if (all(c("station_id", "date", "pollution_index") %in% nms)) {
    return("historical_data")
  }

  if (all(c("station_id", "block_start", "block_end", "request_status") %in% nms)) {
    return("request_ledger")
  }

  "other"
}

normalize_observations <- function(df, origin, source_file) {
  required <- c("station_id", "date", "pollution_index")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    stop(
      "Observation file is missing required columns: ",
      paste(missing, collapse = ", "),
      " | ", source_file,
      call. = FALSE
    )
  }

  out <- tibble(
    station_id = trim_na(df$station_id),
    date = suppressWarnings(as.Date(df$date)),
    pollution_index = suppressWarnings(as.numeric(df$pollution_index)),
    pollution_status_derived_source = safe_chr(df, "pollution_status_derived"),
    station_name = safe_chr(df, "station_name"),
    river = safe_chr(df, "river"),
    watershed = safe_chr(df, "watershed"),
    location_category = safe_chr(df, "location_category"),
    river_segment = safe_chr(df, "river_segment"),
    station_category = safe_chr(df, "station_category"),
    kabupaten_kota = safe_chr(df, "kabupaten_kota"),
    province = safe_chr(df, "province"),
    latitude = safe_num(df, "latitude"),
    longitude = safe_num(df, "longitude"),
    source_endpoint = safe_chr(df, "source_endpoint"),
    retrieved_at = safe_chr(df, "retrieved_at"),
    collector_origin = origin,
    source_file = source_file
  ) |>
    filter(!is.na(station_id), !is.na(date)) |>
    mutate(
      observation_key = make_key(station_id, date),
      ip_token = canonical_ip_token(pollution_index),
      pollution_status_recomputed = classify_ip(pollution_index)
    )

  out
}

normalize_ledger <- function(df, origin, source_file, file_mtime = NA_character_) {
  required <- c("station_id", "block_start", "block_end", "request_status")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    stop(
      "Ledger file is missing required columns: ",
      paste(missing, collapse = ", "),
      " | ", source_file,
      call. = FALSE
    )
  }

  tibble(
    station_id = trim_na(df$station_id),
    block_start = safe_date(df, "block_start"),
    block_end = safe_date(df, "block_end"),
    request_status = trim_na(df$request_status),
    n_rows = suppressWarnings(as.integer(safe_num(df, "n_rows"))),
    attempted_at = safe_chr(df, "attempted_at"),
    error_message = safe_chr(df, "error_message"),
    collector_origin = origin,
    source_file = source_file,
    source_file_mtime = as.character(file_mtime)
  ) |>
    filter(!is.na(station_id), !is.na(block_start), !is.na(block_end))
}

load_backup_files <- function(paths, kind) {
  if (length(paths) == 0L) return(tibble())

  purrr::map_dfr(paths, function(path) {
    dat <- read_tabular(path)
    root_norm <- normalizePath(GITHUB_ROOT, winslash = "/", mustWork = FALSE)
    path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
    rel <- sub(root_norm, "", path_norm, fixed = TRUE)
    rel <- sub("^/", "", rel)
    mtime <- file.info(path)$mtime

    if (kind == "historical_data") {
      normalize_observations(dat, "github_actions", rel)
    } else {
      normalize_ledger(dat, "github_actions", rel, mtime)
    }
  })
}

# Use the latest ledger state for each station/block. attempted_at is ISO-like
# text and therefore sortable for the format written by the collector. File
# mtime/source_file provide deterministic fallbacks for partitioned backups.
latest_ledger_state <- function(ledger) {
  if (nrow(ledger) == 0L) return(ledger)

  ledger |>
    mutate(
      attempted_sort = ifelse(is.na(attempted_at), "", attempted_at),
      mtime_sort = ifelse(is.na(source_file_mtime), "", source_file_mtime)
    ) |>
    arrange(station_id, block_start, block_end, attempted_sort, mtime_sort, source_file) |>
    group_by(station_id, block_start, block_end) |>
    slice_tail(n = 1L) |>
    ungroup() |>
    select(-attempted_sort, -mtime_sort)
}

# Collect unique observation rows that fall inside a set of completed blocks.
# Split by station first so this remains fast even when the ledger grows large.
rows_in_blocks <- function(data, blocks) {
  if (nrow(data) == 0L || nrow(blocks) == 0L) return(data[0, , drop = FALSE])

  data_split <- split(data, data$station_id)
  block_split <- split(blocks, blocks$station_id)
  stations <- intersect(names(data_split), names(block_split))

  if (length(stations) == 0L) return(data[0, , drop = FALSE])

  out <- purrr::map_dfr(stations, function(station) {
    dat <- data_split[[station]]
    bs <- block_split[[station]]

    keep <- rep(FALSE, nrow(dat))
    for (i in seq_len(nrow(bs))) {
      keep <- keep | (dat$date >= bs$block_start[[i]] & dat$date <= bs$block_end[[i]])
    }
    dat[keep, , drop = FALSE]
  })

  out |>
    distinct(observation_key, ip_token, source_file, .keep_all = TRUE)
}

build_block_audit <- function(completed_ledger, gh_data, pc_data) {
  if (nrow(completed_ledger) == 0L) return(tibble())

  gh_split <- split(gh_data, gh_data$station_id)
  pc_split <- split(pc_data, pc_data$station_id)

  purrr::pmap_dfr(
    completed_ledger,
    function(station_id, block_start, block_end, request_status, n_rows,
             attempted_at, error_message, collector_origin, source_file,
             source_file_mtime, ...) {

      gh <- gh_split[[station_id]]
      pc <- pc_split[[station_id]]

      if (is.null(gh)) gh <- gh_data[0, , drop = FALSE]
      if (is.null(pc)) pc <- pc_data[0, , drop = FALSE]

      gh_block <- gh |>
        filter(date >= block_start, date <= block_end)
      pc_block <- pc |>
        filter(date >= block_start, date <= block_end)

      gh_unique_keys <- n_distinct(gh_block$observation_key)
      gh_unique_key_versions <- n_distinct(paste(gh_block$observation_key, gh_block$ip_token, sep = "|"))
      pc_unique_keys <- n_distinct(pc_block$observation_key)

      expected_rows <- suppressWarnings(as.integer(n_rows))
      count_check <- if (is.na(expected_rows)) {
        NA_character_
      } else if (request_status == "ok_empty") {
        if (expected_rows == 0L && gh_unique_keys == 0L) "MATCH" else "MISMATCH"
      } else {
        if (expected_rows == gh_unique_keys) "MATCH" else "MISMATCH"
      }

      tibble(
        station_id = station_id,
        block_start = as.Date(block_start),
        block_end = as.Date(block_end),
        github_request_status = request_status,
        github_ledger_n_rows = expected_rows,
        github_backup_unique_keys = gh_unique_keys,
        github_backup_unique_key_versions = gh_unique_key_versions,
        pc_unique_keys_same_block = pc_unique_keys,
        backup_vs_ledger_count = count_check,
        pc_rows_inside_github_empty_block = request_status == "ok_empty" && pc_unique_keys > 0L,
        github_ledger_attempted_at = attempted_at,
        github_ledger_source_file = source_file
      )
    }
  )
}

summarise_version_sets <- function(data, prefix) {
  if (nrow(data) == 0L) {
    out <- tibble(
      observation_key = character(),
      station_id = character(),
      date = as.Date(character()),
      version_tokens = character(),
      n_versions = integer(),
      representative_ip = double()
    )
    names(out)[names(out) == "version_tokens"] <- paste0(prefix, "_version_tokens")
    names(out)[names(out) == "n_versions"] <- paste0(prefix, "_n_versions")
    names(out)[names(out) == "representative_ip"] <- paste0(prefix, "_representative_ip")
    return(out)
  }

  out <- data |>
    distinct(observation_key, station_id, date, ip_token, pollution_index) |>
    arrange(observation_key, ip_token) |>
    group_by(observation_key, station_id, date) |>
    summarise(
      version_tokens = paste(sort(unique(ip_token)), collapse = " || "),
      n_versions = n_distinct(ip_token),
      representative_ip = dplyr::last(pollution_index),
      .groups = "drop"
    )

  names(out)[names(out) == "version_tokens"] <- paste0(prefix, "_version_tokens")
  names(out)[names(out) == "n_versions"] <- paste0(prefix, "_n_versions")
  names(out)[names(out) == "representative_ip"] <- paste0(prefix, "_representative_ip")
  out
}

version_set_intersects <- function(a, b) {
  if (is.na(a) || is.na(b)) return(FALSE)
  aa <- strsplit(a, " \\|\\| ", perl = TRUE)[[1]]
  bb <- strsplit(b, " \\|\\| ", perl = TRUE)[[1]]
  length(intersect(aa, bb)) > 0L
}

classify_key_comparison <- function(pc_tokens, gh_tokens) {
  pc_present <- !is.na(pc_tokens)
  gh_present <- !is.na(gh_tokens)

  if (pc_present && !gh_present) return("pc_only_in_completed_github_coverage")
  if (!pc_present && gh_present) return("github_only_recovery_gain")
  if (!pc_present && !gh_present) return("neither")

  pc_set <- sort(strsplit(pc_tokens, " \\|\\| ", perl = TRUE)[[1]])
  gh_set <- sort(strsplit(gh_tokens, " \\|\\| ", perl = TRUE)[[1]])

  if (identical(pc_set, gh_set)) return("exact_version_set_match")
  if (length(intersect(pc_set, gh_set)) > 0L) return("shared_key_overlapping_version_history")
  "same_key_different_pollution_index"
}

metadata_signature <- function(df) {
  cols <- c(
    "station_name", "river", "watershed", "location_category", "river_segment",
    "station_category", "kabupaten_kota", "province", "latitude", "longitude"
  )

  vals <- lapply(cols, function(nm) {
    if (!nm %in% names(df)) return(rep("", nrow(df)))
    x <- df[[nm]]
    if (is.numeric(x)) {
      ifelse(is.na(x), "", sprintf("%.12g", x))
    } else {
      ifelse(is.na(x), "", trimws(as.character(x)))
    }
  })
  names(vals) <- cols

  do.call(paste, c(vals, sep = "\u001F"))
}

# =============================================================================
# 3. INPUT VALIDATION AND GITHUB FILE DISCOVERY
# =============================================================================

msg("ONLIMO historical PC <-> GitHub reconciliation")
msg("PC data:      ", PC_DATA_FILE)
msg("PC ledger:    ", PC_LEDGER_FILE)
msg("GitHub root:  ", GITHUB_ROOT)
msg("Output:       ", OUTPUT_DIR)
msg("")

if (!file.exists(PC_DATA_FILE)) {
  stop("PC historical archive not found: ", PC_DATA_FILE, call. = FALSE)
}
if (!dir.exists(GITHUB_ROOT)) {
  stop("GitHub backup root not found: ", GITHUB_ROOT, call. = FALSE)
}

all_files <- list.files(
  GITHUB_ROOT,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)
all_files <- all_files[vapply(all_files, is_supported_file, logical(1))]

if (length(all_files) == 0L) {
  stop("No CSV/CSV.GZ/RDS files found under GitHub backup root.", call. = FALSE)
}

msg("Discovering GitHub backup files ...")

root_norm <- normalizePath(GITHUB_ROOT, winslash = "/", mustWork = FALSE)
all_files_norm <- normalizePath(all_files, winslash = "/", mustWork = FALSE)
relative_paths <- sub(root_norm, "", all_files_norm, fixed = TRUE)
relative_paths <- sub("^/", "", relative_paths)

inventory <- tibble(
  full_path = all_files,
  relative_path = relative_paths,
  bytes = as.numeric(file.info(all_files)$size),
  modified_at = as.character(file.info(all_files)$mtime),
  file_class = vapply(all_files, classify_backup_file, character(1))
)

readr::write_csv(inventory, FILE_INVENTORY_OUT, na = "")

gh_data_files <- inventory |>
  filter(file_class == "historical_data") |>
  pull(full_path)

gh_ledger_files <- inventory |>
  filter(file_class == "request_ledger") |>
  pull(full_path)

msg("  Historical-data file(s): ", length(gh_data_files))
msg("  Request-ledger file(s):  ", length(gh_ledger_files))
msg("  Other tabular file(s):   ", sum(inventory$file_class == "other"))

if (length(gh_data_files) == 0L) {
  stop(
    "Could not discover any GitHub file containing station_id + date + pollution_index. ",
    "Inspect: ", FILE_INVENTORY_OUT,
    call. = FALSE
  )
}
if (length(gh_ledger_files) == 0L) {
  stop(
    "Could not discover any GitHub request ledger containing station_id + block_start + ",
    "block_end + request_status. Inspect: ", FILE_INVENTORY_OUT,
    call. = FALSE
  )
}

# =============================================================================
# 4. LOAD PC AND GITHUB DATA
# =============================================================================

msg("")
msg("Loading PC archive ...")
pc_raw <- read_tabular(PC_DATA_FILE)
pc_data <- normalize_observations(pc_raw, "local_pc", basename(PC_DATA_FILE))

pc_ledger <- if (file.exists(PC_LEDGER_FILE)) {
  normalize_ledger(
    read_tabular(PC_LEDGER_FILE),
    "local_pc",
    basename(PC_LEDGER_FILE),
    file.info(PC_LEDGER_FILE)$mtime
  ) |>
    latest_ledger_state()
} else {
  tibble()
}

msg("Loading/reconstructing GitHub partitions ...")
gh_data <- load_backup_files(gh_data_files, "historical_data")
gh_ledger_all <- load_backup_files(gh_ledger_files, "request_ledger")
gh_ledger <- latest_ledger_state(gh_ledger_all)

# Preserve all distinct GitHub scientific versions. Repeated copies of exactly
# the same station/date/IP across backup partitions are only storage duplicates.
gh_data_versions <- gh_data |>
  distinct(observation_key, ip_token, .keep_all = TRUE)

pc_data_versions <- pc_data |>
  distinct(observation_key, ip_token, .keep_all = TRUE)

msg("  PC archive rows:                     ", nrow(pc_data))
msg("  PC unique station-date keys:         ", n_distinct(pc_data$observation_key))
msg("  GitHub stored rows across partitions:", nrow(gh_data))
msg("  GitHub unique station-date keys:      ", n_distinct(gh_data$observation_key))
msg("  GitHub unique key/IP versions:        ", nrow(gh_data_versions))
msg("  GitHub latest ledger blocks:          ", nrow(gh_ledger))

# =============================================================================
# 5. AUDIT GITHUB COMPLETED BLOCKS AGAINST ITS OWN BACKUP
# =============================================================================

completed_ledger <- gh_ledger |>
  filter(request_status %in% c("ok_data", "ok_empty"))

if (nrow(completed_ledger) == 0L) {
  stop("GitHub ledger contains no completed ok_data/ok_empty blocks.", call. = FALSE)
}

block_audit <- build_block_audit(completed_ledger, gh_data_versions, pc_data_versions)
readr::write_csv(block_audit, BLOCK_AUDIT_OUT, na = "")

ledger_count_mismatches <- block_audit |>
  filter(!is.na(backup_vs_ledger_count), backup_vs_ledger_count == "MISMATCH")

false_empty_blocks <- block_audit |>
  filter(pc_rows_inside_github_empty_block)

# =============================================================================
# 6. COMPARE SCIENTIFIC OBSERVATIONS INSIDE COMPLETED GITHUB COVERAGE
# =============================================================================

pc_covered <- rows_in_blocks(pc_data_versions, completed_ledger)
gh_covered <- rows_in_blocks(gh_data_versions, completed_ledger)

pc_sets <- summarise_version_sets(pc_covered, "pc")
gh_sets <- summarise_version_sets(gh_covered, "github")

key_compare <- full_join(
  pc_sets,
  gh_sets,
  by = c("observation_key", "station_id", "date")
) |>
  rowwise() |>
  mutate(
    reconciliation_class = classify_key_comparison(pc_version_tokens, github_version_tokens)
  ) |>
  ungroup()

# Add representative metadata comparison for shared keys. Metadata is secondary
# to the scientific IP payload because station catalogue attributes may evolve.
pc_meta_source <- pc_covered
pc_meta_source$pc_metadata_signature <- metadata_signature(pc_meta_source)
pc_meta <- pc_meta_source |>
  arrange(observation_key, retrieved_at, source_file) |>
  group_by(observation_key) |>
  slice_tail(n = 1L) |>
  ungroup() |>
  select(observation_key, pc_metadata_signature)

gh_meta_source <- gh_covered
gh_meta_source$github_metadata_signature <- metadata_signature(gh_meta_source)
gh_meta <- gh_meta_source |>
  arrange(observation_key, retrieved_at, source_file) |>
  group_by(observation_key) |>
  slice_tail(n = 1L) |>
  ungroup() |>
  select(observation_key, github_metadata_signature)

key_compare <- key_compare |>
  left_join(pc_meta, by = "observation_key") |>
  left_join(gh_meta, by = "observation_key") |>
  mutate(
    metadata_agreement = case_when(
      is.na(pc_metadata_signature) | is.na(github_metadata_signature) ~ NA,
      pc_metadata_signature == github_metadata_signature ~ TRUE,
      TRUE ~ FALSE
    )
  )

exceptions <- key_compare |>
  filter(reconciliation_class != "exact_version_set_match") |>
  arrange(reconciliation_class, station_id, date)

readr::write_csv(exceptions, EXCEPTIONS_OUT, na = "")

# =============================================================================
# 7. STATION-LEVEL COVERAGE
# =============================================================================

station_block <- completed_ledger |>
  group_by(station_id) |>
  summarise(
    github_completed_blocks = n(),
    github_completed_from = min(block_start),
    github_completed_through = max(block_end),
    github_ok_data_blocks = sum(request_status == "ok_data"),
    github_ok_empty_blocks = sum(request_status == "ok_empty"),
    .groups = "drop"
  )

station_keys <- key_compare |>
  group_by(station_id) |>
  summarise(
    pc_keys_in_completed_coverage = sum(!is.na(pc_version_tokens)),
    github_keys_in_completed_coverage = sum(!is.na(github_version_tokens)),
    exact_version_set_matches = sum(reconciliation_class == "exact_version_set_match"),
    overlapping_version_history = sum(reconciliation_class == "shared_key_overlapping_version_history"),
    same_key_different_ip = sum(reconciliation_class == "same_key_different_pollution_index"),
    pc_only_keys = sum(reconciliation_class == "pc_only_in_completed_github_coverage"),
    github_only_recovery_gain = sum(reconciliation_class == "github_only_recovery_gain"),
    .groups = "drop"
  )

station_block_qa <- block_audit |>
  group_by(station_id) |>
  summarise(
    ledger_backup_count_mismatches = sum(backup_vs_ledger_count == "MISMATCH", na.rm = TRUE),
    github_empty_blocks_with_pc_rows = sum(pc_rows_inside_github_empty_block, na.rm = TRUE),
    .groups = "drop"
  )

station_coverage <- station_block |>
  full_join(station_keys, by = "station_id") |>
  full_join(station_block_qa, by = "station_id") |>
  arrange(station_id)

readr::write_csv(station_coverage, STATION_COVERAGE_OUT, na = "")

# =============================================================================
# 8. OVERALL STATUS
# =============================================================================

class_counts <- key_compare |>
  count(reconciliation_class, name = "n") |>
  arrange(desc(n))

n_pc_only <- sum(key_compare$reconciliation_class == "pc_only_in_completed_github_coverage")
n_gh_only <- sum(key_compare$reconciliation_class == "github_only_recovery_gain")
n_ip_mismatch <- sum(key_compare$reconciliation_class == "same_key_different_pollution_index")
n_overlap_versions <- sum(key_compare$reconciliation_class == "shared_key_overlapping_version_history")
n_exact_sets <- sum(key_compare$reconciliation_class == "exact_version_set_match")
n_metadata_diff <- sum(key_compare$metadata_agreement == FALSE, na.rm = TRUE)

shared_keys <- key_compare |>
  filter(!is.na(pc_version_tokens), !is.na(github_version_tokens))

shared_pc_payload_represented <- if (nrow(shared_keys) == 0L) {
  NA_real_
} else {
  100 * mean(vapply(
    seq_len(nrow(shared_keys)),
    function(i) version_set_intersects(
      shared_keys$pc_version_tokens[[i]],
      shared_keys$github_version_tokens[[i]]
    ),
    logical(1)
  ))
}

# Catch-up is considered current only if every station represented in the
# completed GitHub ledger has completed coverage through yesterday.
yesterday <- Sys.Date() - 1L
catchup_by_station <- station_block |>
  mutate(current_through_yesterday = github_completed_through >= yesterday)

catchup_complete <- nrow(catchup_by_station) > 0L && all(catchup_by_station$current_through_yesterday)

if (nrow(ledger_count_mismatches) > 0L) {
  overall_status <- "REVIEW_BACKUP_PACKAGING"
  reason <- paste0(
    nrow(ledger_count_mismatches),
    " completed GitHub ledger block(s) do not reconcile to the retained GitHub data partitions."
  )
} else if (nrow(false_empty_blocks) > 0L) {
  overall_status <- "REVIEW_FALSE_EMPTY"
  reason <- paste0(
    nrow(false_empty_blocks),
    " GitHub ok_empty block(s) contain PC observations in the same station/date interval."
  )
} else if (n_ip_mismatch > 0L) {
  overall_status <- "REVIEW_PAYLOAD"
  reason <- paste0(
    n_ip_mismatch,
    " shared station-date key(s) have non-overlapping Pollution Index values."
  )
} else if (n_pc_only > 0L) {
  overall_status <- "REVIEW_PC_ONLY"
  reason <- paste0(
    n_pc_only,
    " PC observation(s) are absent from GitHub inside blocks GitHub marked completed."
  )
} else if (catchup_complete) {
  overall_status <- "PASS"
  reason <- paste0(
    "All PC observations inside completed GitHub coverage are represented by GitHub with matching ",
    "Pollution Index payloads, GitHub backup partitions reconcile to the request ledger, and all ",
    "GitHub stations are complete through yesterday."
  )
} else {
  overall_status <- "PASS_PARTIAL_CATCHUP"
  reason <- paste0(
    "All observations inside completed GitHub coverage reconcile, but the GitHub historical catch-up ",
    "has not yet completed through yesterday for every station."
  )
}

status_counts <- gh_ledger |>
  count(request_status, name = "n") |>
  arrange(desc(n))

summary_lines <- c(
  "================ ONLIMO historical reconciliation summary ================",
  paste0("Overall status: ", overall_status),
  paste0("Reason: ", reason),
  paste0("PC archive rows: ", nrow(pc_data)),
  paste0("PC unique station-date keys: ", n_distinct(pc_data$observation_key)),
  paste0("GitHub stored rows across discovered partitions: ", nrow(gh_data)),
  paste0("GitHub unique station-date keys: ", n_distinct(gh_data$observation_key)),
  paste0("GitHub unique station-date/IP versions: ", nrow(gh_data_versions)),
  paste0("GitHub completed ledger blocks: ", nrow(completed_ledger)),
  paste0("GitHub completed stations: ", n_distinct(completed_ledger$station_id)),
  paste0("GitHub catch-up complete through ", yesterday, ": ", toupper(as.character(catchup_complete))),
  paste0("PC keys inside completed GitHub coverage: ", n_distinct(pc_covered$observation_key)),
  paste0("GitHub keys inside completed GitHub coverage: ", n_distinct(gh_covered$observation_key)),
  paste0("Exact version-set matches: ", n_exact_sets),
  paste0("Shared keys with overlapping version history: ", n_overlap_versions),
  paste0("Same key / different Pollution Index: ", n_ip_mismatch),
  paste0("PC-only keys inside completed GitHub coverage: ", n_pc_only),
  paste0("GitHub-only recovery-gain keys: ", n_gh_only),
  paste0("Shared-key PC payload represented in GitHub: ",
         ifelse(is.na(shared_pc_payload_represented), "NA", sprintf("%.6f%%", shared_pc_payload_represented))),
  paste0("Secondary metadata differences on shared keys: ", n_metadata_diff),
  paste0("GitHub ledger/backup block-count mismatches: ", nrow(ledger_count_mismatches)),
  paste0("GitHub ok_empty blocks containing PC observations: ", nrow(false_empty_blocks)),
  "",
  "GitHub latest-ledger status counts:",
  if (nrow(status_counts) == 0L) "none" else paste0("  ", status_counts$request_status, ": ", status_counts$n),
  "",
  "Observation reconciliation classes:",
  if (nrow(class_counts) == 0L) "none" else paste0("  ", class_counts$reconciliation_class, ": ", class_counts$n),
  "",
  "Outputs:",
  paste0("  ", FILE_INVENTORY_OUT),
  paste0("  ", BLOCK_AUDIT_OUT),
  paste0("  ", EXCEPTIONS_OUT),
  paste0("  ", STATION_COVERAGE_OUT),
  paste0("  ", SUMMARY_OUT)
)

writeLines(summary_lines, SUMMARY_OUT, useBytes = TRUE)
cat(paste(summary_lines, collapse = "\n"), "\n")