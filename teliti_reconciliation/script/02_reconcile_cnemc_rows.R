# CNEMC row-level reconciliation - outage-aware
#
# Purpose:
#   Compare retained GitHub CNEMC processed checkpoints against the cumulative
#   Windows-PC CNEMC observation archive while separating:
#     A. strict common-operation validation,
#     B. asymmetric/outage coverage, and
#     C. all-archive union/provenance.
#
# Why this matters:
#   A PC outage should not lower the primary scraper-agreement percentage merely
#   because GitHub continued collecting observations that the PC could not see.
#   Those observations are instead reported as resilience/recovery gains.
#
# Existing outputs are preserved for downstream compatibility. New outputs:
#   - cnemc_row_reconciliation_scope_summary.csv
#   - cnemc_pc_outage_intervals.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------
PRIMARY_ROOT <- Sys.getenv(
  "TELITI_PRIMARY_ROOT",
  unset = "D:/# R Project/penelitian"
)

CLOUD_BACKUP_ROOT <- Sys.getenv(
  "TELITI_CLOUD_BACKUP_ROOT",
  unset = "D:/# R Project/teliti-data-backup"
)

RECON_ROOT <- Sys.getenv(
  "TELITI_RECON_ROOT",
  unset = file.path(PRIMARY_ROOT, "teliti_reconciliation")
)

OUTPUT_DIR <- file.path(RECON_ROOT, "output", "cnemc")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# A gap larger than this is treated as an operational outage rather than an
# ordinary scheduler delay. It can be overridden without editing the script.
OUTAGE_GAP_MINUTES <- suppressWarnings(as.numeric(Sys.getenv(
  "TELITI_CNEMC_OUTAGE_GAP_MINUTES",
  unset = "90"
)))
if (!is.finite(OUTAGE_GAP_MINUTES) || OUTAGE_GAP_MINUTES <= 0) {
  stop("TELITI_CNEMC_OUTAGE_GAP_MINUTES must be a positive number.")
}

# Small schedule offsets near the beginning/end of a gap are still considered
# joint operation. This prevents a normal 5-10 minute cadence offset from being
# labelled an outage.
COVERAGE_TOLERANCE_MINUTES <- suppressWarnings(as.numeric(Sys.getenv(
  "TELITI_CNEMC_COVERAGE_TOLERANCE_MINUTES",
  unset = "15"
)))
if (!is.finite(COVERAGE_TOLERANCE_MINUTES) || COVERAGE_TOLERANCE_MINUTES < 0) {
  stop("TELITI_CNEMC_COVERAGE_TOLERANCE_MINUTES must be non-negative.")
}
coverage_tolerance_seconds <- COVERAGE_TOLERANCE_MINUTES * 60

PC_OBSERVATIONS_PATH <- file.path(
  PRIMARY_ROOT,
  "nmemc", "data", "surfacewater", "processed",
  "nmemc_surfacewater_observations.rds"
)

PC_RUN_MANIFEST_PATH <- file.path(
  PRIMARY_ROOT,
  "nmemc", "data", "surfacewater", "processed",
  "nmemc_surfacewater_run_manifest.csv"
)

GITHUB_COLLECTION_MANIFEST_PATH <- file.path(
  CLOUD_BACKUP_ROOT,
  "cnemc_surfacewater", "manifests", "collection_manifest.csv"
)

GITHUB_SNAPSHOT_MANIFEST_PATH <- file.path(
  CLOUD_BACKUP_ROOT,
  "cnemc_surfacewater", "manifests", "snapshot_manifest.csv"
)

COLLECTION_PAIR_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_pc_to_github_pairs.csv"
)

SNAPSHOT_SUMMARY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_row_reconciliation_snapshot_summary.csv"
)
GLOBAL_SUMMARY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_row_reconciliation_global_summary.csv"
)
EXCEPTION_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_row_reconciliation_exceptions.csv.gz"
)
REPORT_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_row_reconciliation.md"
)
SCOPE_SUMMARY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_row_reconciliation_scope_summary.csv"
)
OUTAGE_INTERVAL_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_pc_outage_intervals.csv"
)

# -----------------------------------------------------------------------------
# 2. Helpers
# -----------------------------------------------------------------------------
assert_file <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path)
}

assert_columns <- function(dat, required, label) {
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0L) {
    stop(
      label,
      " is missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }
}

normalize_hash <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x == ""] <- NA_character_
  x
}

valid_hash64 <- function(x) {
  !is.na(x) & grepl("^[0-9a-f]{16}$", x)
}

valid_md5 <- function(x) {
  !is.na(x) & grepl("^[0-9a-f]{32}$", x)
}

safe_pct <- function(numerator, denominator) {
  if (length(denominator) == 0L || is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }
  100 * numerator / denominator
}

format_pct <- function(x, digits = 2L) {
  if (length(x) == 0L || is.na(x)) return("NA")
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE), "%")
}

format_int <- function(x) {
  format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

normalize_relative_path <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  x <- sub("^/+", "", x)
  x
}

parse_time_utc <- function(x, label) {
  if (inherits(x, "POSIXt")) {
    out <- as.POSIXct(x, tz = "UTC")
  } else {
    out <- suppressWarnings(
      readr::parse_datetime(
        as.character(x),
        locale = readr::locale(tz = "UTC")
      )
    )
  }
  if (any(is.na(out))) {
    stop("Could not parse ", sum(is.na(out)), " timestamp(s) in ", label, ".")
  }
  out
}

format_utc <- function(x) {
  if (length(x) == 0L || all(is.na(x))) return(NA_character_)
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

read_github_snapshot <- function(relative_path, expected_md5) {
  full_path <- file.path(
    CLOUD_BACKUP_ROOT,
    normalize_relative_path(relative_path)
  )

  if (!file.exists(full_path)) {
    stop("Retained GitHub processed snapshot not found: ", full_path)
  }

  dat <- readr::read_csv(
    full_path,
    show_col_types = FALSE,
    progress = FALSE
  )

  assert_columns(
    dat,
    c("snapshot_md5", "observation_key_hash", "row_hash"),
    paste0("GitHub processed snapshot ", basename(full_path))
  )

  dat <- dat %>%
    mutate(
      snapshot_md5 = normalize_hash(snapshot_md5),
      observation_key_hash = normalize_hash(observation_key_hash),
      row_hash = normalize_hash(row_hash)
    )

  snapshot_hashes <- unique(na.omit(dat$snapshot_md5))
  if (length(snapshot_hashes) != 1L || !identical(snapshot_hashes, expected_md5)) {
    stop(
      "Processed snapshot hash does not match manifest for ",
      basename(full_path),
      ". Expected ", expected_md5,
      "; found ", paste(snapshot_hashes, collapse = ", ")
    )
  }

  if (any(!valid_hash64(dat$row_hash))) {
    stop("Invalid row_hash found in GitHub snapshot: ", basename(full_path))
  }
  if (any(!valid_hash64(dat$observation_key_hash))) {
    stop(
      "Invalid observation_key_hash found in GitHub snapshot: ",
      basename(full_path)
    )
  }

  dat
}

# -----------------------------------------------------------------------------
# 3. Read PC archive and collection histories
# -----------------------------------------------------------------------------
assert_file(PC_OBSERVATIONS_PATH, "PC CNEMC cumulative observations")
assert_file(PC_RUN_MANIFEST_PATH, "PC CNEMC run manifest")
assert_file(GITHUB_COLLECTION_MANIFEST_PATH, "GitHub CNEMC collection manifest")
assert_file(GITHUB_SNAPSHOT_MANIFEST_PATH, "GitHub CNEMC snapshot manifest")

pc <- readRDS(PC_OBSERVATIONS_PATH)
assert_columns(
  pc,
  c("observation_key_hash", "row_hash"),
  "PC CNEMC cumulative observations"
)
pc <- pc %>%
  mutate(
    observation_key_hash = normalize_hash(observation_key_hash),
    row_hash = normalize_hash(row_hash)
  )

if (any(!valid_hash64(pc$row_hash))) {
  stop("PC cumulative observations contain invalid row_hash values.")
}
if (any(!valid_hash64(pc$observation_key_hash))) {
  stop("PC cumulative observations contain invalid observation_key_hash values.")
}

pc_row_hashes <- unique(pc$row_hash)
pc_key_hashes <- unique(pc$observation_key_hash)

pc_runs <- readr::read_csv(
  PC_RUN_MANIFEST_PATH,
  show_col_types = FALSE,
  progress = FALSE
)
assert_columns(pc_runs, c("collected_at"), "PC CNEMC run manifest")
pc_runs <- pc_runs %>%
  mutate(collected_at_utc = parse_time_utc(collected_at, "PC collected_at")) %>%
  arrange(collected_at_utc)

# Keep one timestamp if the manifest accidentally contains exact duplicates.
pc_times <- sort(unique(pc_runs$collected_at_utc))
if (length(pc_times) == 0L) stop("PC CNEMC run manifest contains no runs.")

pc_start <- min(pc_times)
pc_end <- max(pc_times)

gh_collections <- readr::read_csv(
  GITHUB_COLLECTION_MANIFEST_PATH,
  show_col_types = FALSE,
  progress = FALSE
)
assert_columns(
  gh_collections,
  c("collected_at", "snapshot_md5"),
  "GitHub CNEMC collection manifest"
)
gh_collections <- gh_collections %>%
  mutate(
    collected_at_utc = parse_time_utc(collected_at, "GitHub collected_at"),
    snapshot_md5 = normalize_hash(snapshot_md5)
  ) %>%
  arrange(collected_at_utc)

gh_start <- min(gh_collections$collected_at_utc)
gh_end <- max(gh_collections$collected_at_utc)

manifest <- readr::read_csv(
  GITHUB_SNAPSHOT_MANIFEST_PATH,
  show_col_types = FALSE,
  progress = FALSE
)
assert_columns(
  manifest,
  c("collected_at", "snapshot_md5", "rows", "processed_file"),
  "GitHub CNEMC snapshot manifest"
)

manifest <- manifest %>%
  mutate(
    collected_at_utc = parse_time_utc(collected_at, "GitHub snapshot collected_at"),
    snapshot_md5 = normalize_hash(snapshot_md5),
    processed_file = trimws(as.character(processed_file)),
    retained_processed = !is.na(processed_file) & processed_file != ""
  )

if (any(!valid_md5(manifest$snapshot_md5))) {
  stop("GitHub snapshot manifest contains invalid snapshot_md5 values.")
}

retained <- manifest %>%
  filter(retained_processed) %>%
  distinct(snapshot_md5, .keep_all = TRUE) %>%
  arrange(collected_at_utc)

if (nrow(retained) == 0L) {
  stop("No retained GitHub processed CNEMC checkpoints are available.")
}

# Optional context from script 01.
pairs <- NULL
if (file.exists(COLLECTION_PAIR_PATH)) {
  pairs <- readr::read_csv(
    COLLECTION_PAIR_PATH,
    show_col_types = FALSE,
    progress = FALSE
  )
  if (all(c("github_snapshot_md5", "exact_snapshot_match") %in% names(pairs))) {
    pairs <- pairs %>%
      mutate(github_snapshot_md5 = normalize_hash(github_snapshot_md5))
  } else {
    pairs <- NULL
  }
}

# -----------------------------------------------------------------------------
# 4. Detect PC outage intervals and classify GitHub checkpoints
# -----------------------------------------------------------------------------
if (length(pc_times) >= 2L) {
  gap_start <- pc_times[-length(pc_times)]
  gap_end <- pc_times[-1L]
  gap_minutes <- as.numeric(difftime(gap_end, gap_start, units = "mins"))

  internal_outages <- tibble(
    interval_type = "pc_internal_outage",
    interval_start_utc = gap_start,
    interval_end_utc = gap_end,
    duration_minutes = gap_minutes
  ) %>%
    filter(duration_minutes > OUTAGE_GAP_MINUTES)
} else {
  internal_outages <- tibble(
    interval_type = character(),
    interval_start_utc = as.POSIXct(character(), tz = "UTC"),
    interval_end_utc = as.POSIXct(character(), tz = "UTC"),
    duration_minutes = numeric()
  )
}

asymmetric_intervals <- internal_outages

if (gh_start < pc_start) {
  asymmetric_intervals <- bind_rows(
    asymmetric_intervals,
    tibble(
      interval_type = "github_only_head",
      interval_start_utc = gh_start,
      interval_end_utc = pc_start,
      duration_minutes = as.numeric(difftime(pc_start, gh_start, units = "mins"))
    )
  )
}

if (gh_end > pc_end) {
  asymmetric_intervals <- bind_rows(
    asymmetric_intervals,
    tibble(
      interval_type = "github_only_tail",
      interval_start_utc = pc_end,
      interval_end_utc = gh_end,
      duration_minutes = as.numeric(difftime(gh_end, pc_end, units = "mins"))
    )
  )
}

is_inside_internal_outage <- function(t) {
  if (nrow(internal_outages) == 0L) return(FALSE)
  any(
    t > internal_outages$interval_start_utc + coverage_tolerance_seconds &
      t < internal_outages$interval_end_utc - coverage_tolerance_seconds
  )
}

classify_checkpoint_scope <- function(t) {
  if (t < pc_start - coverage_tolerance_seconds) return("github_only_head")
  if (t > pc_end + coverage_tolerance_seconds) return("github_only_tail")
  if (is_inside_internal_outage(t)) return("pc_outage")
  "strict_common_operation"
}

retained <- retained %>%
  rowwise() %>%
  mutate(coverage_scope = classify_checkpoint_scope(collected_at_utc)) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 5. Compare every retained GitHub checkpoint with the PC cumulative archive
# -----------------------------------------------------------------------------
snapshot_summaries <- vector("list", nrow(retained))
exception_rows <- vector("list", nrow(retained))
github_index_rows <- vector("list", nrow(retained))

for (i in seq_len(nrow(retained))) {
  m <- retained[i, , drop = FALSE]
  snapshot_md5 <- m$snapshot_md5[[1]]
  coverage_scope <- m$coverage_scope[[1]]

  gh <- read_github_snapshot(
    relative_path = m$processed_file[[1]],
    expected_md5 = snapshot_md5
  ) %>%
    mutate(
      pc_exact_row_version = row_hash %in% pc_row_hashes,
      pc_same_observation_key = observation_key_hash %in% pc_key_hashes,
      reconciliation_class = case_when(
        pc_exact_row_version ~ "confirmed_exact_row_version",
        pc_same_observation_key ~ "same_key_different_version",
        TRUE ~ "github_only_observation_key"
      )
    )

  github_index_rows[[i]] <- gh %>%
    distinct(row_hash, observation_key_hash) %>%
    mutate(
      coverage_scope = coverage_scope,
      github_collected_at = as.character(m$collected_at[[1]]),
      github_collected_at_utc = m$collected_at_utc[[1]],
      github_snapshot_md5 = snapshot_md5
    )

  n_rows <- nrow(gh)
  n_unique_rows <- n_distinct(gh$row_hash)
  n_unique_keys <- n_distinct(gh$observation_key_hash)
  exact_n <- sum(gh$pc_exact_row_version)
  same_key_diff_n <- sum(!gh$pc_exact_row_version & gh$pc_same_observation_key)
  github_only_key_n <- sum(!gh$pc_same_observation_key)
  duplicate_row_n <- n_rows - n_unique_rows

  exact_pair_context <- NA
  if (!is.null(pairs)) {
    relevant_pairs <- pairs %>%
      filter(github_snapshot_md5 == snapshot_md5)
    if (nrow(relevant_pairs) > 0L) {
      exact_pair_context <- any(
        as.logical(relevant_pairs$exact_snapshot_match),
        na.rm = TRUE
      )
    }
  }

  snapshot_summaries[[i]] <- tibble(
    github_collected_at = as.character(m$collected_at[[1]]),
    github_collected_at_utc = m$collected_at_utc[[1]],
    coverage_scope = coverage_scope,
    snapshot_md5 = snapshot_md5,
    processed_file = m$processed_file[[1]],
    manifest_rows = suppressWarnings(as.integer(m$rows[[1]])),
    processed_rows = n_rows,
    unique_row_hashes = n_unique_rows,
    unique_observation_keys = n_unique_keys,
    duplicate_rows_within_snapshot = duplicate_row_n,
    confirmed_exact_row_versions = exact_n,
    confirmed_exact_pct = safe_pct(exact_n, n_rows),
    same_key_different_version = same_key_diff_n,
    same_key_different_version_pct = safe_pct(same_key_diff_n, n_rows),
    github_only_observation_keys = github_only_key_n,
    github_only_observation_key_pct = safe_pct(github_only_key_n, n_rows),
    exact_temporal_pair_seen = exact_pair_context
  )

  exception_rows[[i]] <- gh %>%
    filter(!pc_exact_row_version) %>%
    mutate(
      coverage_scope = coverage_scope,
      github_snapshot_md5 = snapshot_md5,
      github_collected_at = as.character(m$collected_at[[1]]),
      github_processed_file = m$processed_file[[1]]
    ) %>%
    select(
      coverage_scope,
      github_collected_at,
      github_snapshot_md5,
      github_processed_file,
      reconciliation_class,
      observation_key_hash,
      row_hash,
      any_of(c(
        "area",
        "river_basin",
        "monitoring_section",
        "monitoring_time_raw",
        "observation_datetime",
        "water_quality_class_code",
        "water_quality_class"
      ))
    )
}

snapshot_summary <- bind_rows(snapshot_summaries) %>%
  arrange(github_collected_at_utc)
exceptions <- bind_rows(exception_rows)
github_index <- bind_rows(github_index_rows)

# -----------------------------------------------------------------------------
# 6. Scope summaries
# -----------------------------------------------------------------------------
scope_order <- c(
  "strict_common_operation",
  "pc_outage",
  "github_only_tail",
  "github_only_head"
)

summarize_scope <- function(scope_name) {
  idx <- github_index %>% filter(coverage_scope == scope_name)
  snaps <- snapshot_summary %>% filter(coverage_scope == scope_name)

  gh_rows <- unique(idx$row_hash)
  gh_keys <- unique(idx$observation_key_hash)
  exact_rows <- intersect(gh_rows, pc_row_hashes)
  confirmed_keys <- intersect(gh_keys, pc_key_hashes)

  tibble(
    coverage_scope = scope_name,
    retained_checkpoints = nrow(snaps),
    github_unique_row_versions = length(gh_rows),
    github_row_versions_confirmed_in_pc = length(exact_rows),
    github_row_versions_absent_from_pc = length(setdiff(gh_rows, pc_row_hashes)),
    github_row_version_confirmation_pct = safe_pct(length(exact_rows), length(gh_rows)),
    github_unique_observation_keys = length(gh_keys),
    github_observation_keys_confirmed_in_pc = length(confirmed_keys),
    github_observation_keys_absent_from_pc = length(setdiff(gh_keys, pc_key_hashes)),
    github_observation_key_confirmation_pct = safe_pct(length(confirmed_keys), length(gh_keys))
  )
}

scope_summary <- bind_rows(lapply(scope_order, summarize_scope)) %>%
  filter(retained_checkpoints > 0L | github_unique_row_versions > 0L)

strict_idx <- github_index %>% filter(coverage_scope == "strict_common_operation")
asym_idx <- github_index %>% filter(coverage_scope != "strict_common_operation")

strict_row_hashes <- unique(strict_idx$row_hash)
strict_key_hashes <- unique(strict_idx$observation_key_hash)
asym_row_hashes <- unique(asym_idx$row_hash)
asym_key_hashes <- unique(asym_idx$observation_key_hash)

strict_confirmed_rows <- intersect(strict_row_hashes, pc_row_hashes)
strict_confirmed_keys <- intersect(strict_key_hashes, pc_key_hashes)
asym_missing_rows <- setdiff(asym_row_hashes, pc_row_hashes)
asym_missing_keys <- setdiff(asym_key_hashes, pc_key_hashes)

all_github_row_hashes <- unique(github_index$row_hash)
all_github_key_hashes <- unique(github_index$observation_key_hash)
confirmed_row_union <- intersect(all_github_row_hashes, pc_row_hashes)
missing_row_union <- setdiff(all_github_row_hashes, pc_row_hashes)
confirmed_key_union <- intersect(all_github_key_hashes, pc_key_hashes)
missing_key_union <- setdiff(all_github_key_hashes, pc_key_hashes)

canonical_row_union <- union(pc_row_hashes, all_github_row_hashes)
canonical_key_union <- union(pc_key_hashes, all_github_key_hashes)

# Add checkpoint/data counts to detected outage/asymmetric intervals.
if (nrow(asymmetric_intervals) > 0L) {
  asymmetric_intervals <- asymmetric_intervals %>%
    rowwise() %>%
    mutate(
      github_retained_checkpoints = sum(
        snapshot_summary$github_collected_at_utc > interval_start_utc &
          snapshot_summary$github_collected_at_utc < interval_end_utc,
        na.rm = TRUE
      ),
      github_unique_row_versions = {
        in_interval <-
          github_index$github_collected_at_utc > interval_start_utc &
          github_index$github_collected_at_utc < interval_end_utc
        n_distinct(github_index$row_hash[in_interval])
      },
      github_unique_observation_keys = {
        in_interval <-
          github_index$github_collected_at_utc > interval_start_utc &
          github_index$github_collected_at_utc < interval_end_utc
        n_distinct(github_index$observation_key_hash[in_interval])
      },
      github_only_observation_keys_absent_from_pc = {
        in_interval <-
          github_index$github_collected_at_utc > interval_start_utc &
          github_index$github_collected_at_utc < interval_end_utc
        keys <- unique(github_index$observation_key_hash[in_interval])
        length(setdiff(keys, pc_key_hashes))
      }
    ) %>%
    ungroup() %>%
    arrange(interval_start_utc)
}

# -----------------------------------------------------------------------------
# 7. Backward-compatible all-archive global metrics + new primary metrics
# -----------------------------------------------------------------------------
global_summary <- tibble(
  metric = c(
    "pc_unique_row_versions",
    "pc_unique_observation_keys",
    "github_retained_checkpoints",
    "github_retained_unique_row_versions",
    "github_retained_unique_observation_keys",
    "github_row_versions_confirmed_in_pc",
    "github_row_versions_absent_from_pc",
    "github_row_version_confirmation_pct",
    "github_observation_keys_confirmed_in_pc",
    "github_observation_keys_absent_from_pc",
    "github_observation_key_confirmation_pct",
    "exception_rows_across_checkpoints",
    "strict_common_operation_checkpoints",
    "strict_common_github_unique_row_versions",
    "strict_common_exact_row_versions_in_pc",
    "strict_common_exact_row_version_pct",
    "strict_common_github_unique_observation_keys",
    "strict_common_observation_keys_in_pc",
    "strict_common_observation_key_pct",
    "asymmetric_github_unique_row_versions",
    "asymmetric_row_versions_absent_from_pc",
    "asymmetric_github_unique_observation_keys",
    "asymmetric_observation_keys_absent_from_pc",
    "detected_pc_internal_outages",
    "pc_outage_gap_threshold_minutes",
    "coverage_tolerance_minutes",
    "canonical_union_unique_row_versions",
    "canonical_union_unique_observation_keys"
  ),
  value = c(
    length(pc_row_hashes),
    length(pc_key_hashes),
    nrow(snapshot_summary),
    length(all_github_row_hashes),
    length(all_github_key_hashes),
    length(confirmed_row_union),
    length(missing_row_union),
    safe_pct(length(confirmed_row_union), length(all_github_row_hashes)),
    length(confirmed_key_union),
    length(missing_key_union),
    safe_pct(length(confirmed_key_union), length(all_github_key_hashes)),
    nrow(exceptions),
    sum(snapshot_summary$coverage_scope == "strict_common_operation"),
    length(strict_row_hashes),
    length(strict_confirmed_rows),
    safe_pct(length(strict_confirmed_rows), length(strict_row_hashes)),
    length(strict_key_hashes),
    length(strict_confirmed_keys),
    safe_pct(length(strict_confirmed_keys), length(strict_key_hashes)),
    length(asym_row_hashes),
    length(asym_missing_rows),
    length(asym_key_hashes),
    length(asym_missing_keys),
    nrow(internal_outages),
    OUTAGE_GAP_MINUTES,
    COVERAGE_TOLERANCE_MINUTES,
    length(canonical_row_union),
    length(canonical_key_union)
  )
)

# -----------------------------------------------------------------------------
# 8. Write outputs
# -----------------------------------------------------------------------------
readr::write_csv(snapshot_summary, SNAPSHOT_SUMMARY_PATH, na = "")
readr::write_csv(global_summary, GLOBAL_SUMMARY_PATH, na = "")
readr::write_csv(scope_summary, SCOPE_SUMMARY_PATH, na = "")
readr::write_csv(asymmetric_intervals, OUTAGE_INTERVAL_PATH, na = "")

if (nrow(exceptions) > 0L) {
  readr::write_csv(exceptions, EXCEPTION_PATH, na = "")
} else {
  empty_exceptions <- tibble(
    coverage_scope = character(),
    github_collected_at = character(),
    github_snapshot_md5 = character(),
    github_processed_file = character(),
    reconciliation_class = character(),
    observation_key_hash = character(),
    row_hash = character()
  )
  readr::write_csv(empty_exceptions, EXCEPTION_PATH, na = "")
}

strict_row_pct <- safe_pct(length(strict_confirmed_rows), length(strict_row_hashes))
strict_key_pct <- safe_pct(length(strict_confirmed_keys), length(strict_key_hashes))
all_row_pct <- safe_pct(length(confirmed_row_union), length(all_github_row_hashes))
all_key_pct <- safe_pct(length(confirmed_key_union), length(all_github_key_hashes))

outage_lines <- if (nrow(asymmetric_intervals) > 0L) {
  vapply(
    seq_len(nrow(asymmetric_intervals)),
    function(i) {
      x <- asymmetric_intervals[i, ]
      paste0(
        "- ", x$interval_type,
        ": ", format_utc(x$interval_start_utc),
        " -> ", format_utc(x$interval_end_utc),
        " (", round(x$duration_minutes, 1), " min)",
        "; retained GitHub checkpoints=", x$github_retained_checkpoints,
        "; GitHub-only keys absent from PC=",
        x$github_only_observation_keys_absent_from_pc
      )
    },
    character(1)
  )
} else {
  "- No asymmetric/outage interval was detected."
}

report_lines <- c(
  "# CNEMC row-level collector reconciliation",
  "",
  paste0("Generated from ", nrow(snapshot_summary), " retained GitHub full checkpoint(s)."),
  paste0("PC outage-gap threshold: ", OUTAGE_GAP_MINUTES, " minutes."),
  paste0("Coverage-boundary tolerance: ±", COVERAGE_TOLERANCE_MINUTES, " minutes."),
  "",
  "## A. Strict common-operation validation",
  "",
  "This is the primary collector-comparability view. GitHub checkpoints collected during detected PC outages or outside the PC manifest envelope are excluded.",
  "",
  paste0("- Retained GitHub checkpoints: ", sum(snapshot_summary$coverage_scope == "strict_common_operation")),
  paste0("- GitHub unique row versions: ", format_int(length(strict_row_hashes))),
  paste0("- Exact GitHub row versions present in PC archive: ", format_int(length(strict_confirmed_rows)), " (", format_pct(strict_row_pct), ")"),
  paste0("- GitHub unique observation keys: ", format_int(length(strict_key_hashes))),
  paste0("- GitHub observation keys present in PC archive: ", format_int(length(strict_confirmed_keys)), " (", format_pct(strict_key_pct), ")"),
  paste0("- GitHub observation keys absent from PC archive: ", format_int(length(setdiff(strict_key_hashes, pc_key_hashes)))),
  "",
  "## B. Asymmetric coverage / outage recovery",
  "",
  outage_lines,
  "",
  paste0("- GitHub unique row versions sampled outside strict common operation: ", format_int(length(asym_row_hashes))),
  paste0("- Those row versions absent from PC: ", format_int(length(asym_missing_rows))),
  paste0("- GitHub unique observation keys sampled outside strict common operation: ", format_int(length(asym_key_hashes))),
  paste0("- Those observation keys absent from PC: ", format_int(length(asym_missing_keys))),
  "",
  "These are resilience/recovery gains, not primary scraper-disagreement counts.",
  "",
  "## C. All-archive union",
  "",
  paste0("- PC cumulative unique row versions: ", format_int(length(pc_row_hashes))),
  paste0("- GitHub retained unique row versions: ", format_int(length(all_github_row_hashes))),
  paste0("- GitHub row versions also present in PC archive: ", format_int(length(confirmed_row_union)), " (", format_pct(all_row_pct), ")"),
  paste0("- GitHub row versions absent from PC archive: ", format_int(length(missing_row_union))),
  paste0("- Canonical PC + GitHub union row versions: ", format_int(length(canonical_row_union))),
  paste0("- PC cumulative unique observation keys: ", format_int(length(pc_key_hashes))),
  paste0("- GitHub retained unique observation keys: ", format_int(length(all_github_key_hashes))),
  paste0("- GitHub observation keys also present in PC archive: ", format_int(length(confirmed_key_union)), " (", format_pct(all_key_pct), ")"),
  paste0("- GitHub observation keys absent from PC archive: ", format_int(length(missing_key_union))),
  paste0("- Canonical PC + GitHub union observation keys: ", format_int(length(canonical_key_union))),
  "",
  "## Interpretation",
  "",
  "- `strict_common_operation` is the primary validation scope.",
  "- `pc_outage`, `github_only_tail`, and `github_only_head` are asymmetric-coverage scopes. Their GitHub-only rows show what the redundant collector preserved while equivalent PC collection was unavailable.",
  "- `confirmed_exact_row_version` means the exact GitHub-published row version (`row_hash`) occurs somewhere in the cumulative PC archive.",
  "- `same_key_different_version` means the PC archive contains the same station/time observation identity but not that exact published revision.",
  "- `github_only_observation_key` means no PC version of that logical observation occurs in the cumulative PC archive.",
  "- Because CNEMC is mutable and the collectors run at different times, all-archive overlap is provenance context, not the primary reliability score.",
  "- Both collectors use the same CNEMC parser; this validates acquisition/provenance, not independent semantic interpretation of source fields.",
  "",
  "## Outputs",
  "",
  paste0("- `", basename(SNAPSHOT_SUMMARY_PATH), "`"),
  paste0("- `", basename(GLOBAL_SUMMARY_PATH), "`"),
  paste0("- `", basename(SCOPE_SUMMARY_PATH), "`"),
  paste0("- `", basename(OUTAGE_INTERVAL_PATH), "`"),
  paste0("- `", basename(EXCEPTION_PATH), "`")
)

writeLines(report_lines, REPORT_PATH, useBytes = TRUE)

# -----------------------------------------------------------------------------
# 9. Console summary
# -----------------------------------------------------------------------------
cat("\nCNEMC row-level reconciliation complete.\n")
cat("\nA. STRICT COMMON-OPERATION VALIDATION\n")
cat("  Retained GitHub full checkpoints:", sum(snapshot_summary$coverage_scope == "strict_common_operation"), "\n")
cat("  GitHub unique row versions:", length(strict_row_hashes), "\n")
cat(
  "  Exact row versions present in PC:",
  length(strict_confirmed_rows),
  sprintf("(%.2f%%)\n", strict_row_pct)
)
cat("  GitHub unique observation keys:", length(strict_key_hashes), "\n")
cat(
  "  Observation keys present in PC:",
  length(strict_confirmed_keys),
  sprintf("(%.2f%%)\n", strict_key_pct)
)
cat("  Observation keys absent from PC:", length(setdiff(strict_key_hashes, pc_key_hashes)), "\n")

cat("\nB. ASYMMETRIC / OUTAGE COVERAGE\n")
cat("  Detected internal PC outages:", nrow(internal_outages), "\n")
if (nrow(asymmetric_intervals) > 0L) {
  for (i in seq_len(nrow(asymmetric_intervals))) {
    x <- asymmetric_intervals[i, ]
    cat(
      "  ", x$interval_type, ": ",
      format_utc(x$interval_start_utc), " -> ", format_utc(x$interval_end_utc),
      " | GitHub-only keys absent from PC=",
      x$github_only_observation_keys_absent_from_pc,
      "\n",
      sep = ""
    )
  }
}
cat("  GitHub unique row versions in asymmetric coverage:", length(asym_row_hashes), "\n")
cat("  Row versions absent from PC:", length(asym_missing_rows), "\n")
cat("  GitHub unique observation keys in asymmetric coverage:", length(asym_key_hashes), "\n")
cat("  Observation keys absent from PC:", length(asym_missing_keys), "\n")

cat("\nC. ALL-ARCHIVE UNION\n")
cat("  PC unique row versions:", length(pc_row_hashes), "\n")
cat("  GitHub retained unique row versions:", length(all_github_row_hashes), "\n")
cat("  Canonical union row versions:", length(canonical_row_union), "\n")
cat("  PC unique observation keys:", length(pc_key_hashes), "\n")
cat("  GitHub retained unique observation keys:", length(all_github_key_hashes), "\n")
cat("  Canonical union observation keys:", length(canonical_key_union), "\n")
cat("  All-archive GitHub row-version confirmation:", sprintf("%.2f%%", all_row_pct), "\n")
cat("  All-archive GitHub key confirmation:", sprintf("%.2f%%", all_key_pct), "\n")

cat("\nOutputs:\n")
cat(" ", SNAPSHOT_SUMMARY_PATH, "\n")
cat(" ", GLOBAL_SUMMARY_PATH, "\n")
cat(" ", SCOPE_SUMMARY_PATH, "\n")
cat(" ", OUTAGE_INTERVAL_PATH, "\n")
cat(" ", EXCEPTION_PATH, "\n")
cat(" ", REPORT_PATH, "\n")
