# Teliti CNEMC collection-level reconciliation
#
# Purpose:
#   Compare the independent Windows-PC and GitHub Actions CNEMC collectors
#   without modifying either archive. This script reconciles acquisition
#   metadata only; row-level scientific observation reconciliation comes next.
#
# Outputs:
#   - cnemc_collection_reconciliation_summary.csv
#   - cnemc_source_state_reconciliation.csv
#   - cnemc_pc_to_github_pairs.csv
#   - cnemc_collection_reconciliation.md

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

PAIR_TOLERANCE_MINUTES <- suppressWarnings(as.numeric(Sys.getenv(
  "TELITI_CNEMC_PAIR_TOLERANCE_MINUTES",
  unset = "15"
)))

if (!is.finite(PAIR_TOLERANCE_MINUTES) || PAIR_TOLERANCE_MINUTES <= 0) {
  stop("TELITI_CNEMC_PAIR_TOLERANCE_MINUTES must be a positive number.")
}

PC_MANIFEST_PATH <- file.path(
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

PC_OBSERVATIONS_PATH <- file.path(
  PRIMARY_ROOT,
  "nmemc", "data", "surfacewater", "processed",
  "nmemc_surfacewater_observations.rds"
)

SUMMARY_CSV <- file.path(
  OUTPUT_DIR,
  "cnemc_collection_reconciliation_summary.csv"
)

STATE_CSV <- file.path(
  OUTPUT_DIR,
  "cnemc_source_state_reconciliation.csv"
)

PAIR_CSV <- file.path(
  OUTPUT_DIR,
  "cnemc_pc_to_github_pairs.csv"
)

SUMMARY_MD <- file.path(
  OUTPUT_DIR,
  "cnemc_collection_reconciliation.md"
)

# -----------------------------------------------------------------------------
# 2. Helpers
# -----------------------------------------------------------------------------

assert_file <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " not found: ", path)
  }
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

normalize_datetime <- function(x, label) {
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
    stop(
      "Could not parse ",
      sum(is.na(out)),
      " timestamp(s) in ",
      label,
      "."
    )
  }

  out
}

normalize_hash <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x == ""] <- NA_character_
  x
}

valid_md5 <- function(x) {
  !is.na(x) & grepl("^[0-9a-f]{32}$", x)
}

safe_pct <- function(numerator, denominator) {
  if (is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }
  100 * numerator / denominator
}

format_utc <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_character_)
  }
  format(x, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

nearest_right_index <- function(left_time, right_time) {
  # right_time must be sorted ascending and non-empty.
  right_num <- as.numeric(right_time)
  left_num <- as.numeric(left_time)

  lower <- findInterval(left_num, right_num)
  upper <- pmin(lower + 1L, length(right_num))
  lower <- pmax(lower, 1L)

  lower_diff <- abs(left_num - right_num[lower])
  upper_diff <- abs(left_num - right_num[upper])

  ifelse(upper_diff < lower_diff, upper, lower)
}

write_metric <- function(metric, value, scope = "overall", note = NA_character_) {
  tibble(
    scope = scope,
    metric = metric,
    value = as.character(value),
    note = note
  )
}

# -----------------------------------------------------------------------------
# 3. Read and validate source manifests
# -----------------------------------------------------------------------------

assert_file(PC_MANIFEST_PATH, "PC CNEMC run manifest")
assert_file(
  GITHUB_COLLECTION_MANIFEST_PATH,
  "GitHub CNEMC collection manifest"
)
assert_file(
  GITHUB_SNAPSHOT_MANIFEST_PATH,
  "GitHub CNEMC snapshot manifest"
)
assert_file(PC_OBSERVATIONS_PATH, "PC CNEMC cumulative observations")

pc <- readr::read_csv(
  PC_MANIFEST_PATH,
  show_col_types = FALSE,
  progress = FALSE
)

gh <- readr::read_csv(
  GITHUB_COLLECTION_MANIFEST_PATH,
  show_col_types = FALSE,
  progress = FALSE
)

gh_snapshots <- readr::read_csv(
  GITHUB_SNAPSHOT_MANIFEST_PATH,
  show_col_types = FALSE,
  progress = FALSE
)

assert_columns(
  pc,
  c(
    "collected_at", "snapshot_md5", "changed", "rows",
    "total_pages", "page_size", "collector_id"
  ),
  "PC CNEMC run manifest"
)

assert_columns(
  gh,
  c(
    "collection_key", "collected_at", "snapshot_md5", "changed",
    "rows", "total_pages", "page_size", "collector_id"
  ),
  "GitHub CNEMC collection manifest"
)

assert_columns(
  gh_snapshots,
  c(
    "collected_at", "snapshot_md5", "raw_file", "processed_file"
  ),
  "GitHub CNEMC snapshot manifest"
)

pc <- pc %>%
  mutate(
    collected_at_utc = normalize_datetime(collected_at, "PC collected_at"),
    snapshot_md5 = normalize_hash(snapshot_md5),
    collector = "pc"
  ) %>%
  arrange(collected_at_utc)

gh <- gh %>%
  mutate(
    collected_at_utc = normalize_datetime(collected_at, "GitHub collected_at"),
    snapshot_md5 = normalize_hash(snapshot_md5),
    collector = "github"
  ) %>%
  arrange(collected_at_utc)

if (any(!valid_md5(pc$snapshot_md5))) {
  stop("PC manifest contains invalid or missing snapshot_md5 values.")
}

if (any(!valid_md5(gh$snapshot_md5))) {
  stop("GitHub manifest contains invalid or missing snapshot_md5 values.")
}

# The PC archive is loaded only to verify that the row-level reconciliation
# prerequisites are present. No observations are changed here.
pc_obs <- readRDS(PC_OBSERVATIONS_PATH)

required_obs_hashes <- c("observation_key_hash", "row_hash")
assert_columns(
  pc_obs,
  required_obs_hashes,
  "PC CNEMC cumulative observations"
)

# -----------------------------------------------------------------------------
# 4. Determine the fair common operating window
# -----------------------------------------------------------------------------

pc_start <- min(pc$collected_at_utc)
pc_end <- max(pc$collected_at_utc)
gh_start <- min(gh$collected_at_utc)
gh_end <- max(gh$collected_at_utc)

common_start <- max(pc_start, gh_start)
common_end <- min(pc_end, gh_end)

if (common_start > common_end) {
  stop(
    "PC and GitHub CNEMC manifests do not overlap in time. ",
    "PC: ", format_utc(pc_start), " to ", format_utc(pc_end),
    "; GitHub: ", format_utc(gh_start), " to ", format_utc(gh_end), "."
  )
}

pc_common <- pc %>%
  filter(collected_at_utc >= common_start, collected_at_utc <= common_end)

gh_common <- gh %>%
  filter(collected_at_utc >= common_start, collected_at_utc <= common_end)

# -----------------------------------------------------------------------------
# 5. Reconcile unique source states by snapshot MD5
# -----------------------------------------------------------------------------

summarize_states <- function(dat, prefix) {
  dat %>%
    group_by(snapshot_md5) %>%
    summarise(
      first_seen = min(collected_at_utc),
      last_seen = max(collected_at_utc),
      runs = n(),
      .groups = "drop"
    ) %>%
    rename_with(
      ~ paste0(prefix, "_", .x),
      -snapshot_md5
    )
}

pc_states <- summarize_states(pc, "pc")
gh_states <- summarize_states(gh, "github")

pc_common_hashes <- unique(pc_common$snapshot_md5)
gh_common_hashes <- unique(gh_common$snapshot_md5)

state_reconciliation <- full_join(
  pc_states,
  gh_states,
  by = "snapshot_md5"
) %>%
  mutate(
    seen_pc = !is.na(pc_first_seen),
    seen_github = !is.na(github_first_seen),
    source_state_class = case_when(
      seen_pc & seen_github ~ "confirmed_both",
      seen_pc & !seen_github ~ "pc_only",
      !seen_pc & seen_github ~ "github_only",
      TRUE ~ "unknown"
    ),
    seen_pc_common_window = snapshot_md5 %in% pc_common_hashes,
    seen_github_common_window = snapshot_md5 %in% gh_common_hashes,
    common_window_class = case_when(
      seen_pc_common_window & seen_github_common_window ~ "confirmed_both",
      seen_pc_common_window & !seen_github_common_window ~ "pc_only",
      !seen_pc_common_window & seen_github_common_window ~ "github_only",
      TRUE ~ "outside_common_window"
    )
  ) %>%
  arrange(
    factor(
      common_window_class,
      levels = c(
        "confirmed_both", "pc_only", "github_only", "outside_common_window"
      )
    ),
    snapshot_md5
  )

# -----------------------------------------------------------------------------
# 6. Pair each PC run to the nearest GitHub run within the common window
# -----------------------------------------------------------------------------

if (nrow(pc_common) > 0L && nrow(gh_common) > 0L) {
  nearest_idx <- nearest_right_index(
    pc_common$collected_at_utc,
    gh_common$collected_at_utc
  )

  nearest_gh <- gh_common[nearest_idx, , drop = FALSE]

  pairs <- tibble(
    pc_collected_at_utc = pc_common$collected_at_utc,
    pc_snapshot_md5 = pc_common$snapshot_md5,
    pc_rows = pc_common$rows,
    pc_changed = pc_common$changed,
    github_collected_at_utc = nearest_gh$collected_at_utc,
    github_snapshot_md5 = nearest_gh$snapshot_md5,
    github_rows = nearest_gh$rows,
    github_changed = nearest_gh$changed,
    github_collection_key = nearest_gh$collection_key
  ) %>%
    mutate(
      lag_minutes = as.numeric(
        difftime(
          github_collected_at_utc,
          pc_collected_at_utc,
          units = "mins"
        )
      ),
      abs_lag_minutes = abs(lag_minutes),
      within_tolerance = abs_lag_minutes <= PAIR_TOLERANCE_MINUTES,
      exact_snapshot_match = within_tolerance &
        pc_snapshot_md5 == github_snapshot_md5,
      row_count_match = within_tolerance & pc_rows == github_rows
    )
} else {
  pairs <- tibble()
}

# -----------------------------------------------------------------------------
# 7. Summary metrics
# -----------------------------------------------------------------------------

all_pc_states <- n_distinct(pc$snapshot_md5)
all_gh_states <- n_distinct(gh$snapshot_md5)
all_shared_states <- length(intersect(unique(pc$snapshot_md5), unique(gh$snapshot_md5)))
all_union_states <- length(union(unique(pc$snapshot_md5), unique(gh$snapshot_md5)))

common_pc_states <- n_distinct(pc_common$snapshot_md5)
common_gh_states <- n_distinct(gh_common$snapshot_md5)
common_shared_states <- length(intersect(pc_common_hashes, gh_common_hashes))
common_union_states <- length(union(pc_common_hashes, gh_common_hashes))

paired_within <- if (nrow(pairs) > 0L) sum(pairs$within_tolerance) else 0L
paired_exact <- if (nrow(pairs) > 0L) sum(pairs$exact_snapshot_match) else 0L
paired_rows_equal <- if (nrow(pairs) > 0L) {
  sum(pairs$within_tolerance & pairs$row_count_match)
} else {
  0L
}

median_abs_lag <- if (paired_within > 0L) {
  median(pairs$abs_lag_minutes[pairs$within_tolerance], na.rm = TRUE)
} else {
  NA_real_
}

summary_table <- bind_rows(
  write_metric("pc_manifest_runs", nrow(pc)),
  write_metric("github_collection_manifest_runs", nrow(gh)),
  write_metric("github_snapshot_manifest_states", nrow(gh_snapshots)),
  write_metric(
    "github_full_checkpoints_retained",
    sum(
      !is.na(gh_snapshots$raw_file) & gh_snapshots$raw_file != "" &
        !is.na(gh_snapshots$processed_file) & gh_snapshots$processed_file != ""
    )
  ),
  write_metric("pc_observation_rows", nrow(pc_obs)),
  write_metric("pc_unique_row_hashes", n_distinct(pc_obs$row_hash)),
  write_metric(
    "pc_unique_observation_keys",
    n_distinct(pc_obs$observation_key_hash)
  ),
  write_metric("pc_start", format_utc(pc_start)),
  write_metric("pc_end", format_utc(pc_end)),
  write_metric("github_start", format_utc(gh_start)),
  write_metric("github_end", format_utc(gh_end)),
  write_metric("common_window_start", format_utc(common_start)),
  write_metric("common_window_end", format_utc(common_end)),

  write_metric("pc_unique_source_states", all_pc_states, "all_time"),
  write_metric("github_unique_source_states", all_gh_states, "all_time"),
  write_metric("shared_source_states", all_shared_states, "all_time"),
  write_metric(
    "pc_states_seen_by_github_pct",
    round(safe_pct(all_shared_states, all_pc_states), 2),
    "all_time",
    "Descriptive source-state overlap; schedules differ."
  ),
  write_metric(
    "github_states_seen_by_pc_pct",
    round(safe_pct(all_shared_states, all_gh_states), 2),
    "all_time",
    "Descriptive source-state overlap; schedules differ."
  ),
  write_metric(
    "source_state_jaccard_pct",
    round(safe_pct(all_shared_states, all_union_states), 2),
    "all_time",
    "Intersection divided by union of unique snapshot hashes."
  ),

  write_metric(
    "pc_unique_source_states",
    common_pc_states,
    "common_window"
  ),
  write_metric(
    "github_unique_source_states",
    common_gh_states,
    "common_window"
  ),
  write_metric(
    "shared_source_states",
    common_shared_states,
    "common_window"
  ),
  write_metric(
    "pc_states_seen_by_github_pct",
    round(safe_pct(common_shared_states, common_pc_states), 2),
    "common_window",
    "Primary exact-state overlap metric."
  ),
  write_metric(
    "github_states_seen_by_pc_pct",
    round(safe_pct(common_shared_states, common_gh_states), 2),
    "common_window",
    "Primary exact-state overlap metric."
  ),
  write_metric(
    "source_state_jaccard_pct",
    round(safe_pct(common_shared_states, common_union_states), 2),
    "common_window"
  ),

  write_metric(
    "pc_runs_paired_within_tolerance",
    paired_within,
    "temporal_pairing"
  ),
  write_metric(
    "pair_tolerance_minutes",
    PAIR_TOLERANCE_MINUTES,
    "temporal_pairing"
  ),
  write_metric(
    "median_absolute_pair_lag_minutes",
    round(median_abs_lag, 2),
    "temporal_pairing"
  ),
  write_metric(
    "temporally_paired_exact_snapshot_matches",
    paired_exact,
    "temporal_pairing"
  ),
  write_metric(
    "temporally_paired_exact_snapshot_match_pct",
    round(safe_pct(paired_exact, paired_within), 2),
    "temporal_pairing",
    "Not expected to be 100% because CNEMC can change between collector times."
  ),
  write_metric(
    "temporally_paired_equal_row_counts",
    paired_rows_equal,
    "temporal_pairing"
  ),
  write_metric(
    "temporally_paired_equal_row_count_pct",
    round(safe_pct(paired_rows_equal, paired_within), 2),
    "temporal_pairing"
  )
)

# -----------------------------------------------------------------------------
# 8. Write outputs
# -----------------------------------------------------------------------------

readr::write_csv(summary_table, SUMMARY_CSV, na = "")
readr::write_csv(state_reconciliation, STATE_CSV, na = "")
readr::write_csv(pairs, PAIR_CSV, na = "")

common_class_counts <- state_reconciliation %>%
  count(common_window_class, name = "n")

get_common_n <- function(class_name) {
  value <- common_class_counts$n[common_class_counts$common_window_class == class_name]
  if (length(value) == 0L) 0L else value[[1]]
}

md_lines <- c(
  "# CNEMC collector reconciliation",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Interpretation",
  "",
  paste0(
    "This report compares the Windows PC and GitHub Actions CNEMC collectors. ",
    "Exact `snapshot_md5` overlap is a source-state overlap measure, not a ",
    "standalone reliability score, because the collectors run at different times."
  ),
  "",
  "## Common operating window",
  "",
  paste0("- Start: ", format_utc(common_start)),
  paste0("- End: ", format_utc(common_end)),
  "",
  "## Exact source-state overlap",
  "",
  paste0("- PC unique states: ", common_pc_states),
  paste0("- GitHub unique states: ", common_gh_states),
  paste0("- Confirmed by both: ", common_shared_states),
  paste0("- PC-only states: ", get_common_n("pc_only")),
  paste0("- GitHub-only states: ", get_common_n("github_only")),
  paste0(
    "- PC states also seen by GitHub: ",
    round(safe_pct(common_shared_states, common_pc_states), 2),
    "%"
  ),
  paste0(
    "- GitHub states also seen by PC: ",
    round(safe_pct(common_shared_states, common_gh_states), 2),
    "%"
  ),
  paste0(
    "- Source-state Jaccard overlap: ",
    round(safe_pct(common_shared_states, common_union_states), 2),
    "%"
  ),
  "",
  "## Nearest-in-time pairing",
  "",
  paste0("- Pair tolerance: ±", PAIR_TOLERANCE_MINUTES, " minutes"),
  paste0("- PC runs with a GitHub run within tolerance: ", paired_within),
  paste0("- Median absolute time lag: ", round(median_abs_lag, 2), " minutes"),
  paste0("- Exact snapshot matches among temporal pairs: ", paired_exact),
  paste0(
    "- Exact-match rate among temporal pairs: ",
    round(safe_pct(paired_exact, paired_within), 2),
    "%"
  ),
  paste0("- Equal row counts among temporal pairs: ", paired_rows_equal),
  paste0(
    "- Equal-row-count rate among temporal pairs: ",
    round(safe_pct(paired_rows_equal, paired_within), 2),
    "%"
  ),
  "",
  "## Row-level reconciliation readiness",
  "",
  paste0("- PC cumulative observations: ", format(nrow(pc_obs), big.mark = ",")),
  paste0("- Unique PC row hashes: ", format(n_distinct(pc_obs$row_hash), big.mark = ",")),
  paste0(
    "- Unique PC observation-key hashes: ",
    format(n_distinct(pc_obs$observation_key_hash), big.mark = ",")
  ),
  "- Required `row_hash` and `observation_key_hash` fields are present.",
  "",
  "## Next analytical step",
  "",
  paste0(
    "For GitHub snapshots retained as full checkpoints, compare row hashes ",
    "against the PC archive. This will distinguish true collector disagreement ",
    "from ordinary source changes between different collection times."
  )
)

writeLines(md_lines, SUMMARY_MD, useBytes = TRUE)

# -----------------------------------------------------------------------------
# 9. Console report
# -----------------------------------------------------------------------------

cat("\nCNEMC collection-level reconciliation complete.\n")
cat("Common window: ", format_utc(common_start), " -> ", format_utc(common_end), "\n", sep = "")
cat("PC unique source states in common window: ", common_pc_states, "\n", sep = "")
cat("GitHub unique source states in common window: ", common_gh_states, "\n", sep = "")
cat("Confirmed exact source states: ", common_shared_states, "\n", sep = "")
cat(
  "Source-state Jaccard overlap: ",
  round(safe_pct(common_shared_states, common_union_states), 2),
  "%\n",
  sep = ""
)
cat("PC runs paired within ±", PAIR_TOLERANCE_MINUTES, " min: ", paired_within, "\n", sep = "")
cat("Exact snapshot matches among temporal pairs: ", paired_exact, "\n", sep = "")
cat("Equal row counts among temporal pairs: ", paired_rows_equal, "\n", sep = "")
cat("\nOutputs:\n")
cat("  ", SUMMARY_CSV, "\n", sep = "")
cat("  ", STATE_CSV, "\n", sep = "")
cat("  ", PAIR_CSV, "\n", sep = "")
cat("  ", SUMMARY_MD, "\n", sep = "")