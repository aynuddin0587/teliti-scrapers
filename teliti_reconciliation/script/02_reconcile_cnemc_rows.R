# Teliti CNEMC row-level reconciliation
#
# Purpose:
#   Compare retained GitHub CNEMC processed checkpoints against the cumulative
#   Windows-PC CNEMC observation archive using observation_key_hash and row_hash.
#
# Interpretation:
#   - row_hash match: the exact published row version was captured by the PC.
#   - observation_key_hash match but row_hash mismatch: the PC captured the same
#     station/time observation key, but not that exact published revision.
#   - neither match: the retained GitHub row version/key is absent from the PC
#     cumulative archive and should be reviewed as a possible transient record
#     missed by the PC collector.
#
# This is an acquisition/provenance validation layer. Both collectors use the
# same parser, so it does not constitute an independent validation of the
# semantic interpretation of CNEMC fields.
#
# Outputs under teliti_reconciliation/output/cnemc:
#   - cnemc_row_reconciliation_snapshot_summary.csv
#   - cnemc_row_reconciliation_global_summary.csv
#   - cnemc_row_reconciliation_exceptions.csv.gz
#   - cnemc_row_reconciliation.md

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

PC_OBSERVATIONS_PATH <- file.path(
  PRIMARY_ROOT,
  "nmemc", "data", "surfacewater", "processed",
  "nmemc_surfacewater_observations.rds"
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

normalize_relative_path <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  x <- sub("^/+", "", x)
  x
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
# 3. Read PC master and GitHub retained-checkpoint manifest
# -----------------------------------------------------------------------------

assert_file(PC_OBSERVATIONS_PATH, "PC CNEMC cumulative observations")
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
    snapshot_md5 = normalize_hash(snapshot_md5),
    processed_file = trimws(as.character(processed_file)),
    retained_processed = !is.na(processed_file) & processed_file != ""
  )

if (any(!valid_md5(manifest$snapshot_md5))) {
  stop("GitHub snapshot manifest contains invalid snapshot_md5 values.")
}

retained <- manifest %>%
  filter(retained_processed) %>%
  distinct(snapshot_md5, .keep_all = TRUE)

if (nrow(retained) == 0L) {
  stop("No retained GitHub processed CNEMC checkpoints are available.")
}

# Optional context from collection-level temporal reconciliation.
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
# 4. Compare every retained GitHub checkpoint with the PC cumulative archive
# -----------------------------------------------------------------------------

snapshot_summaries <- vector("list", nrow(retained))
exception_rows <- vector("list", nrow(retained))
all_github_row_hashes <- character(0)
all_github_key_hashes <- character(0)

for (i in seq_len(nrow(retained))) {
  m <- retained[i, , drop = FALSE]
  snapshot_md5 <- m$snapshot_md5[[1]]

  gh <- read_github_snapshot(
    relative_path = m$processed_file[[1]],
    expected_md5 = snapshot_md5
  )

  gh <- gh %>%
    mutate(
      pc_exact_row_version = row_hash %in% pc_row_hashes,
      pc_same_observation_key = observation_key_hash %in% pc_key_hashes,
      reconciliation_class = case_when(
        pc_exact_row_version ~ "confirmed_exact_row_version",
        pc_same_observation_key ~ "same_key_different_version",
        TRUE ~ "github_only_observation_key"
      )
    )

  all_github_row_hashes <- union(all_github_row_hashes, gh$row_hash)
  all_github_key_hashes <- union(all_github_key_hashes, gh$observation_key_hash)

  n_rows <- nrow(gh)
  n_unique_rows <- n_distinct(gh$row_hash)
  n_unique_keys <- n_distinct(gh$observation_key_hash)
  exact_n <- sum(gh$pc_exact_row_version)
  same_key_diff_n <- sum(
    !gh$pc_exact_row_version & gh$pc_same_observation_key
  )
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

  exceptions <- gh %>%
    filter(!pc_exact_row_version) %>%
    mutate(
      github_snapshot_md5 = snapshot_md5,
      github_collected_at = as.character(m$collected_at[[1]]),
      github_processed_file = m$processed_file[[1]]
    ) %>%
    select(
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

  exception_rows[[i]] <- exceptions
}

snapshot_summary <- bind_rows(snapshot_summaries) %>%
  arrange(github_collected_at)

exceptions <- bind_rows(exception_rows)

# -----------------------------------------------------------------------------
# 5. Global reconciliation metrics
# -----------------------------------------------------------------------------

all_github_row_hashes <- unique(all_github_row_hashes)
all_github_key_hashes <- unique(all_github_key_hashes)

confirmed_row_union <- intersect(all_github_row_hashes, pc_row_hashes)
missing_row_union <- setdiff(all_github_row_hashes, pc_row_hashes)
confirmed_key_union <- intersect(all_github_key_hashes, pc_key_hashes)
missing_key_union <- setdiff(all_github_key_hashes, pc_key_hashes)

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
    "exception_rows_across_checkpoints"
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
    nrow(exceptions)
  )
)

# -----------------------------------------------------------------------------
# 6. Write outputs
# -----------------------------------------------------------------------------

readr::write_csv(snapshot_summary, SNAPSHOT_SUMMARY_PATH, na = "")
readr::write_csv(global_summary, GLOBAL_SUMMARY_PATH, na = "")

if (nrow(exceptions) > 0L) {
  readr::write_csv(exceptions, EXCEPTION_PATH, na = "")
} else {
  # Preserve a stable empty-schema output rather than leaving a stale file from
  # an older reconciliation run.
  empty_exceptions <- tibble(
    github_collected_at = character(),
    github_snapshot_md5 = character(),
    github_processed_file = character(),
    reconciliation_class = character(),
    observation_key_hash = character(),
    row_hash = character()
  )
  readr::write_csv(empty_exceptions, EXCEPTION_PATH, na = "")
}

get_global <- function(metric) {
  x <- global_summary$value[global_summary$metric == metric]
  if (length(x) == 0L) return(NA_real_)
  suppressWarnings(as.numeric(x[[1]]))
}

row_confirm_pct <- get_global("github_row_version_confirmation_pct")
key_confirm_pct <- get_global("github_observation_key_confirmation_pct")

report_lines <- c(
  "# CNEMC row-level collector reconciliation",
  "",
  paste0("Generated from ", nrow(snapshot_summary), " retained GitHub full checkpoint(s)."),
  "",
  "## Global capture comparison",
  "",
  paste0("- PC cumulative unique row versions: ", length(pc_row_hashes)),
  paste0("- PC cumulative unique observation keys: ", length(pc_key_hashes)),
  paste0("- GitHub retained unique row versions: ", length(all_github_row_hashes)),
  paste0("- GitHub retained unique observation keys: ", length(all_github_key_hashes)),
  paste0(
    "- GitHub row versions also present in PC archive: ",
    length(confirmed_row_union),
    " (", format_pct(row_confirm_pct), ")"
  ),
  paste0(
    "- GitHub row versions absent from PC archive: ",
    length(missing_row_union)
  ),
  paste0(
    "- GitHub observation keys also present in PC archive: ",
    length(confirmed_key_union),
    " (", format_pct(key_confirm_pct), ")"
  ),
  paste0(
    "- GitHub observation keys absent from PC archive: ",
    length(missing_key_union)
  ),
  "",
  "## Interpretation",
  "",
  "- `confirmed_exact_row_version` means the exact GitHub-published row version (`row_hash`) occurs in the cumulative PC archive.",
  "- `same_key_different_version` means the PC archive contains the same station/time observation identity but not that exact published revision.",
  "- `github_only_observation_key` means neither that exact row version nor its observation key occurs in the PC cumulative archive.",
  "- Because GitHub retains only periodic full checkpoints, PC-only row versions are not treated as collector failures in this report.",
  "- Both collectors use the same CNEMC parsing code. This validates acquisition coverage/provenance, not independent semantic interpretation of source fields.",
  "",
  "## Per-checkpoint summary",
  ""
)

if (nrow(snapshot_summary) > 0L) {
  checkpoint_lines <- vapply(
    seq_len(nrow(snapshot_summary)),
    function(i) {
      x <- snapshot_summary[i, ]
      paste0(
        "- ", x$github_collected_at,
        " | rows=", x$processed_rows,
        " | exact-PC=", x$confirmed_exact_row_versions,
        " (", format_pct(x$confirmed_exact_pct), ")",
        " | same-key/different-version=", x$same_key_different_version,
        " | GitHub-only-key=", x$github_only_observation_keys,
        if (!is.na(x$exact_temporal_pair_seen)) {
          paste0(" | exact temporal-pair snapshot=", x$exact_temporal_pair_seen)
        } else {
          ""
        }
      )
    },
    character(1)
  )
  report_lines <- c(report_lines, checkpoint_lines)
}

report_lines <- c(
  report_lines,
  "",
  "## Output files",
  "",
  paste0("- `", basename(SNAPSHOT_SUMMARY_PATH), "`"),
  paste0("- `", basename(GLOBAL_SUMMARY_PATH), "`"),
  paste0("- `", basename(EXCEPTION_PATH), "`"),
  ""
)

writeLines(report_lines, REPORT_PATH, useBytes = TRUE)

# -----------------------------------------------------------------------------
# 7. Console summary
# -----------------------------------------------------------------------------

cat("\nCNEMC row-level reconciliation complete.\n")
cat("Retained GitHub full checkpoints:", nrow(snapshot_summary), "\n")
cat("GitHub retained unique row versions:", length(all_github_row_hashes), "\n")
cat(
  "Exact GitHub row versions present in PC archive:",
  length(confirmed_row_union),
  sprintf("(%.2f%%)\n", row_confirm_pct)
)
cat(
  "GitHub row versions absent from PC archive:",
  length(missing_row_union),
  "\n"
)
cat(
  "GitHub retained observation keys present in PC archive:",
  length(confirmed_key_union),
  sprintf("(%.2f%%)\n", key_confirm_pct)
)
cat(
  "GitHub observation keys absent from PC archive:",
  length(missing_key_union),
  "\n"
)
cat("Exception rows across retained checkpoints:", nrow(exceptions), "\n")
cat("\nOutputs:\n")
cat(" ", SNAPSHOT_SUMMARY_PATH, "\n")
cat(" ", GLOBAL_SUMMARY_PATH, "\n")
cat(" ", EXCEPTION_PATH, "\n")
cat(" ", REPORT_PATH, "\n")