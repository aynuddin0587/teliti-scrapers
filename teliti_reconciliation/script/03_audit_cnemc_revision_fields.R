# CNEMC revision-field audit - outage-aware
#
# Purpose:
#   Explain same-observation/different-version CNEMC exceptions while retaining
#   the coverage scope assigned by 02_reconcile_cnemc_rows.R. This separates
#   revisions observed during strict common operation from revisions observed
#   while the PC collector was unavailable.
#
# Existing outputs are preserved. New outputs:
#   - cnemc_revision_field_summary_by_scope.csv
#   - cnemc_revision_scope_summary.csv

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

EXCEPTION_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_row_reconciliation_exceptions.csv.gz"
)

FIELD_SUMMARY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_field_summary.csv"
)
FIELD_SCOPE_SUMMARY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_field_summary_by_scope.csv"
)
FIELD_DIFF_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_field_differences.csv.gz"
)
GITHUB_ONLY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_github_only_observation_keys.csv"
)
REVISION_SCOPE_SUMMARY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_scope_summary.csv"
)
REPORT_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_audit.md"
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
    stop(label, " is missing required column(s): ", paste(missing, collapse = ", "))
  }
}

normalize_hash <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x == ""] <- NA_character_
  x
}

normalize_relative_path <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  sub("^/+", "", x)
}

normalize_compare_value <- function(x) {
  if (length(x) == 0L || is.null(x)) return(NA_character_)
  if (inherits(x, "POSIXt")) {
    if (is.na(x[[1]])) return(NA_character_)
    return(format(as.POSIXct(x[[1]], tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    if (is.na(x[[1]])) return(NA_character_)
    return(format(x[[1]], "%Y-%m-%d"))
  }
  value <- as.character(x[[1]])
  if (is.na(value)) return(NA_character_)
  value <- trimws(value)
  if (value == "") return(NA_character_)
  value
}

values_equal <- function(a, b) {
  a <- normalize_compare_value(a)
  b <- normalize_compare_value(b)
  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.na(a) || is.na(b)) return(FALSE)
  identical(a, b)
}

scope_priority <- function(x) {
  match(
    x,
    c(
      "strict_common_operation",
      "pc_outage",
      "github_only_tail",
      "github_only_head",
      "unclassified_all_archive"
    ),
    nomatch = 99L
  )
}

safe_n_distinct <- function(x) {
  dplyr::n_distinct(x[!is.na(x)])
}

scientific_fields <- c(
  "area",
  "river_basin",
  "monitoring_section",
  "monitoring_time_raw",
  "water_quality_class_code",
  "water_temperature_c_raw",
  "ph_raw",
  "dissolved_oxygen_mg_l_raw",
  "conductivity_raw",
  "turbidity_ntu_raw",
  "permanganate_index_mg_l_raw",
  "ammonia_nitrogen_mg_l_raw",
  "total_phosphorus_mg_l_raw",
  "total_nitrogen_mg_l_raw",
  "chlorophyll_a_raw",
  "algal_density_raw",
  "water_quality_class",
  "observation_datetime"
)

# -----------------------------------------------------------------------------
# 3. Read inputs
# -----------------------------------------------------------------------------
assert_file(PC_OBSERVATIONS_PATH, "PC CNEMC cumulative observations")
assert_file(EXCEPTION_PATH, "CNEMC row reconciliation exceptions")

pc <- readRDS(PC_OBSERVATIONS_PATH) %>%
  mutate(
    observation_key_hash = normalize_hash(observation_key_hash),
    row_hash = normalize_hash(row_hash)
  )

exceptions <- readr::read_csv(
  EXCEPTION_PATH,
  show_col_types = FALSE,
  progress = FALSE
) %>%
  mutate(
    observation_key_hash = normalize_hash(observation_key_hash),
    row_hash = normalize_hash(row_hash)
  )

if (!"coverage_scope" %in% names(exceptions)) {
  warning(
    "Exception file has no coverage_scope column. Run the outage-aware ",
    "02_reconcile_cnemc_rows.R first. Treating existing rows as unclassified."
  )
  exceptions$coverage_scope <- "unclassified_all_archive"
}

assert_columns(
  pc,
  c("observation_key_hash", "row_hash"),
  "PC CNEMC cumulative observations"
)
assert_columns(
  exceptions,
  c(
    "coverage_scope",
    "github_collected_at",
    "github_processed_file",
    "reconciliation_class",
    "observation_key_hash",
    "row_hash"
  ),
  "CNEMC row reconciliation exceptions"
)

if (nrow(exceptions) == 0L) {
  readr::write_csv(tibble(), FIELD_SUMMARY_PATH)
  readr::write_csv(tibble(), FIELD_SCOPE_SUMMARY_PATH)
  readr::write_csv(tibble(), FIELD_DIFF_PATH)
  readr::write_csv(tibble(), GITHUB_ONLY_PATH)
  readr::write_csv(tibble(), REVISION_SCOPE_SUMMARY_PATH)
  writeLines(
    c(
      "# CNEMC revision-field audit",
      "",
      "No row-level reconciliation exceptions were present."
    ),
    REPORT_PATH
  )
  cat("No CNEMC row-level exceptions to audit.\n")
  quit(save = "no", status = 0L)
}

# If a logical key/version appears in more than one scope, strict common
# operation gets priority. This prevents the same source record from being
# double-counted as both a validation record and an outage recovery record.
github_only <- exceptions %>%
  filter(reconciliation_class == "github_only_observation_key") %>%
  mutate(scope_rank = scope_priority(coverage_scope)) %>%
  arrange(observation_key_hash, scope_rank, github_collected_at) %>%
  group_by(observation_key_hash) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-scope_rank) %>%
  arrange(coverage_scope, github_collected_at, observation_key_hash)

readr::write_csv(github_only, GITHUB_ONLY_PATH, na = "")

revision_exceptions <- exceptions %>%
  filter(reconciliation_class == "same_key_different_version") %>%
  mutate(scope_rank = scope_priority(coverage_scope)) %>%
  arrange(row_hash, scope_rank, github_collected_at) %>%
  group_by(row_hash) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-scope_rank)

# -----------------------------------------------------------------------------
# 4. Recover GitHub rows and compare with closest PC version
# -----------------------------------------------------------------------------
snapshot_cache <- new.env(parent = emptyenv())

read_snapshot_cached <- function(relative_path) {
  key <- normalize_relative_path(relative_path)
  if (exists(key, envir = snapshot_cache, inherits = FALSE)) {
    return(get(key, envir = snapshot_cache, inherits = FALSE))
  }

  full_path <- file.path(CLOUD_BACKUP_ROOT, key)
  assert_file(full_path, "Retained GitHub processed checkpoint")

  dat <- readr::read_csv(
    full_path,
    show_col_types = FALSE,
    progress = FALSE
  ) %>%
    mutate(
      observation_key_hash = normalize_hash(observation_key_hash),
      row_hash = normalize_hash(row_hash)
    )

  assign(key, dat, envir = snapshot_cache)
  dat
}

compare_candidate <- function(gh_row, pc_row, fields) {
  differs <- vapply(
    fields,
    function(field) !values_equal(gh_row[[field]], pc_row[[field]]),
    logical(1)
  )
  names(differs) <- fields
  differs
}

field_differences <- list()
comparison_index <- 0L

if (nrow(revision_exceptions) > 0L) {
  for (i in seq_len(nrow(revision_exceptions))) {
    ex <- revision_exceptions[i, , drop = FALSE]

    gh_snapshot <- read_snapshot_cached(ex$github_processed_file[[1]])
    gh_candidates <- gh_snapshot %>%
      filter(row_hash == ex$row_hash[[1]])

    if (nrow(gh_candidates) != 1L) {
      stop(
        "Expected exactly one GitHub row_hash in checkpoint; found ",
        nrow(gh_candidates),
        " for ", ex$row_hash[[1]]
      )
    }

    gh_row <- gh_candidates[1, , drop = FALSE]
    pc_candidates <- pc %>%
      filter(observation_key_hash == ex$observation_key_hash[[1]])

    if (nrow(pc_candidates) == 0L) {
      stop(
        "same_key_different_version exception has no PC candidate for key ",
        ex$observation_key_hash[[1]]
      )
    }

    fields <- intersect(
      scientific_fields,
      intersect(names(gh_row), names(pc_candidates))
    )
    if (length(fields) == 0L) {
      stop("No common scientific fields available for revision comparison.")
    }

    candidate_scores <- vector("list", nrow(pc_candidates))
    for (j in seq_len(nrow(pc_candidates))) {
      diff_flags <- compare_candidate(
        gh_row,
        pc_candidates[j, , drop = FALSE],
        fields
      )

      candidate_scores[[j]] <- tibble(
        pc_candidate_index = j,
        pc_row_hash = pc_candidates$row_hash[[j]],
        differing_field_count = sum(diff_flags),
        differing_fields = paste(names(diff_flags)[diff_flags], collapse = ";")
      )
    }

    scores <- bind_rows(candidate_scores) %>%
      arrange(differing_field_count, pc_candidate_index)

    best <- scores[1, , drop = FALSE]
    best_pc <- pc_candidates[best$pc_candidate_index[[1]], , drop = FALSE]
    best_diff_flags <- compare_candidate(gh_row, best_pc, fields)
    changed_fields <- names(best_diff_flags)[best_diff_flags]

    if (length(changed_fields) == 0L) {
      changed_fields <- "<none_in_selected_scientific_fields>"
    }

    for (field in changed_fields) {
      comparison_index <- comparison_index + 1L

      if (identical(field, "<none_in_selected_scientific_fields>")) {
        gh_value <- NA_character_
        pc_value <- NA_character_
      } else {
        gh_value <- normalize_compare_value(gh_row[[field]])
        pc_value <- normalize_compare_value(best_pc[[field]])
      }

      field_differences[[comparison_index]] <- tibble(
        coverage_scope = ex$coverage_scope[[1]],
        github_collected_at = as.character(ex$github_collected_at[[1]]),
        github_processed_file = ex$github_processed_file[[1]],
        observation_key_hash = ex$observation_key_hash[[1]],
        github_row_hash = ex$row_hash[[1]],
        closest_pc_row_hash = best$pc_row_hash[[1]],
        pc_candidate_versions = nrow(pc_candidates),
        differing_field_count = best$differing_field_count[[1]],
        differing_fields = best$differing_fields[[1]],
        differing_field = field,
        github_value = gh_value,
        pc_value = pc_value,
        area = if ("area" %in% names(gh_row)) as.character(gh_row$area[[1]]) else NA_character_,
        river_basin = if ("river_basin" %in% names(gh_row)) as.character(gh_row$river_basin[[1]]) else NA_character_,
        monitoring_section = if ("monitoring_section" %in% names(gh_row)) as.character(gh_row$monitoring_section[[1]]) else NA_character_,
        monitoring_time_raw = if ("monitoring_time_raw" %in% names(gh_row)) as.character(gh_row$monitoring_time_raw[[1]]) else NA_character_
      )
    }
  }
}

field_diff <- bind_rows(field_differences)

# -----------------------------------------------------------------------------
# 5. Summaries
# -----------------------------------------------------------------------------
if (nrow(field_diff) == 0L) {
  field_summary <- tibble(
    differing_field = character(),
    difference_records = integer(),
    unique_observation_keys = integer(),
    pct_revision_keys = numeric()
  )
  field_scope_summary <- tibble(
    coverage_scope = character(),
    differing_field = character(),
    difference_records = integer(),
    unique_observation_keys = integer(),
    pct_revision_keys_within_scope = numeric()
  )
} else {
  total_revision_keys <- n_distinct(field_diff$observation_key_hash)

  field_summary <- field_diff %>%
    count(differing_field, name = "difference_records", sort = TRUE) %>%
    left_join(
      field_diff %>%
        distinct(differing_field, observation_key_hash) %>%
        count(differing_field, name = "unique_observation_keys"),
      by = "differing_field"
    ) %>%
    mutate(
      pct_revision_keys = if (total_revision_keys > 0L) {
        100 * unique_observation_keys / total_revision_keys
      } else {
        NA_real_
      }
    )

  field_scope_source <- bind_rows(
    field_diff,
    field_diff %>%
      filter(coverage_scope != "strict_common_operation") %>%
      mutate(coverage_scope = "asymmetric_coverage"),
    field_diff %>% mutate(coverage_scope = "all_archive")
  )

  scope_key_totals <- field_scope_source %>%
    distinct(coverage_scope, observation_key_hash) %>%
    count(coverage_scope, name = "scope_revision_keys")

  field_scope_summary <- field_scope_source %>%
    count(coverage_scope, differing_field, name = "difference_records") %>%
    left_join(
      field_scope_source %>%
        distinct(coverage_scope, differing_field, observation_key_hash) %>%
        count(
          coverage_scope,
          differing_field,
          name = "unique_observation_keys"
        ),
      by = c("coverage_scope", "differing_field")
    ) %>%
    left_join(scope_key_totals, by = "coverage_scope") %>%
    mutate(
      pct_revision_keys_within_scope = if_else(
        scope_revision_keys > 0L,
        100 * unique_observation_keys / scope_revision_keys,
        NA_real_
      )
    ) %>%
    arrange(
      factor(
        coverage_scope,
        levels = c(
          "strict_common_operation",
          "asymmetric_coverage",
          "pc_outage",
          "github_only_tail",
          "github_only_head",
          "all_archive"
        )
      ),
      desc(unique_observation_keys),
      differing_field
    )
}

scope_levels <- unique(c(
  "strict_common_operation",
  "pc_outage",
  "github_only_tail",
  "github_only_head",
  as.character(revision_exceptions$coverage_scope),
  as.character(github_only$coverage_scope)
))
scope_levels <- scope_levels[!is.na(scope_levels)]

count_scope <- function(scope_name) {
  rev <- revision_exceptions %>% filter(coverage_scope == scope_name)
  only <- github_only %>% filter(coverage_scope == scope_name)
  tibble(
    coverage_scope = scope_name,
    unique_revision_row_versions = n_distinct(rev$row_hash),
    unique_revised_observation_keys = n_distinct(rev$observation_key_hash),
    unique_github_only_observation_keys = n_distinct(only$observation_key_hash)
  )
}

revision_scope_summary <- bind_rows(lapply(scope_levels, count_scope))

strict_revision_keys <- n_distinct(
  revision_exceptions$observation_key_hash[
    revision_exceptions$coverage_scope == "strict_common_operation"
  ]
)
strict_github_only_keys <- n_distinct(
  github_only$observation_key_hash[
    github_only$coverage_scope == "strict_common_operation"
  ]
)
asym_revision_keys <- n_distinct(
  revision_exceptions$observation_key_hash[
    revision_exceptions$coverage_scope != "strict_common_operation"
  ]
)
asym_github_only_keys <- n_distinct(
  github_only$observation_key_hash[
    github_only$coverage_scope != "strict_common_operation"
  ]
)

readr::write_csv(field_summary, FIELD_SUMMARY_PATH, na = "")
readr::write_csv(field_scope_summary, FIELD_SCOPE_SUMMARY_PATH, na = "")
readr::write_csv(field_diff, FIELD_DIFF_PATH, na = "")
readr::write_csv(revision_scope_summary, REVISION_SCOPE_SUMMARY_PATH, na = "")

n_revision_keys <- n_distinct(revision_exceptions$observation_key_hash)
n_revision_rows <- nrow(revision_exceptions)
n_github_only_keys <- nrow(github_only)
n_no_scientific_diff <- if (nrow(field_diff) == 0L) {
  0L
} else {
  n_distinct(
    field_diff$observation_key_hash[
      field_diff$differing_field == "<none_in_selected_scientific_fields>"
    ]
  )
}

format_top_fields <- function(scope_name, n = 10L) {
  x <- field_scope_summary %>%
    filter(coverage_scope == scope_name) %>%
    slice_head(n = n)
  if (nrow(x) == 0L) return("- None.")
  vapply(
    seq_len(nrow(x)),
    function(i) {
      row <- x[i, ]
      paste0(
        "- `", row$differing_field, "`: ",
        row$unique_observation_keys,
        " unique observation key(s); ",
        format(round(row$pct_revision_keys_within_scope, 2), nsmall = 2, trim = TRUE),
        "% of revised keys in this scope"
      )
    },
    character(1)
  )
}

# -----------------------------------------------------------------------------
# 6. Report
# -----------------------------------------------------------------------------
report_lines <- c(
  "# CNEMC revision-field audit",
  "",
  "## A. Strict common-operation validation",
  "",
  paste0("- Revised shared observation keys: ", strict_revision_keys),
  paste0("- GitHub-only observation keys during strict common operation: ", strict_github_only_keys),
  "",
  "### Most frequently changed fields during strict common operation",
  "",
  format_top_fields("strict_common_operation"),
  "",
  "## B. Asymmetric coverage / outage context",
  "",
  paste0("- Revised observation keys first represented outside strict common operation: ", asym_revision_keys),
  paste0("- GitHub-only recovery-gain observation keys: ", asym_github_only_keys),
  "",
  "### Most frequently changed fields in asymmetric coverage",
  "",
  format_top_fields("asymmetric_coverage"),
  "",
  "Revisions in this section are real source-version differences, but the PC collector was not equivalently available when GitHub captured them. They should not be interpreted as primary scraper disagreement.",
  "",
  "## C. All-archive revision provenance",
  "",
  paste0("- Unique same-key/different-version GitHub rows audited: ", n_revision_rows),
  paste0("- Unique observation keys represented by those revisions: ", n_revision_keys),
  paste0("- Unique GitHub-only observation keys: ", n_github_only_keys),
  paste0("- Revision keys with no difference in selected scientific fields: ", n_no_scientific_diff),
  "",
  "## Interpretation",
  "",
  "- Each GitHub revision is compared with the closest PC row version for the same observation key; 'closest' means the fewest differing selected scientific fields.",
  "- `strict_common_operation` is the primary scope for evaluating source revisions seen while both collection systems were operational.",
  "- `pc_outage`, `github_only_tail`, and `github_only_head` preserve asymmetric-coverage context from script 02.",
  "- A GitHub-only observation key in asymmetric coverage is primarily a redundancy/recovery gain, not evidence of scraper disagreement.",
  "- Both collectors use the same parser, so this is a provenance/acquisition comparison rather than independent semantic validation of CNEMC fields.",
  "",
  "## Outputs",
  "",
  paste0("- Field summary, all archive: `", basename(FIELD_SUMMARY_PATH), "`"),
  paste0("- Field summary by scope: `", basename(FIELD_SCOPE_SUMMARY_PATH), "`"),
  paste0("- Long field differences: `", basename(FIELD_DIFF_PATH), "`"),
  paste0("- Unique GitHub-only keys: `", basename(GITHUB_ONLY_PATH), "`"),
  paste0("- Revision scope summary: `", basename(REVISION_SCOPE_SUMMARY_PATH), "`")
)

writeLines(report_lines, REPORT_PATH, useBytes = TRUE)

# -----------------------------------------------------------------------------
# 7. Console summary
# -----------------------------------------------------------------------------
cat("\nCNEMC revision-field audit complete.\n")
cat("\nA. STRICT COMMON-OPERATION VALIDATION\n")
cat("  Unique revised observation keys:", strict_revision_keys, "\n")
cat("  Unique GitHub-only observation keys:", strict_github_only_keys, "\n")

strict_fields <- field_scope_summary %>%
  filter(coverage_scope == "strict_common_operation") %>%
  slice_head(n = 10)
if (nrow(strict_fields) > 0L) {
  cat("\n  Most frequently changed fields:\n")
  print(
    strict_fields %>%
      select(
        differing_field,
        difference_records,
        unique_observation_keys,
        pct_revision_keys_within_scope
      ),
    n = nrow(strict_fields)
  )
}

cat("\nB. ASYMMETRIC / OUTAGE COVERAGE\n")
cat("  Unique revised observation keys:", asym_revision_keys, "\n")
cat("  GitHub-only recovery-gain keys:", asym_github_only_keys, "\n")

cat("\nC. ALL-ARCHIVE PROVENANCE\n")
cat("  Unique revised observation keys:", n_revision_keys, "\n")
cat("  Unique GitHub-only observation keys:", n_github_only_keys, "\n")

cat("\nOutputs:\n")
cat(" ", FIELD_SUMMARY_PATH, "\n")
cat(" ", FIELD_SCOPE_SUMMARY_PATH, "\n")
cat(" ", FIELD_DIFF_PATH, "\n")
cat(" ", GITHUB_ONLY_PATH, "\n")
cat(" ", REVISION_SCOPE_SUMMARY_PATH, "\n")
cat(" ", REPORT_PATH, "\n")
