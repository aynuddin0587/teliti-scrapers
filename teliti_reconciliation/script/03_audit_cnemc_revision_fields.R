# Teliti CNEMC revision-field audit
#
# Purpose:
#   Explain CNEMC row-level reconciliation exceptions by identifying which
#   scientific fields differ when GitHub captured the same observation key but
#   a different row version than the Windows-PC cumulative archive.
#
# Inputs:
#   - PC cumulative CNEMC observations RDS
#   - row-level reconciliation exceptions CSV.GZ
#   - retained GitHub processed checkpoints referenced by the exceptions file
#
# Outputs under teliti_reconciliation/output/cnemc:
#   - cnemc_revision_field_summary.csv
#   - cnemc_revision_field_differences.csv.gz
#   - cnemc_github_only_observation_keys.csv
#   - cnemc_revision_audit.md

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

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

FIELD_DIFF_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_field_differences.csv.gz"
)

GITHUB_ONLY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_github_only_observation_keys.csv"
)

REPORT_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_audit.md"
)

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

assert_columns(
  pc,
  c("observation_key_hash", "row_hash"),
  "PC CNEMC cumulative observations"
)

assert_columns(
  exceptions,
  c(
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
  readr::write_csv(tibble(), FIELD_DIFF_PATH)
  readr::write_csv(tibble(), GITHUB_ONLY_PATH)
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

github_only <- exceptions %>%
  filter(reconciliation_class == "github_only_observation_key") %>%
  distinct(observation_key_hash, .keep_all = TRUE) %>%
  arrange(github_collected_at, observation_key_hash)

readr::write_csv(github_only, GITHUB_ONLY_PATH, na = "")

revision_exceptions <- exceptions %>%
  filter(reconciliation_class == "same_key_different_version") %>%
  distinct(row_hash, .keep_all = TRUE)

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

    fields <- intersect(scientific_fields, intersect(names(gh_row), names(pc_candidates)))
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

if (nrow(field_diff) == 0L) {
  field_summary <- tibble(
    differing_field = character(),
    difference_records = integer(),
    unique_observation_keys = integer(),
    pct_revision_keys = numeric()
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
}

readr::write_csv(field_summary, FIELD_SUMMARY_PATH, na = "")
readr::write_csv(field_diff, FIELD_DIFF_PATH, na = "")

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

report_lines <- c(
  "# CNEMC revision-field audit",
  "",
  paste0("- Unique same-key/different-version GitHub rows audited: ", n_revision_rows),
  paste0("- Unique observation keys represented by those revisions: ", n_revision_keys),
  paste0("- Unique GitHub-only observation keys: ", n_github_only_keys),
  paste0("- Revision keys with no difference in selected scientific fields: ", n_no_scientific_diff),
  "",
  "## Most frequently changed scientific fields",
  ""
)

if (nrow(field_summary) > 0L) {
  summary_lines <- vapply(
    seq_len(nrow(field_summary)),
    function(i) {
      x <- field_summary[i, ]
      paste0(
        "- `", x$differing_field, "`: ",
        x$unique_observation_keys,
        " unique observation key(s); ",
        format(round(x$pct_revision_keys, 2), nsmall = 2, trim = TRUE),
        "% of revised keys"
      )
    },
    character(1)
  )
  report_lines <- c(report_lines, summary_lines)
} else {
  report_lines <- c(report_lines, "- No field-level differences were produced.")
}

report_lines <- c(
  report_lines,
  "",
  "## Interpretation",
  "",
  "- This audit compares each GitHub revision with the closest PC row version for the same observation key, where 'closest' means the fewest differing selected scientific fields.",
  "- It is designed to distinguish substantive environmental-value revisions from differences caused only by non-scientific/provenance fields.",
  "- A GitHub-only observation key remains an acquisition exception because no PC version of that logical observation exists in the cumulative PC archive.",
  "- Both collectors still use the same parser, so this is a provenance/acquisition comparison rather than independent semantic validation of the CNEMC source.",
  "",
  "## Outputs",
  "",
  paste0("- Field summary: `", basename(FIELD_SUMMARY_PATH), "`"),
  paste0("- Long field differences: `", basename(FIELD_DIFF_PATH), "`"),
  paste0("- Unique GitHub-only keys: `", basename(GITHUB_ONLY_PATH), "`")
)

writeLines(report_lines, REPORT_PATH, useBytes = TRUE)

cat("\nCNEMC revision-field audit complete.\n")
cat("Unique revised observation keys:", n_revision_keys, "\n")
cat("Unique GitHub-only observation keys:", n_github_only_keys, "\n")
if (nrow(field_summary) > 0L) {
  cat("\nMost frequently changed fields:\n")
  print(head(field_summary, 10), n = 10)
}
cat("\nOutputs:\n")
cat(" ", FIELD_SUMMARY_PATH, "\n")
cat(" ", FIELD_DIFF_PATH, "\n")
cat(" ", GITHUB_ONLY_PATH, "\n")
cat(" ", REPORT_PATH, "\n")