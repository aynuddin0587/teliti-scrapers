# CNEMC revision-magnitude audit - outage-aware
#
# Purpose:
#   Quantify substantive CNEMC revisions while separating revisions captured
#   during strict common operation from revisions captured during asymmetric
#   coverage / PC outages.
#
# Existing outputs are preserved. New output:
#   - cnemc_revision_magnitude_summary_by_scope.csv

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

RECON_ROOT <- Sys.getenv(
  "TELITI_RECON_ROOT",
  unset = file.path(PRIMARY_ROOT, "teliti_reconciliation")
)

OUTPUT_DIR <- file.path(RECON_ROOT, "output", "cnemc")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

FIELD_DIFF_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_field_differences.csv.gz"
)
GITHUB_ONLY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_github_only_observation_keys.csv"
)

DETAIL_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_magnitude_details.csv.gz"
)
SUMMARY_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_magnitude_summary.csv"
)
SUMMARY_SCOPE_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_magnitude_summary_by_scope.csv"
)
CLASS_TRANSITION_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_class_transitions.csv"
)
REPORT_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_magnitude_audit.md"
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

normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- NA_character_
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

extract_qualifier <- function(x) {
  x <- normalize_text(x)
  out <- rep(NA_character_, length(x))

  out[grepl("^(<=|≤|≦)", x, perl = TRUE)] <- "<="
  out[grepl("^(>=|≥|≧)", x, perl = TRUE)] <- ">="
  out[is.na(out) & grepl("^(<|＜)", x, perl = TRUE)] <- "<"
  out[is.na(out) & grepl("^(>|＞)", x, perl = TRUE)] <- ">"
  out[is.na(out) & !is.na(x)] <- "="

  out
}

extract_numeric <- function(x) {
  x <- normalize_text(x)
  out <- rep(NA_real_, length(x))

  for (i in seq_along(x)) {
    if (is.na(x[[i]])) next

    value <- gsub(",", "", x[[i]], fixed = TRUE)
    hit <- regexpr(
      "[-+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?",
      value,
      perl = TRUE
    )

    if (hit[[1]] > 0L) {
      token <- regmatches(value, hit)
      out[[i]] <- suppressWarnings(as.numeric(token))
    }
  }

  out
}

safe_quantile <- function(x, prob) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  as.numeric(
    stats::quantile(
      x,
      probs = prob,
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
  )
}

summarize_magnitude <- function(dat, scope_label = NULL) {
  if (nrow(dat) == 0L) {
    out <- tibble(
      differing_field = character(),
      difference_records = integer(),
      unique_observation_keys = integer(),
      numeric_pairs = integer(),
      qualifier_changes = integer(),
      github_higher = integer(),
      github_lower = integer(),
      same_numeric_value = integer(),
      median_absolute_change = numeric(),
      p90_absolute_change = numeric(),
      max_absolute_change = numeric(),
      median_absolute_pct_change = numeric(),
      p90_absolute_pct_change = numeric(),
      max_absolute_pct_change = numeric()
    )
    if (!is.null(scope_label)) {
      out <- out %>% mutate(coverage_scope = character(), .before = 1)
    }
    return(out)
  }

  out <- dat %>%
    filter(numeric_field) %>%
    group_by(differing_field) %>%
    summarise(
      difference_records = n(),
      unique_observation_keys = n_distinct(observation_key_hash),
      numeric_pairs = sum(numeric_pair_available, na.rm = TRUE),
      qualifier_changes = sum(qualifier_changed %in% TRUE, na.rm = TRUE),
      github_higher = sum(change_direction == "github_higher", na.rm = TRUE),
      github_lower = sum(change_direction == "github_lower", na.rm = TRUE),
      same_numeric_value = sum(change_direction == "same_numeric_value", na.rm = TRUE),
      median_absolute_change = if (any(numeric_pair_available)) {
        stats::median(absolute_change[numeric_pair_available], na.rm = TRUE)
      } else {
        NA_real_
      },
      p90_absolute_change = safe_quantile(
        absolute_change[numeric_pair_available],
        0.90
      ),
      max_absolute_change = if (any(numeric_pair_available)) {
        max(absolute_change[numeric_pair_available], na.rm = TRUE)
      } else {
        NA_real_
      },
      median_absolute_pct_change = if (any(is.finite(absolute_pct_change))) {
        stats::median(absolute_pct_change, na.rm = TRUE)
      } else {
        NA_real_
      },
      p90_absolute_pct_change = safe_quantile(absolute_pct_change, 0.90),
      max_absolute_pct_change = if (any(is.finite(absolute_pct_change))) {
        max(absolute_pct_change, na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    arrange(desc(unique_observation_keys), differing_field)

  if (!is.null(scope_label)) {
    out <- out %>% mutate(coverage_scope = scope_label, .before = 1)
  }
  out
}

numeric_fields <- c(
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
  "algal_density_raw"
)

# -----------------------------------------------------------------------------
# 3. Read field-level revision audit
# -----------------------------------------------------------------------------
assert_file(FIELD_DIFF_PATH, "CNEMC revision field differences")

field_diff <- readr::read_csv(
  FIELD_DIFF_PATH,
  show_col_types = FALSE,
  progress = FALSE
)

if (!"coverage_scope" %in% names(field_diff)) {
  warning(
    "Field-difference file has no coverage_scope column. Run outage-aware ",
    "scripts 02 and 03 first. Treating rows as unclassified."
  )
  field_diff$coverage_scope <- "unclassified_all_archive"
}

assert_columns(
  field_diff,
  c(
    "coverage_scope",
    "observation_key_hash",
    "github_row_hash",
    "closest_pc_row_hash",
    "differing_field",
    "github_value",
    "pc_value"
  ),
  "CNEMC revision field differences"
)

if (nrow(field_diff) == 0L) {
  readr::write_csv(tibble(), DETAIL_PATH)
  readr::write_csv(tibble(), SUMMARY_PATH)
  readr::write_csv(tibble(), SUMMARY_SCOPE_PATH)
  readr::write_csv(tibble(), CLASS_TRANSITION_PATH)
  writeLines(
    c(
      "# CNEMC revision-magnitude audit",
      "",
      "No field-level CNEMC revisions were available to quantify."
    ),
    REPORT_PATH
  )
  cat("No CNEMC revision-field differences to quantify.\n")
  quit(save = "no", status = 0L)
}

# -----------------------------------------------------------------------------
# 4. Quantify revision magnitudes
# -----------------------------------------------------------------------------
detail <- field_diff %>%
  mutate(
    github_value = normalize_text(github_value),
    pc_value = normalize_text(pc_value),
    numeric_field = differing_field %in% numeric_fields,
    github_qualifier = if_else(
      numeric_field,
      extract_qualifier(github_value),
      NA_character_
    ),
    pc_qualifier = if_else(
      numeric_field,
      extract_qualifier(pc_value),
      NA_character_
    ),
    github_numeric = if_else(
      numeric_field,
      extract_numeric(github_value),
      NA_real_
    ),
    pc_numeric = if_else(
      numeric_field,
      extract_numeric(pc_value),
      NA_real_
    ),
    numeric_pair_available =
      numeric_field & is.finite(github_numeric) & is.finite(pc_numeric),
    signed_change = if_else(
      numeric_pair_available,
      github_numeric - pc_numeric,
      NA_real_
    ),
    absolute_change = if_else(
      numeric_pair_available,
      abs(signed_change),
      NA_real_
    ),
    absolute_pct_change = if_else(
      numeric_pair_available & is.finite(pc_numeric) & pc_numeric != 0,
      100 * absolute_change / abs(pc_numeric),
      NA_real_
    ),
    change_direction = case_when(
      !numeric_pair_available ~ NA_character_,
      signed_change > 0 ~ "github_higher",
      signed_change < 0 ~ "github_lower",
      TRUE ~ "same_numeric_value"
    ),
    qualifier_changed = case_when(
      !numeric_field ~ NA,
      is.na(github_qualifier) & is.na(pc_qualifier) ~ FALSE,
      TRUE ~ github_qualifier != pc_qualifier
    )
  )

readr::write_csv(detail, DETAIL_PATH, na = "")

# Backward-compatible all-archive summary.
summary <- summarize_magnitude(detail)
readr::write_csv(summary, SUMMARY_PATH, na = "")

# New scope-aware summaries.
actual_scopes <- unique(as.character(detail$coverage_scope))
actual_scopes <- actual_scopes[!is.na(actual_scopes)]

scope_parts <- lapply(
  actual_scopes,
  function(s) summarize_magnitude(
    detail %>% filter(coverage_scope == s),
    s
  )
)

scope_parts[[length(scope_parts) + 1L]] <- summarize_magnitude(
  detail %>% filter(coverage_scope != "strict_common_operation"),
  "asymmetric_coverage"
)
scope_parts[[length(scope_parts) + 1L]] <- summarize_magnitude(
  detail,
  "all_archive"
)

summary_by_scope <- bind_rows(scope_parts) %>%
  arrange(
    factor(
      coverage_scope,
      levels = c(
        "strict_common_operation",
        "asymmetric_coverage",
        "pc_outage",
        "github_only_tail",
        "github_only_head",
        "unclassified_all_archive",
        "all_archive"
      )
    ),
    desc(unique_observation_keys),
    differing_field
  )

readr::write_csv(summary_by_scope, SUMMARY_SCOPE_PATH, na = "")

class_transitions <- detail %>%
  filter(differing_field == "water_quality_class_code") %>%
  transmute(
    coverage_scope,
    observation_key_hash,
    github_collected_at = if ("github_collected_at" %in% names(detail)) github_collected_at else NA_character_,
    area = if ("area" %in% names(detail)) area else NA_character_,
    river_basin = if ("river_basin" %in% names(detail)) river_basin else NA_character_,
    monitoring_section = if ("monitoring_section" %in% names(detail)) monitoring_section else NA_character_,
    monitoring_time_raw = if ("monitoring_time_raw" %in% names(detail)) monitoring_time_raw else NA_character_,
    pc_class_code = pc_numeric,
    github_class_code = github_numeric,
    class_code_change = signed_change,
    transition = paste0(
      ifelse(is.na(pc_numeric), "NA", format(pc_numeric, trim = TRUE)),
      " -> ",
      ifelse(is.na(github_numeric), "NA", format(github_numeric, trim = TRUE))
    )
  ) %>%
  arrange(coverage_scope, github_collected_at, observation_key_hash)

readr::write_csv(class_transitions, CLASS_TRANSITION_PATH, na = "")

# -----------------------------------------------------------------------------
# 5. Scope counts
# -----------------------------------------------------------------------------
strict_keys <- n_distinct(
  detail$observation_key_hash[
    detail$coverage_scope == "strict_common_operation"
  ]
)
asym_keys <- n_distinct(
  detail$observation_key_hash[
    detail$coverage_scope != "strict_common_operation"
  ]
)
all_keys <- n_distinct(detail$observation_key_hash)

strict_numeric_keys <- n_distinct(
  detail$observation_key_hash[
    detail$coverage_scope == "strict_common_operation" & detail$numeric_field
  ]
)
asym_numeric_keys <- n_distinct(
  detail$observation_key_hash[
    detail$coverage_scope != "strict_common_operation" & detail$numeric_field
  ]
)
all_numeric_keys <- n_distinct(detail$observation_key_hash[detail$numeric_field])

strict_class_n <- sum(
  class_transitions$coverage_scope == "strict_common_operation",
  na.rm = TRUE
)
asym_class_n <- sum(
  class_transitions$coverage_scope != "strict_common_operation",
  na.rm = TRUE
)

n_github_only <- NA_integer_
n_github_only_strict <- NA_integer_
n_github_only_asym <- NA_integer_

if (file.exists(GITHUB_ONLY_PATH)) {
  github_only <- readr::read_csv(
    GITHUB_ONLY_PATH,
    show_col_types = FALSE,
    progress = FALSE
  )

  if ("observation_key_hash" %in% names(github_only)) {
    n_github_only <- n_distinct(github_only$observation_key_hash)
  } else {
    n_github_only <- nrow(github_only)
  }

  if ("coverage_scope" %in% names(github_only) &&
      "observation_key_hash" %in% names(github_only)) {
    n_github_only_strict <- n_distinct(
      github_only$observation_key_hash[
        github_only$coverage_scope == "strict_common_operation"
      ]
    )
    n_github_only_asym <- n_distinct(
      github_only$observation_key_hash[
        github_only$coverage_scope != "strict_common_operation"
      ]
    )
  }
}

# -----------------------------------------------------------------------------
# 6. Report helpers
# -----------------------------------------------------------------------------
format_magnitude_lines <- function(scope_name, n = 12L) {
  x <- summary_by_scope %>%
    filter(coverage_scope == scope_name) %>%
    slice_head(n = n)

  if (nrow(x) == 0L) return("- No numeric field revisions were available.")

  vapply(
    seq_len(nrow(x)),
    function(i) {
      row <- x[i, ]
      paste0(
        "- `", row$differing_field, "`: ",
        row$unique_observation_keys,
        " revised key(s); median |change| = ",
        ifelse(
          is.na(row$median_absolute_change),
          "NA",
          format(signif(row$median_absolute_change, 6), trim = TRUE)
        ),
        "; p90 |change| = ",
        ifelse(
          is.na(row$p90_absolute_change),
          "NA",
          format(signif(row$p90_absolute_change, 6), trim = TRUE)
        ),
        "; qualifier changes = ", row$qualifier_changes
      )
    },
    character(1)
  )
}

# -----------------------------------------------------------------------------
# 7. Report
# -----------------------------------------------------------------------------
report_lines <- c(
  "# CNEMC revision-magnitude audit",
  "",
  "## A. Strict common-operation validation",
  "",
  paste0("- Revised observation keys represented: ", strict_keys),
  paste0("- Revised keys involving numeric/class fields: ", strict_numeric_keys),
  paste0("- GitHub-only observation keys in strict common operation: ", n_github_only_strict),
  paste0("- Water-quality class transitions: ", strict_class_n),
  "",
  "### Numeric revision magnitude",
  "",
  format_magnitude_lines("strict_common_operation"),
  "",
  "## B. Asymmetric coverage / outage context",
  "",
  paste0("- Revised observation keys represented: ", asym_keys),
  paste0("- Revised keys involving numeric/class fields: ", asym_numeric_keys),
  paste0("- GitHub-only recovery-gain observation keys: ", n_github_only_asym),
  paste0("- Water-quality class transitions: ", asym_class_n),
  "",
  "### Numeric revision magnitude",
  "",
  format_magnitude_lines("asymmetric_coverage"),
  "",
  "Revision magnitudes in this section remain scientifically real source-version changes, but they were captured without equivalent PC operating coverage. They should be interpreted as revision provenance plus redundancy benefit, not as primary scraper disagreement.",
  "",
  "## C. All-archive revision provenance",
  "",
  paste0("- Revised observation keys represented: ", all_keys),
  paste0("- Revised keys involving numeric/class fields: ", all_numeric_keys),
  paste0("- Unique GitHub-only observation keys from acquisition audit: ", n_github_only),
  paste0("- Water-quality class transitions: ", nrow(class_transitions)),
  "",
  "## Interpretation",
  "",
  "- Absolute and percentage changes are summarized within each parameter only because parameters use different units and scales.",
  "- A percentage change can be unstable when the PC comparison value is near zero; inspect absolute changes alongside percentages.",
  "- Raw censoring/inequality qualifiers (<, >, <=, >=) are preserved and counted separately.",
  "- This audit quantifies source revisions; it does not decide which revision is scientifically preferable.",
  "- `strict_common_operation` is the primary scope for comparing revision behavior while both collectors were available.",
  "- The canonical dataset should continue to preserve all observed row versions and assign latest/preferred-version status in a separate derivation layer.",
  "",
  "## Outputs",
  "",
  paste0("- Detailed magnitude audit: `", basename(DETAIL_PATH), "`"),
  paste0("- All-archive field summary: `", basename(SUMMARY_PATH), "`"),
  paste0("- Scope-aware field summary: `", basename(SUMMARY_SCOPE_PATH), "`"),
  paste0("- Water-quality class transitions: `", basename(CLASS_TRANSITION_PATH), "`")
)

writeLines(report_lines, REPORT_PATH, useBytes = TRUE)

# -----------------------------------------------------------------------------
# 8. Console summary
# -----------------------------------------------------------------------------
cat("\nCNEMC revision-magnitude audit complete.\n")

cat("\nA. STRICT COMMON-OPERATION VALIDATION\n")
cat("  Revised observation keys:", strict_keys, "\n")
cat("  Numeric/class revision keys:", strict_numeric_keys, "\n")
cat("  GitHub-only observation keys:", n_github_only_strict, "\n")
cat("  Water-quality class transitions:", strict_class_n, "\n")

strict_summary <- summary_by_scope %>%
  filter(coverage_scope == "strict_common_operation")
if (nrow(strict_summary) > 0L) {
  cat("\n  Magnitude summary:\n")
  print(
    strict_summary %>% select(-coverage_scope),
    n = nrow(strict_summary),
    width = Inf
  )
}

cat("\nB. ASYMMETRIC / OUTAGE COVERAGE\n")
cat("  Revised observation keys:", asym_keys, "\n")
cat("  Numeric/class revision keys:", asym_numeric_keys, "\n")
cat("  GitHub-only recovery-gain keys:", n_github_only_asym, "\n")
cat("  Water-quality class transitions:", asym_class_n, "\n")

cat("\nC. ALL-ARCHIVE PROVENANCE\n")
cat("  Revised observation keys:", all_keys, "\n")
cat("  Numeric/class revision keys:", all_numeric_keys, "\n")
cat("  GitHub-only observation keys:", n_github_only, "\n")
cat("  Water-quality class transitions:", nrow(class_transitions), "\n")

if (nrow(class_transitions) > 0L) {
  cat("\nWater-quality class transitions (all scopes):\n")
  print(class_transitions, n = nrow(class_transitions), width = Inf)
}

cat("\nOutputs:\n")
cat(" ", DETAIL_PATH, "\n")
cat(" ", SUMMARY_PATH, "\n")
cat(" ", SUMMARY_SCOPE_PATH, "\n")
cat(" ", CLASS_TRANSITION_PATH, "\n")
cat(" ", REPORT_PATH, "\n")
