# Teliti CNEMC revision-magnitude audit
#
# Purpose:
#   Quantify how large substantive CNEMC revisions are after the field-level
#   audit has identified which scientific variables changed between a retained
#   GitHub row version and the closest PC row version for the same observation
#   key.
#
# Inputs:
#   teliti_reconciliation/output/cnemc/
#     - cnemc_revision_field_differences.csv.gz
#     - cnemc_github_only_observation_keys.csv
#
# Outputs:
#   - cnemc_revision_magnitude_details.csv.gz
#   - cnemc_revision_magnitude_summary.csv
#   - cnemc_revision_class_transitions.csv
#   - cnemc_revision_magnitude_audit.md
#
# Notes:
#   - Magnitudes are summarized within fields only; units differ among fields.
#   - Raw-value qualifiers such as <, >, <= and >= are retained and flagged.
#   - The paired PC version is inherited from 03_audit_cnemc_revision_fields.R,
#     where "closest" means the fewest differing selected scientific fields.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

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

CLASS_TRANSITION_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_class_transitions.csv"
)

REPORT_PATH <- file.path(
  OUTPUT_DIR,
  "cnemc_revision_magnitude_audit.md"
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
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE, type = 7))
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

assert_file(FIELD_DIFF_PATH, "CNEMC revision field differences")

field_diff <- readr::read_csv(
  FIELD_DIFF_PATH,
  show_col_types = FALSE,
  progress = FALSE
)

assert_columns(
  field_diff,
  c(
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

summary <- detail %>%
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
    p90_absolute_change = safe_quantile(absolute_change[numeric_pair_available], 0.90),
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

readr::write_csv(summary, SUMMARY_PATH, na = "")

class_transitions <- detail %>%
  filter(differing_field == "water_quality_class_code") %>%
  transmute(
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
  arrange(github_collected_at, observation_key_hash)

readr::write_csv(class_transitions, CLASS_TRANSITION_PATH, na = "")

n_revision_keys <- n_distinct(field_diff$observation_key_hash)
n_numeric_revision_keys <- n_distinct(
  detail$observation_key_hash[detail$numeric_field]
)
n_github_only <- if (file.exists(GITHUB_ONLY_PATH)) {
  github_only <- readr::read_csv(
    GITHUB_ONLY_PATH,
    show_col_types = FALSE,
    progress = FALSE
  )
  if ("observation_key_hash" %in% names(github_only)) {
    n_distinct(github_only$observation_key_hash)
  } else {
    nrow(github_only)
  }
} else {
  NA_integer_
}

report_lines <- c(
  "# CNEMC revision-magnitude audit",
  "",
  paste0("- Revised observation keys represented: ", n_revision_keys),
  paste0("- Revised keys involving numeric/class fields: ", n_numeric_revision_keys),
  paste0("- Unique GitHub-only observation keys from acquisition audit: ", n_github_only),
  "",
  "## Numeric revision magnitude",
  ""
)

if (nrow(summary) > 0L) {
  magnitude_lines <- vapply(
    seq_len(nrow(summary)),
    function(i) {
      x <- summary[i, ]
      paste0(
        "- `", x$differing_field, "`: ",
        x$unique_observation_keys, " revised key(s); median |change| = ",
        ifelse(is.na(x$median_absolute_change), "NA", format(signif(x$median_absolute_change, 6), trim = TRUE)),
        "; p90 |change| = ",
        ifelse(is.na(x$p90_absolute_change), "NA", format(signif(x$p90_absolute_change, 6), trim = TRUE)),
        "; qualifier changes = ", x$qualifier_changes
      )
    },
    character(1)
  )
  report_lines <- c(report_lines, magnitude_lines)
} else {
  report_lines <- c(report_lines, "- No numeric field revisions were available.")
}

report_lines <- c(
  report_lines,
  "",
  "## Water-quality class transitions",
  "",
  paste0("- Class-code revisions observed: ", nrow(class_transitions)),
  "",
  "## Interpretation",
  "",
  "- Absolute and percentage changes are summarized within each parameter only because the parameters use different units and scales.",
  "- A percentage change can be unstable when the PC comparison value is near zero, so absolute changes should be inspected alongside percentages.",
  "- Raw censoring/inequality qualifiers (<, >, <=, >=) are preserved and counted separately; a qualifier change can matter even when the extracted numeric value is unchanged.",
  "- This script quantifies source revisions; it does not decide which revision is scientifically preferable.",
  "- The eventual canonical dataset should preserve all observed row versions and assign preferred/final-version status in a separate derivation layer.",
  "",
  "## Outputs",
  "",
  paste0("- Detailed magnitude audit: `", basename(DETAIL_PATH), "`"),
  paste0("- Field summary: `", basename(SUMMARY_PATH), "`"),
  paste0("- Water-quality class transitions: `", basename(CLASS_TRANSITION_PATH), "`")
)

writeLines(report_lines, REPORT_PATH, useBytes = TRUE)

cat("\nCNEMC revision-magnitude audit complete.\n")
cat("Revised observation keys represented:", n_revision_keys, "\n")
cat("Revised keys involving numeric/class fields:", n_numeric_revision_keys, "\n")
cat("Unique GitHub-only observation keys:", n_github_only, "\n")
cat("\nMagnitude summary:\n")
print(summary, n = nrow(summary), width = Inf)
if (nrow(class_transitions) > 0L) {
  cat("\nWater-quality class transitions:\n")
  print(class_transitions, n = nrow(class_transitions), width = Inf)
}
cat("\nOutputs:\n")
cat(" ", DETAIL_PATH, "\n")
cat(" ", SUMMARY_PATH, "\n")
cat(" ", CLASS_TRANSITION_PATH, "\n")
cat(" ", REPORT_PATH, "\n")