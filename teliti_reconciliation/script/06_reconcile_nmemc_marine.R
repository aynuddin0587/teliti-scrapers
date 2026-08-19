# ============================================================
# Teliti - NMEMC marine PC <-> GitHub reconciliation
#
# Purpose
#   1. Verify byte-level equality of annual raw JSON files.
#   2. Independently normalize the GitHub raw files.
#   3. Verify that the PC processed master is reproducible from
#      the GitHub raw archive.
#
# This script is read-only with respect to both source archives.
# Generated reports are written under:
#   teliti_reconciliation/output/nmemc_marine/
# ============================================================

options(stringsAsFactors = FALSE)

required_packages <- c("jsonlite", "dplyr", "readr", "tibble", "digest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "))
}

# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------
project_root <- normalizePath(
  Sys.getenv("TELITI_DATA_ROOT", unset = "D:/# R Project/penelitian"),
  winslash = "/",
  mustWork = TRUE
)

backup_root <- normalizePath(
  Sys.getenv("TELITI_CLOUD_BACKUP_ROOT", unset = "D:/# R Project/teliti-data-backup"),
  winslash = "/",
  mustWork = TRUE
)

pc_raw_dir <- file.path(project_root, "nmemc", "data", "raw")
pc_archive_dir <- file.path(pc_raw_dir, "archive")
pc_master_path <- file.path(
  project_root, "nmemc", "data", "processed", "nmemc_water_master.rds"
)

gh_raw_dir <- file.path(backup_root, "nmemc_marine", "state", "raw")
gh_snapshot_root <- file.path(backup_root, "nmemc_marine", "snapshots")

output_dir <- file.path(
  project_root, "teliti_reconciliation", "output", "nmemc_marine"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(pc_master_path)) {
  stop("PC processed master not found: ", pc_master_path)
}

# ------------------------------------------------------------
# 2. Helpers
# ------------------------------------------------------------
sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

latest_matching_file <- function(path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) return(NA_character_)
  info <- file.info(files)
  files[[which.max(info$mtime)]]
}

normalize_nmemc <- function(df, source_year, source_file) {
  expected <- c(
    "sea", "province", "city", "site", "lon", "lat", "minitor_month",
    "pH", "rjy", "hxxyl", "wjd", "hxlxy", "syl", "szlb"
  )

  missing <- setdiff(expected, names(df))
  if (length(missing) > 0L) {
    stop(
      "Unexpected schema in ", basename(source_file),
      "; missing field(s): ", paste(missing, collapse = ", ")
    )
  }

  tibble::as_tibble(df) |>
    dplyr::transmute(
      source_year = as.integer(source_year),
      sea = as.character(.data$sea),
      province = as.character(.data$province),
      city = as.character(.data$city),
      site_code = as.character(.data$site),
      longitude_raw = as.character(.data$lon),
      latitude_raw = as.character(.data$lat),
      longitude = suppressWarnings(
        readr::parse_number(as.character(.data$lon), na = c("", "-", "--", "NA"))
      ),
      latitude = suppressWarnings(
        readr::parse_number(as.character(.data$lat), na = c("", "-", "--", "NA"))
      ),
      monitor_time_raw = as.character(.data$minitor_month),
      ph_raw = as.character(.data$pH),
      dissolved_oxygen_mg_l_raw = as.character(.data$rjy),
      cod_mg_l_raw = as.character(.data$hxxyl),
      inorganic_nitrogen_mg_l_raw = as.character(.data$wjd),
      reactive_phosphate_mg_l_raw = as.character(.data$hxlxy),
      petroleum_mg_l_raw = as.character(.data$syl),
      water_quality_class = as.character(.data$szlb),
      source_file = basename(source_file)
    )
}

parse_raw_year <- function(path) {
  year <- as.integer(sub("^water([0-9]{4})\\.json$", "\\1", basename(path)))
  x <- suppressWarnings(jsonlite::fromJSON(path, simplifyDataFrame = TRUE))
  if (!is.data.frame(x)) {
    stop("Expected JSON array of records in ", basename(path))
  }
  normalize_nmemc(x, year, path)
}

canonicalize_master <- function(x) {
  expected <- c(
    "source_year", "sea", "province", "city", "site_code",
    "longitude_raw", "latitude_raw", "longitude", "latitude",
    "monitor_time_raw", "ph_raw", "dissolved_oxygen_mg_l_raw",
    "cod_mg_l_raw", "inorganic_nitrogen_mg_l_raw",
    "reactive_phosphate_mg_l_raw", "petroleum_mg_l_raw",
    "water_quality_class", "source_file"
  )

  missing <- setdiff(expected, names(x))
  if (length(missing) > 0L) {
    stop("Processed master is missing column(s): ", paste(missing, collapse = ", "))
  }

  x |>
    dplyr::select(dplyr::all_of(expected)) |>
    dplyr::mutate(
      source_year = as.integer(.data$source_year),
      dplyr::across(
        c(
          "sea", "province", "city", "site_code", "longitude_raw",
          "latitude_raw", "monitor_time_raw", "ph_raw",
          "dissolved_oxygen_mg_l_raw", "cod_mg_l_raw",
          "inorganic_nitrogen_mg_l_raw", "reactive_phosphate_mg_l_raw",
          "petroleum_mg_l_raw", "water_quality_class", "source_file"
        ),
        as.character
      ),
      longitude = as.numeric(.data$longitude),
      latitude = as.numeric(.data$latitude)
    ) |>
    dplyr::arrange(
      .data$source_year,
      .data$sea,
      .data$province,
      .data$city,
      .data$site_code,
      .data$monitor_time_raw,
      .data$longitude_raw,
      .data$latitude_raw,
      .data$ph_raw,
      .data$dissolved_oxygen_mg_l_raw,
      .data$cod_mg_l_raw,
      .data$inorganic_nitrogen_mg_l_raw,
      .data$reactive_phosphate_mg_l_raw,
      .data$petroleum_mg_l_raw,
      .data$water_quality_class,
      .data$source_file
    ) |>
    dplyr::ungroup()
}

# ------------------------------------------------------------
# 3. Raw-file reconciliation
# ------------------------------------------------------------
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- 2017:current_year

raw_results <- lapply(years, function(year) {
  pc_current <- file.path(pc_raw_dir, sprintf("water%d.json", year))
  gh_current <- file.path(gh_raw_dir, sprintf("water%d.json", year))

  pc_archive <- latest_matching_file(
    pc_archive_dir,
    sprintf("^water%d_[0-9]{8}_[0-9]{6}\\.json$", year)
  )
  gh_snapshot <- latest_matching_file(
    file.path(gh_snapshot_root, as.character(year)),
    sprintf("^water%d_[0-9]{8}_[0-9]{6}_[0-9a-fA-F]+\\.json$", year)
  )

  required <- c(pc_current = pc_current, gh_current = gh_current)
  missing_required <- names(required)[!file.exists(required)]
  if (length(missing_required) > 0L) {
    stop(
      "Missing required raw file(s) for ", year, ": ",
      paste(missing_required, collapse = ", ")
    )
  }

  pc_hash <- sha256_file(pc_current)
  gh_hash <- sha256_file(gh_current)
  pc_archive_hash <- if (!is.na(pc_archive) && file.exists(pc_archive)) {
    sha256_file(pc_archive)
  } else {
    NA_character_
  }
  gh_snapshot_hash <- if (!is.na(gh_snapshot) && file.exists(gh_snapshot)) {
    sha256_file(gh_snapshot)
  } else {
    NA_character_
  }

  pc_rows <- nrow(parse_raw_year(pc_current))
  gh_rows <- nrow(parse_raw_year(gh_current))

  tibble::tibble(
    year = year,
    pc_current_file = basename(pc_current),
    github_current_file = basename(gh_current),
    pc_archive_file = ifelse(is.na(pc_archive), NA_character_, basename(pc_archive)),
    github_snapshot_file = ifelse(is.na(gh_snapshot), NA_character_, basename(gh_snapshot)),
    pc_bytes = file.info(pc_current)$size,
    github_bytes = file.info(gh_current)$size,
    pc_sha256 = pc_hash,
    github_sha256 = gh_hash,
    pc_archive_sha256 = pc_archive_hash,
    github_snapshot_sha256 = gh_snapshot_hash,
    pc_vs_github_current_exact = identical(pc_hash, gh_hash),
    pc_current_vs_pc_archive_exact = !is.na(pc_archive_hash) && identical(pc_hash, pc_archive_hash),
    github_current_vs_snapshot_exact = !is.na(gh_snapshot_hash) && identical(gh_hash, gh_snapshot_hash),
    all_available_copies_exact = all(
      na.omit(c(pc_hash, gh_hash, pc_archive_hash, gh_snapshot_hash)) == pc_hash
    ),
    pc_rows = pc_rows,
    github_rows = gh_rows,
    raw_row_count_match = identical(pc_rows, gh_rows)
  )
}) |>
  dplyr::bind_rows()

# ------------------------------------------------------------
# 4. Rebuild normalized master from GitHub raw files
# ------------------------------------------------------------
gh_raw_files <- file.path(gh_raw_dir, sprintf("water%d.json", years))
if (any(!file.exists(gh_raw_files))) {
  stop("One or more GitHub canonical raw year files are missing.")
}

gh_master <- dplyr::bind_rows(lapply(gh_raw_files, parse_raw_year))
pc_master <- readRDS(pc_master_path)

pc_master_c <- canonicalize_master(pc_master)
gh_master_c <- canonicalize_master(gh_master)

same_dimensions <- identical(dim(pc_master_c), dim(gh_master_c))
master_equal <- isTRUE(all.equal(
  pc_master_c,
  gh_master_c,
  check.attributes = FALSE,
  tolerance = 0
))

pc_dup <- sum(duplicated(pc_master_c))
gh_dup <- sum(duplicated(gh_master_c))

annual_processing <- dplyr::full_join(
  pc_master_c |>
    dplyr::count(.data$source_year, name = "pc_processed_rows"),
  gh_master_c |>
    dplyr::count(.data$source_year, name = "github_rebuilt_rows"),
  by = "source_year"
) |>
  dplyr::mutate(
    row_count_match = .data$pc_processed_rows == .data$github_rebuilt_rows
  ) |>
  dplyr::arrange(.data$source_year)

processing_summary <- tibble::tibble(
  metric = c(
    "years_checked",
    "raw_pc_vs_github_exact_years",
    "raw_all_available_copies_exact_years",
    "raw_row_count_match_years",
    "pc_processed_rows",
    "github_rebuilt_rows",
    "processed_dimensions_match",
    "processed_master_exact_match",
    "pc_exact_duplicate_rows",
    "github_rebuilt_exact_duplicate_rows"
  ),
  value = c(
    as.character(nrow(raw_results)),
    as.character(sum(raw_results$pc_vs_github_current_exact)),
    as.character(sum(raw_results$all_available_copies_exact)),
    as.character(sum(raw_results$raw_row_count_match)),
    as.character(nrow(pc_master_c)),
    as.character(nrow(gh_master_c)),
    as.character(same_dimensions),
    as.character(master_equal),
    as.character(pc_dup),
    as.character(gh_dup)
  )
)

# ------------------------------------------------------------
# 5. Write reports
# ------------------------------------------------------------
raw_out <- file.path(output_dir, "nmemc_marine_raw_reconciliation.csv")
annual_out <- file.path(output_dir, "nmemc_marine_annual_processing_reconciliation.csv")
summary_out <- file.path(output_dir, "nmemc_marine_reconciliation_summary.csv")
md_out <- file.path(output_dir, "nmemc_marine_reconciliation.md")

readr::write_csv(raw_results, raw_out, na = "")
readr::write_csv(annual_processing, annual_out, na = "")
readr::write_csv(processing_summary, summary_out, na = "")

status <- if (
  all(raw_results$pc_vs_github_current_exact) &&
  all(raw_results$all_available_copies_exact) &&
  all(raw_results$raw_row_count_match) &&
  master_equal
) {
  "PASS"
} else {
  "REVIEW"
}

md_lines <- c(
  "# NMEMC marine collector reconciliation",
  "",
  paste0("**Overall status:** ", status),
  "",
  "## Raw acquisition",
  "",
  paste0("- Years checked: ", nrow(raw_results)),
  paste0(
    "- PC current vs GitHub current exact SHA-256 matches: ",
    sum(raw_results$pc_vs_github_current_exact), "/", nrow(raw_results)
  ),
  paste0(
    "- Years where all available PC/GitHub current/archive copies are identical: ",
    sum(raw_results$all_available_copies_exact), "/", nrow(raw_results)
  ),
  paste0(
    "- Annual raw row-count matches: ",
    sum(raw_results$raw_row_count_match), "/", nrow(raw_results)
  ),
  "",
  "## Processing reproducibility",
  "",
  paste0("- PC processed master rows: ", nrow(pc_master_c)),
  paste0("- GitHub-raw rebuilt master rows: ", nrow(gh_master_c)),
  paste0("- Dimensions match: ", same_dimensions),
  paste0("- Exact normalized processed-master match: ", master_equal),
  paste0("- PC exact duplicate rows: ", pc_dup),
  paste0("- GitHub-rebuilt exact duplicate rows: ", gh_dup),
  "",
  "## Interpretation",
  "",
  if (status == "PASS") {
    paste(
      "The independently maintained PC and GitHub NMEMC archives contain the same",
      "annual source bytes for every checked year, and the PC processed master is",
      "exactly reproducible from the GitHub raw archive under the same normalization logic."
    )
  } else {
    "At least one acquisition or processing comparison requires review before collector retirement."
  },
  ""
)
writeLines(md_lines, md_out, useBytes = TRUE)

# ------------------------------------------------------------
# 6. Console summary
# ------------------------------------------------------------
cat("\nNMEMC marine reconciliation complete.\n")
cat("Years checked:", nrow(raw_results), "\n")
cat(
  "Exact PC-current vs GitHub-current raw files:",
  sum(raw_results$pc_vs_github_current_exact), "/", nrow(raw_results), "\n"
)
cat(
  "All available copies identical:",
  sum(raw_results$all_available_copies_exact), "/", nrow(raw_results), "\n"
)
cat(
  "Raw annual row-count matches:",
  sum(raw_results$raw_row_count_match), "/", nrow(raw_results), "\n"
)
cat("PC processed master rows:", nrow(pc_master_c), "\n")
cat("GitHub-raw rebuilt master rows:", nrow(gh_master_c), "\n")
cat("Processed master exact match:", master_equal, "\n")
cat("Overall status:", status, "\n\n")
cat("Outputs:\n")
cat("  ", raw_out, "\n")
cat("  ", annual_out, "\n")
cat("  ", summary_out, "\n")
cat("  ", md_out, "\n")