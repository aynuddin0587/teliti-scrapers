# ============================================================================
# Build CNEMC <-> Qingyue station reference crosswalk
#
# This script performs no web requests. It uses only locally archived data.
# Run manually after Qingyue catalogue updates, or let script 12 call the
# ensure_cnemc_station_crosswalk() function directly.
# ============================================================================

options(stringsAsFactors = FALSE)

PROJECT_DIR <- "D:/# R Project/penelitian"
RECON_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation")

CNEMC_ANALYSIS_HELPER <- file.path(
  RECON_DIR,
  "script",
  "cnemc_analysis_helpers.R"
)

CNEMC_OBSERVATIONS <- file.path(
  PROJECT_DIR,
  "nmemc",
  "data",
  "surfacewater",
  "processed",
  "nmemc_surfacewater_observations.rds"
)

QINGYUE_STATION_CATALOG <- file.path(
  PROJECT_DIR,
  "nmemc",
  "data",
  "surfacewater",
  "qingyue",
  "processed",
  "qingyue_national_station_catalog_unfiltered.csv"
)

CNEMC_COORD_CROSSWALK <- file.path(
  PROJECT_DIR,
  "nmemc",
  "data",
  "surfacewater",
  "processed",
  "cnemc_station_crosswalk.csv"
)

CNEMC_STATION_OVERRIDES <- file.path(
  RECON_DIR,
  "reference",
  "cnemc_station_overrides.csv"
)

CNEMC_STATION_QA_DIR <- file.path(
  PROJECT_DIR,
  "nmemc",
  "data",
  "surfacewater",
  "processed",
  "station_reference_qa"
)

if (!file.exists(CNEMC_ANALYSIS_HELPER)) {
  stop("Missing helper: ", CNEMC_ANALYSIS_HELPER, call. = FALSE)
}

source(CNEMC_ANALYSIS_HELPER, encoding = "UTF-8")

ensure_cnemc_station_crosswalk(
  cnemc_observations_path = CNEMC_OBSERVATIONS,
  qingyue_catalog_path = QINGYUE_STATION_CATALOG,
  output_path = CNEMC_COORD_CROSSWALK,
  overrides_path = CNEMC_STATION_OVERRIDES,
  qa_dir = CNEMC_STATION_QA_DIR,
  force = TRUE,
  quiet = FALSE
)

cat("\nDone.\n")
cat("Crosswalk:\n", CNEMC_COORD_CROSSWALK, "\n", sep = "")
cat("QA directory:\n", CNEMC_STATION_QA_DIR, "\n", sep = "")
cat("Manual override file:\n", CNEMC_STATION_OVERRIDES, "\n", sep = "")
