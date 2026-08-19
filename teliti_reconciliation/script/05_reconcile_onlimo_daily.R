# ONLIMO daily PC <-> GitHub reconciliation
#
# Purpose:
#   Compare the Windows cumulative ONLIMO daily archive with the independently
#   collected GitHub backup without modifying either source archive.
#
# Outputs are written only under teliti_reconciliation/output/onlimo_daily/.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

TELITI_DATA_ROOT <- Sys.getenv(
  "TELITI_DATA_ROOT",
  unset = "D:/# R Project/penelitian"
)

TELITI_BACKUP_REPO <- Sys.getenv(
  "TELITI_BACKUP_REPO",
  unset = "D:/# R Project/teliti-data-backup"
)

PC_ONLIMO_DIR <- file.path(TELITI_DATA_ROOT, "onlimo", "data")
GH_ONLIMO_DIR <- file.path(TELITI_BACKUP_REPO, "onlimo_daily")

PC_ARCHIVE_FILE <- file.path(PC_ONLIMO_DIR, "onlimo_daily_parameters_archive.csv")
PC_STATE_FILE <- file.path(PC_ONLIMO_DIR, "onlimo_daily_station_state.csv")
PC_CATALOG_FILE <- file.path(PC_ONLIMO_DIR, "onlimo_station_catalog.csv")

GH_STATE_FILE <- file.path(GH_ONLIMO_DIR, "state", "station_state.csv")
GH_MANIFEST_FILE <- file.path(GH_ONLIMO_DIR, "manifests", "snapshot_manifest.csv")
GH_SNAPSHOT_DIR <- file.path(GH_ONLIMO_DIR, "snapshots")
GH_CATALOG_DIR <- file.path(GH_ONLIMO_DIR, "catalogs")

OUTPUT_DIR <- file.path(
  TELITI_DATA_ROOT,
  "teliti_reconciliation",
  "output",
  "onlimo_daily"
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

stop_missing <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
}

normalize_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "<NA>"
  trimws(x)
}

normalize_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep("<NA>", length(x))
  ok <- !is.na(x)
  out[ok] <- format(
    x[ok],
    digits = 15,
    scientific = FALSE,
    trim = TRUE,
    drop0trailing = FALSE
  )
  out
}

normalize_field <- function(x) {
  if (inherits(x, "Date")) {
    out <- as.character(x)
    out[is.na(out)] <- "<NA>"
    return(out)
  }
  if (inherits(x, c("POSIXct", "POSIXt"))) {
    out <- format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    out[is.na(x)] <- "<NA>"
    return(out)
  }
  if (is.numeric(x) || is.integer(x)) {
    return(normalize_num(x))
  }
  normalize_chr(x)
}

make_payload_signature <- function(df, fields) {
  if (length(fields) == 0L) {
    return(rep(NA_character_, nrow(df)))
  }

  pieces <- lapply(fields, function(nm) {
    paste0(nm, "=", normalize_field(df[[nm]]))
  })

  do.call(paste, c(pieces, sep = "|"))
}

latest_file <- function(paths) {
  if (length(paths) == 0L) return(NA_character_)
  info <- file.info(paths)
  paths[[which.max(info$mtime)]]
}

# -----------------------------------------------------------------------------
# Read inputs
# -----------------------------------------------------------------------------

stop_missing(PC_ARCHIVE_FILE, "PC ONLIMO archive")
stop_missing(PC_STATE_FILE, "PC ONLIMO station state")
stop_missing(GH_STATE_FILE, "GitHub ONLIMO station state")
stop_missing(GH_MANIFEST_FILE, "GitHub ONLIMO snapshot manifest")

pc <- read_csv(PC_ARCHIVE_FILE, show_col_types = FALSE)
pc_state <- read_csv(PC_STATE_FILE, show_col_types = FALSE)
gh_state <- read_csv(GH_STATE_FILE, show_col_types = FALSE)
gh_manifest <- read_csv(GH_MANIFEST_FILE, show_col_types = FALSE)

required_observation_cols <- c("station_id", "date")
if (!all(required_observation_cols %in% names(pc))) {
  stop("PC archive lacks required columns: station_id and date", call. = FALSE)
}
if (!all(c("station_id", "last_archived_date") %in% names(pc_state))) {
  stop("PC station state lacks station_id/last_archived_date", call. = FALSE)
}
if (!all(c("station_id", "last_archived_date") %in% names(gh_state))) {
  stop("GitHub station state lacks station_id/last_archived_date", call. = FALSE)
}

snapshot_files <- list.files(
  GH_SNAPSHOT_DIR,
  pattern = "[.]csv[.]gz$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(snapshot_files) == 0L) {
  stop("No GitHub ONLIMO daily snapshots found under: ", GH_SNAPSHOT_DIR, call. = FALSE)
}

snapshot_rows <- lapply(snapshot_files, function(f) {
  x <- read_csv(f, show_col_types = FALSE)
  if (!all(required_observation_cols %in% names(x))) {
    stop("GitHub snapshot lacks station_id/date: ", f, call. = FALSE)
  }
  backup_root_norm <- normalizePath(
    TELITI_BACKUP_REPO,
    winslash = "/",
    mustWork = TRUE
  )
  snapshot_norm <- normalizePath(
    f,
    winslash = "/",
    mustWork = TRUE
  )
  backup_prefix <- paste0(backup_root_norm, "/")

  snapshot_relative <- if (startsWith(snapshot_norm, backup_prefix)) {
    substring(snapshot_norm, nchar(backup_prefix) + 1L)
  } else {
    snapshot_norm
  }

  x %>%
    mutate(
      .github_snapshot_file = snapshot_relative
    )
})

gh <- bind_rows(snapshot_rows)

# Explicit date normalization.
pc <- pc %>% mutate(date = as.Date(date))
gh <- gh %>% mutate(date = as.Date(date))
pc_state <- pc_state %>% mutate(last_archived_date = as.Date(last_archived_date))
gh_state <- gh_state %>% mutate(last_archived_date = as.Date(last_archived_date))

# -----------------------------------------------------------------------------
# Observation reconciliation
# -----------------------------------------------------------------------------

# Fields that represent scientific/derived ONLIMO content. Provenance fields
# (retrieved_at, collector_id, GitHub IDs, scraper commit) and recovery method
# are deliberately excluded because they are expected to differ by collector.
candidate_scientific_fields <- c(
  "station_name",
  "river",
  "watershed",
  "river_segment",
  "kabupaten_kota",
  "province",
  "latitude",
  "longitude",
  "ph",
  "do",
  "tds",
  "cod",
  "bod",
  "tss",
  "nitrate",
  "ammonia",
  "temperature",
  "index_ph",
  "index_do",
  "index_tds",
  "index_cod",
  "index_bod",
  "index_tss",
  "index_nitrate",
  "index_ammonia",
  "mean_parameter_index",
  "maximum_parameter_index",
  "critical_parameter",
  "pollution_index",
  "pollution_status",
  "status_colour",
  "location_category",
  "station_category"
)

scientific_fields <- intersect(
  candidate_scientific_fields,
  intersect(names(pc), names(gh))
)

if (length(scientific_fields) == 0L) {
  stop("No common ONLIMO scientific fields were found between PC and GitHub data.", call. = FALSE)
}

pc_current <- pc %>%
  arrange(station_id, date) %>%
  group_by(station_id, date) %>%
  slice_tail(n = 1L) %>%
  ungroup() %>%
  mutate(
    pc_payload_signature = make_payload_signature(pick(everything()), scientific_fields)
  ) %>%
  select(station_id, date, pc_payload_signature, everything())

# A station-date can occur in more than one retained GitHub snapshot. Preserve
# each distinct scientific version, while avoiding duplicate copies of the same
# payload from repeated snapshots.
gh_versions <- gh %>%
  mutate(
    github_payload_signature = make_payload_signature(pick(everything()), scientific_fields)
  ) %>%
  distinct(
    station_id,
    date,
    github_payload_signature,
    .keep_all = TRUE
  )

pc_lookup <- pc_current %>%
  select(station_id, date, pc_payload_signature)

obs_recon <- gh_versions %>%
  left_join(pc_lookup, by = c("station_id", "date")) %>%
  mutate(
    reconciliation_class = case_when(
      is.na(pc_payload_signature) ~ "github_only_observation_key",
      github_payload_signature == pc_payload_signature ~ "confirmed_exact_scientific_payload",
      TRUE ~ "same_key_different_scientific_payload"
    )
  )

obs_class_summary <- obs_recon %>%
  count(reconciliation_class, name = "rows") %>%
  mutate(pct = round(100 * rows / sum(rows), 4)) %>%
  arrange(desc(rows))

unique_gh_keys <- gh_versions %>% distinct(station_id, date) %>% nrow()
confirmed_keys <- obs_recon %>%
  filter(reconciliation_class != "github_only_observation_key") %>%
  distinct(station_id, date) %>%
  nrow()
exact_keys <- obs_recon %>%
  filter(reconciliation_class == "confirmed_exact_scientific_payload") %>%
  distinct(station_id, date) %>%
  nrow()

github_only_keys <- obs_recon %>%
  filter(reconciliation_class == "github_only_observation_key") %>%
  distinct(station_id, date) %>%
  nrow()

same_key_different <- obs_recon %>%
  filter(reconciliation_class == "same_key_different_scientific_payload") %>%
  distinct(station_id, date) %>%
  nrow()

# -----------------------------------------------------------------------------
# Station-state reconciliation
# -----------------------------------------------------------------------------

state_recon <- full_join(
  pc_state %>% rename(pc_last_archived_date = last_archived_date),
  gh_state %>% rename(github_last_archived_date = last_archived_date),
  by = "station_id"
) %>%
  mutate(
    state_class = case_when(
      is.na(pc_last_archived_date) ~ "github_only_station",
      is.na(github_last_archived_date) ~ "pc_only_station",
      pc_last_archived_date == github_last_archived_date ~ "same_last_archived_date",
      pc_last_archived_date > github_last_archived_date ~ "pc_ahead",
      pc_last_archived_date < github_last_archived_date ~ "github_ahead",
      TRUE ~ "unclassified"
    ),
    day_difference_pc_minus_github = as.integer(
      pc_last_archived_date - github_last_archived_date
    )
  ) %>%
  arrange(state_class, station_id)

state_summary <- state_recon %>%
  count(state_class, name = "stations") %>%
  mutate(pct = round(100 * stations / sum(stations), 4)) %>%
  arrange(desc(stations))

# -----------------------------------------------------------------------------
# Station-catalogue coverage
# -----------------------------------------------------------------------------

catalog_summary <- tibble(
  metric = character(),
  value = numeric()
)

catalog_recon <- tibble()

catalog_files <- list.files(
  GH_CATALOG_DIR,
  pattern = "[.]csv$",
  full.names = TRUE
)

if (file.exists(PC_CATALOG_FILE) && length(catalog_files) > 0L) {
  gh_catalog_file <- latest_file(catalog_files)
  pc_catalog <- read_csv(PC_CATALOG_FILE, show_col_types = FALSE)
  gh_catalog <- read_csv(gh_catalog_file, show_col_types = FALSE)

  if ("station_id" %in% names(pc_catalog) && "station_id" %in% names(gh_catalog)) {
    catalog_recon <- full_join(
      pc_catalog %>% distinct(station_id) %>% mutate(in_pc_catalog = TRUE),
      gh_catalog %>% distinct(station_id) %>% mutate(in_github_catalog = TRUE),
      by = "station_id"
    ) %>%
      mutate(
        in_pc_catalog = coalesce(in_pc_catalog, FALSE),
        in_github_catalog = coalesce(in_github_catalog, FALSE),
        catalog_class = case_when(
          in_pc_catalog & in_github_catalog ~ "confirmed_both",
          in_pc_catalog ~ "pc_only",
          in_github_catalog ~ "github_only",
          TRUE ~ "unclassified"
        )
      ) %>%
      arrange(catalog_class, station_id)

    catalog_summary <- catalog_recon %>%
      count(catalog_class, name = "stations") %>%
      mutate(pct = round(100 * stations / sum(stations), 4))
  }
}

# -----------------------------------------------------------------------------
# Summary and outputs
# -----------------------------------------------------------------------------

manifest_min_date <- if ("min_observation_date" %in% names(gh_manifest)) {
  suppressWarnings(min(as.Date(gh_manifest$min_observation_date), na.rm = TRUE))
} else {
  suppressWarnings(min(gh_versions$date, na.rm = TRUE))
}

manifest_max_date <- if ("max_observation_date" %in% names(gh_manifest)) {
  suppressWarnings(max(as.Date(gh_manifest$max_observation_date), na.rm = TRUE))
} else {
  suppressWarnings(max(gh_versions$date, na.rm = TRUE))
}

summary_tbl <- tibble(
  metric = c(
    "pc_archive_rows",
    "pc_archive_unique_station_dates",
    "github_snapshot_files",
    "github_snapshot_manifest_rows",
    "github_snapshot_rows_total",
    "github_unique_scientific_versions",
    "github_unique_observation_keys",
    "github_keys_present_in_pc",
    "github_key_recovery_pct",
    "github_keys_exact_scientific_payload_in_pc",
    "github_exact_scientific_payload_pct",
    "github_keys_same_key_different_scientific_payload",
    "github_only_observation_keys",
    "pc_station_state_rows",
    "github_station_state_rows",
    "scientific_fields_compared"
  ),
  value = c(
    nrow(pc),
    nrow(distinct(pc, station_id, date)),
    length(snapshot_files),
    nrow(gh_manifest),
    nrow(gh),
    nrow(gh_versions),
    unique_gh_keys,
    confirmed_keys,
    if (unique_gh_keys > 0) round(100 * confirmed_keys / unique_gh_keys, 4) else NA_real_,
    exact_keys,
    if (unique_gh_keys > 0) round(100 * exact_keys / unique_gh_keys, 4) else NA_real_,
    same_key_different,
    github_only_keys,
    nrow(pc_state),
    nrow(gh_state),
    length(scientific_fields)
  )
)

write_csv(
  summary_tbl,
  file.path(OUTPUT_DIR, "onlimo_daily_reconciliation_summary.csv"),
  na = ""
)
write_csv(
  obs_class_summary,
  file.path(OUTPUT_DIR, "onlimo_daily_observation_class_summary.csv"),
  na = ""
)
write_csv(
  obs_recon %>%
    select(
      station_id,
      date,
      reconciliation_class,
      .github_snapshot_file,
      github_payload_signature,
      pc_payload_signature
    ),
  file.path(OUTPUT_DIR, "onlimo_daily_observation_reconciliation.csv.gz"),
  na = ""
)
write_csv(
  obs_recon %>%
    filter(reconciliation_class != "confirmed_exact_scientific_payload") %>%
    select(
      station_id,
      date,
      reconciliation_class,
      .github_snapshot_file,
      any_of(c("station_name", "river", "watershed", "province")),
      github_payload_signature,
      pc_payload_signature
    ),
  file.path(OUTPUT_DIR, "onlimo_daily_observation_exceptions.csv"),
  na = ""
)
write_csv(
  state_recon,
  file.path(OUTPUT_DIR, "onlimo_daily_station_state_reconciliation.csv"),
  na = ""
)
write_csv(
  state_summary,
  file.path(OUTPUT_DIR, "onlimo_daily_station_state_summary.csv"),
  na = ""
)

if (nrow(catalog_recon) > 0L) {
  write_csv(
    catalog_recon,
    file.path(OUTPUT_DIR, "onlimo_daily_station_catalog_reconciliation.csv"),
    na = ""
  )
  write_csv(
    catalog_summary,
    file.path(OUTPUT_DIR, "onlimo_daily_station_catalog_summary.csv"),
    na = ""
  )
}

report_file <- file.path(OUTPUT_DIR, "onlimo_daily_reconciliation.md")

report_lines <- c(
  "# ONLIMO Daily PC-GitHub Reconciliation",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Scope",
  "",
  paste0("- PC archive rows: ", nrow(pc)),
  paste0("- GitHub retained snapshot files: ", length(snapshot_files)),
  paste0("- GitHub retained snapshot rows: ", nrow(gh)),
  paste0("- GitHub observation-date range represented: ", manifest_min_date, " to ", manifest_max_date),
  paste0("- Scientific fields compared: ", length(scientific_fields)),
  "",
  "## Observation-key recovery",
  "",
  paste0("- Unique GitHub station-date keys: ", unique_gh_keys),
  paste0("- Keys represented in PC archive: ", confirmed_keys),
  paste0(
    "- Observation-key recovery: ",
    if (unique_gh_keys > 0) round(100 * confirmed_keys / unique_gh_keys, 4) else NA,
    "%"
  ),
  paste0("- GitHub-only observation keys: ", github_only_keys),
  "",
  "## Scientific-payload agreement",
  "",
  paste0("- Exact scientific payload keys: ", exact_keys),
  paste0(
    "- Exact scientific-payload agreement: ",
    if (unique_gh_keys > 0) round(100 * exact_keys / unique_gh_keys, 4) else NA,
    "%"
  ),
  paste0("- Same key but different scientific payload: ", same_key_different),
  "",
  "## Interpretation",
  "",
  "The comparison is directional: retained GitHub station-date observations are checked against the current Windows cumulative archive. A same-key/different-payload result can reflect an upstream ONLIMO revision observed later by the PC, not necessarily a collector error.",
  "",
  "Station-state reconciliation should be used alongside row comparison because the two collectors can run at different times of day.",
  ""
)

writeLines(report_lines, report_file, useBytes = TRUE)

cat("\nONLIMO daily reconciliation complete.\n")
cat("GitHub retained snapshot files:", length(snapshot_files), "\n")
cat("GitHub retained snapshot rows:", nrow(gh), "\n")
cat("GitHub unique observation keys:", unique_gh_keys, "\n")
cat(
  "GitHub observation keys present in PC archive:",
  confirmed_keys,
  sprintf("(%.2f%%)", if (unique_gh_keys > 0) 100 * confirmed_keys / unique_gh_keys else NA_real_),
  "\n"
)
cat(
  "Exact scientific payloads represented in PC archive:",
  exact_keys,
  sprintf("(%.2f%%)", if (unique_gh_keys > 0) 100 * exact_keys / unique_gh_keys else NA_real_),
  "\n"
)
cat("Same key, different scientific payload:", same_key_different, "\n")
cat("GitHub-only observation keys:", github_only_keys, "\n")
cat("\nStation-state classes:\n")
print(state_summary, n = Inf)
if (nrow(catalog_summary) > 0L) {
  cat("\nStation-catalog classes:\n")
  print(catalog_summary, n = Inf)
}
cat("\nScientific fields compared:\n")
cat(paste0("  - ", scientific_fields, collapse = "\n"), "\n")
cat("\nOutputs:\n")
cat("  ", OUTPUT_DIR, "\n")