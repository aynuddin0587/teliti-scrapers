# =============================================================================
# 14_plot_data_inventory_overview.R
#
# Scientific inventory dashboard for the canonical water-quality datasets.
#
# This complements 09_plot_collection_progress.R:
#   09 = collection / validation / operational health
#   14 = scientific data holdings: what, how much, where, when, parameters
#
# Required upstream step:
#   Rscript teliti_reconciliation/script/12_build_analysis_datasets.R
#
# Main outputs
# ------------
# teliti_reconciliation/output/data_inventory/
#   water_quality_data_inventory_overview.png
#   water_quality_data_inventory_overview.pdf
#   parameter_completeness_matrix.png
#   panel_dataset_volume.png
#   panel_spatial_coverage.png
#   panel_temporal_coverage.png
#   panel_parameter_availability.png
#   data_inventory_summary.csv
#   data_parameter_inventory.csv
#   data_station_coverage.csv
#   data_temporal_coverage.csv
# =============================================================================

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------
PROJECT_DIR <- "D:/# R Project/penelitian"
ANALYSIS_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation", "analysis")
OUTPUT_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation", "output", "data_inventory")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUTPUT_WIDTH <- 16
OUTPUT_HEIGHT <- 12
OUTPUT_DPI <- 320
SAVE_PDF <- TRUE
SAVE_PANELS <- TRUE

DATASET_ORDER <- c(
  "Fujian weekly",
  "NMEMC marine",
  "CNEMC surface water",
  "ONLIMO daily",
  "ONLIMO historical IP"
)

DATASET_COLORS <- c(
  "Fujian weekly" = "#009E73",
  "NMEMC marine" = "#0072B2",
  "CNEMC surface water" = "#D55E00",
  "ONLIMO daily" = "#CC79A7",
  "ONLIMO historical IP" = "#E69F00",
  "CNEMC revision history" = "#6B7280"
)

COLORS <- list(
  text = "#1F2937",
  muted = "#6B7280",
  title = "#153E75",
  grid = "#E5E7EB",
  background = "white",
  missing = "#F3F4F6",
  measured = "#2A6FBB",
  status = "#E69F00",
  coordinate = "#0072B2",
  no_coordinate = "#D1D5DB"
)

# Canonical scientific products. CNEMC revision history is loaded separately
# as provenance/version information, not treated as independent observations.
DATASET_FILES <- c(
  "Fujian weekly" = file.path(ANALYSIS_DIR, "fujian_weekly_analysis.rds"),
  "NMEMC marine" = file.path(ANALYSIS_DIR, "nmemc_marine_analysis.rds"),
  "CNEMC surface water" = file.path(ANALYSIS_DIR, "cnemc_latest_analysis.rds"),
  "ONLIMO daily" = file.path(ANALYSIS_DIR, "onlimo_daily_analysis.rds"),
  "ONLIMO historical IP" = file.path(ANALYSIS_DIR, "onlimo_historical_analysis.rds")
)

CNEMC_HISTORY_FILE <- file.path(ANALYSIS_DIR, "cnemc_revision_history.rds")
STATION_METADATA_FILE <- file.path(ANALYSIS_DIR, "station_metadata.rds")
DATASET_INVENTORY_FILE <- file.path(ANALYSIS_DIR, "dataset_inventory.csv")

# Descriptive metadata for interpretation. These labels do not change data.
DATASET_META <- data.frame(
  dataset = DATASET_ORDER,
  environmental_domain = c(
    "River / inland surface water",
    "Coastal / marine water",
    "Automatic river / surface water",
    "River monitoring",
    "River Pollution Index"
  ),
  geographic_scope = c(
    "Fujian Province, China",
    "China coastal waters",
    "China national network",
    "Indonesia active ONLIMO network",
    "Selected ONLIMO historical watersheds"
  ),
  native_resolution = c(
    "Weekly",
    "Monthly / periodic",
    "High-frequency monitoring",
    "Daily",
    "Daily"
  ),
  stringsAsFactors = FALSE
)

# Native parameter dictionary -------------------------------------------------
# The concept labels intentionally preserve distinctions such as CODMn vs COD
# and inorganic N vs total N. A filled tile means that the native field exists;
# it does NOT imply that similarly named fields are analytically interchangeable.
PARAMETER_DICTIONARY <- data.frame(
  concept = c(
    "pH", "DO", "Temperature", "Conductivity", "Turbidity", "TDS", "TSS",
    "CODMn", "COD", "BOD", "NH3-N", "Nitrate", "Total N", "Inorganic N",
    "Total P", "Reactive PO4", "Petroleum", "TOC", "Chlorophyll-a",
    "Algal density", "WQ class*", "Pollution Index*"
  ),
  parameter_group = c(
    "General", "Oxygen", "General", "Ionic", "Solids", "Solids", "Solids",
    "Organic matter", "Organic matter", "Organic matter", "Nutrients",
    "Nutrients", "Nutrients", "Nutrients", "Nutrients", "Nutrients",
    "Hydrocarbon", "Organic matter", "Ecology", "Ecology", "Status/index",
    "Status/index"
  ),
  parameter_type = c(rep("measured_or_published_parameter", 20), rep("status_or_index", 2)),
  stringsAsFactors = FALSE
)

# Field candidates for each dataset and concept. The first existing field is
# used for completeness calculations. This is source-native, not harmonized.
PARAMETER_FIELDS <- list(
  "Fujian weekly" = list(
    "pH" = c("ph_raw"),
    "DO" = c("dissolved_oxygen_mg_l_raw"),
    "CODMn" = c("permanganate_index_mg_l_raw"),
    "NH3-N" = c("ammonia_nitrogen_mg_l_raw"),
    "Total N" = c("total_nitrogen_mg_l_raw"),
    "Total P" = c("total_phosphorus_mg_l_raw"),
    "WQ class*" = c("current_week_water_quality", "water_quality_class")
  ),
  "NMEMC marine" = list(
    "pH" = c("ph_raw"),
    "DO" = c("dissolved_oxygen_mg_l_raw"),
    "COD" = c("cod_mg_l_raw"),
    "Inorganic N" = c("inorganic_nitrogen_mg_l_raw"),
    "Reactive PO4" = c("reactive_phosphate_mg_l_raw"),
    "Petroleum" = c("petroleum_mg_l_raw"),
    "WQ class*" = c("water_quality_class")
  ),
  "CNEMC surface water" = list(
    "pH" = c("ph_raw"),
    "DO" = c("dissolved_oxygen_mg_l_raw"),
    "Temperature" = c("water_temperature_c_raw"),
    "Conductivity" = c("conductivity_raw"),
    "Turbidity" = c("turbidity_ntu_raw"),
    "CODMn" = c("permanganate_index_mg_l_raw"),
    "NH3-N" = c("ammonia_nitrogen_mg_l_raw"),
    "Total N" = c("total_nitrogen_mg_l_raw"),
    "Total P" = c("total_phosphorus_mg_l_raw"),
    "TOC" = c("toc_mg_l_raw"),
    "Chlorophyll-a" = c("chlorophyll_a_raw"),
    "Algal density" = c("algal_density_raw"),
    "WQ class*" = c("water_quality_class_code", "water_quality_class")
  ),
  "ONLIMO daily" = list(
    "pH" = c("ph"),
    "DO" = c("do", "dissolved_oxygen_mg_l"),
    "Temperature" = c("temperature"),
    "TDS" = c("tds"),
    "TSS" = c("tss"),
    "COD" = c("cod"),
    "BOD" = c("bod"),
    "NH3-N" = c("ammonia"),
    "Nitrate" = c("nitrate"),
    "Pollution Index*" = c("pollution_index")
  ),
  "ONLIMO historical IP" = list(
    "Pollution Index*" = c("pollution_index")
  )
)

# -----------------------------------------------------------------------------
# 2. Packages
# -----------------------------------------------------------------------------
required_packages <- c("dplyr", "tidyr", "readr", "ggplot2", "patchwork", "scales", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them with: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(tibble)
})

# -----------------------------------------------------------------------------
# 3. Helpers
# -----------------------------------------------------------------------------
log_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
}

safe_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), error = function(e) NULL)
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0L) return(NA_character_)
  hit[[1]]
}

nonmissing_mask <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(!is.na(x))
  if (is.numeric(x) || is.integer(x)) return(!is.na(x) & is.finite(as.numeric(x)))
  if (is.logical(x)) return(!is.na(x))

  ch <- trimws(as.character(x))
  !is.na(ch) & nzchar(ch) & !(tolower(ch) %in% c("na", "n/a", "nan", "null", "-", "--", "---", "—"))
}

parse_date_flexible <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  ch <- trimws(as.character(x))
  ch[ch == ""] <- NA_character_
  out <- rep(as.Date(NA), length(ch))

  formats <- c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d")
  for (fmt in formats) {
    idx <- is.na(out) & !is.na(ch)
    if (!any(idx)) break
    parsed <- suppressWarnings(as.Date(ch[idx], format = fmt))
    out[idx] <- parsed
  }

  # Datetime-like strings: first ten characters often carry YYYY-MM-DD.
  idx <- is.na(out) & !is.na(ch) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", ch)
  if (any(idx)) out[idx] <- suppressWarnings(as.Date(substr(ch[idx], 1, 10), format = "%Y-%m-%d"))

  # Chinese year-month-day strings.
  idx <- is.na(out) & !is.na(ch) & grepl("^[0-9]{4}年[0-9]{1,2}月[0-9]{1,2}日", ch)
  if (any(idx)) {
    normalized <- gsub("年|月", "-", ch[idx])
    normalized <- gsub("日.*$", "", normalized)
    parts <- strsplit(normalized, "-", fixed = TRUE)
    normalized <- vapply(parts, function(z) {
      if (length(z) < 3L) return(NA_character_)
      sprintf("%04d-%02d-%02d", as.integer(z[[1]]), as.integer(z[[2]]), as.integer(z[[3]]))
    }, character(1))
    out[idx] <- suppressWarnings(as.Date(normalized, format = "%Y-%m-%d"))
  }

  out
}

min_date_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(as.Date(NA))
  min(x)
}

max_date_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(as.Date(NA))
  max(x)
}

valid_lonlat <- function(lon, lat) {
  lon <- suppressWarnings(as.numeric(lon))
  lat <- suppressWarnings(as.numeric(lat))
  !is.na(lon) & !is.na(lat) & is.finite(lon) & is.finite(lat) &
    lon >= -180 & lon <= 180 & lat >= -90 & lat <= 90
}

base_theme <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3, colour = COLORS$title),
      plot.subtitle = element_text(size = base_size - 1, colour = COLORS$muted),
      plot.caption = element_text(size = base_size - 2, colour = COLORS$muted),
      axis.title = element_text(face = "bold", colour = COLORS$text),
      axis.text = element_text(colour = COLORS$text),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = COLORS$grid, linewidth = 0.3),
      strip.text = element_text(face = "bold", colour = COLORS$text),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

save_panel <- function(plot_obj, filename, width = 8, height = 5) {
  ggsave(
    file.path(OUTPUT_DIR, filename), plot_obj,
    width = width, height = height, dpi = OUTPUT_DPI,
    bg = COLORS$background
  )
}

# -----------------------------------------------------------------------------
# 4. Load canonical datasets
# -----------------------------------------------------------------------------
missing_files <- DATASET_FILES[!file.exists(DATASET_FILES)]
if (length(missing_files) > 0L) {
  stop(
    "Canonical analysis dataset(s) missing:\n",
    paste(" -", missing_files, collapse = "\n"),
    "\nRun teliti_reconciliation/script/12_build_analysis_datasets.R first.",
    call. = FALSE
  )
}

log_msg("Reading canonical scientific datasets ...")
data_list <- lapply(DATASET_FILES, readRDS)
cnemc_history <- safe_read_rds(CNEMC_HISTORY_FILE)
canonical_inventory <- safe_read_csv(DATASET_INVENTORY_FILE)

# -----------------------------------------------------------------------------
# 5. Dataset-specific coverage helpers
# -----------------------------------------------------------------------------
get_station_key <- function(dataset, df) {
  n <- nrow(df)
  if (n == 0L) return(character())

  if (dataset == "Fujian weekly") {
    nm <- first_existing_col(df, c("station_name", "station_name_raw"))
    return(if (!is.na(nm)) as.character(df[[nm]]) else rep(NA_character_, n))
  }

  if (dataset == "NMEMC marine") {
    nm <- first_existing_col(df, c("site_code", "site", "station_name"))
    return(if (!is.na(nm)) as.character(df[[nm]]) else rep(NA_character_, n))
  }

  if (dataset == "CNEMC surface water") {
    section_col <- first_existing_col(df, c("monitoring_section", "station_name"))
    area_col <- first_existing_col(df, c("area", "province"))
    if (!is.na(section_col)) {
      section <- as.character(df[[section_col]])
      if (!is.na(area_col)) return(paste(as.character(df[[area_col]]), section, sep = " | "))
      return(section)
    }
    return(rep(NA_character_, n))
  }

  if (dataset %in% c("ONLIMO daily", "ONLIMO historical IP")) {
    nm <- first_existing_col(df, c("station_id", "station_name"))
    return(if (!is.na(nm)) as.character(df[[nm]]) else rep(NA_character_, n))
  }

  rep(NA_character_, n)
}

get_coord_columns <- function(dataset, df) {
  if (dataset == "Fujian weekly") {
    return(c(
      first_existing_col(df, c("station_longitude", "longitude", "longitude_final")),
      first_existing_col(df, c("station_latitude", "latitude", "latitude_final"))
    ))
  }

  c(
    first_existing_col(df, c("longitude", "station_longitude", "lon", "lng")),
    first_existing_col(df, c("latitude", "station_latitude", "lat"))
  )
}

get_date_range <- function(dataset, df) {
  # Prefer true parsed date/datetime fields.
  candidates <- switch(
    dataset,
    "Fujian weekly" = c("report_period_end", "report_period_start"),
    "NMEMC marine" = c("monitor_date", "sample_date", "date", "monitor_time_raw"),
    "CNEMC surface water" = c("observation_datetime", "monitoring_datetime", "collected_at"),
    "ONLIMO daily" = c("date"),
    "ONLIMO historical IP" = c("date"),
    character()
  )

  for (nm in candidates) {
    if (!nm %in% names(df)) next
    d <- parse_date_flexible(df[[nm]])
    d <- d[!is.na(d)]
    if (length(d) > 0L) return(c(min(d), max(d)))
  }

  # NMEMC canonical rows always carry source_year even if the native monitoring
  # month string cannot be parsed consistently. Use year bounds conservatively.
  year_candidates <- c("source_year", "year")
  for (nm in year_candidates) {
    if (!nm %in% names(df)) next
    yr <- suppressWarnings(as.integer(df[[nm]]))
    yr <- yr[is.finite(yr) & yr >= 1900 & yr <= 2200]
    if (length(yr) > 0L) {
      return(c(
        as.Date(sprintf("%04d-01-01", min(yr)), format = "%Y-%m-%d"),
        as.Date(sprintf("%04d-12-31", max(yr)), format = "%Y-%m-%d")
      ))
    }
  }

  c(as.Date(NA), as.Date(NA))
}

get_spatial_group_count <- function(dataset, df) {
  candidates <- switch(
    dataset,
    "Fujian weekly" = c("river_system", "river_system_raw"),
    "NMEMC marine" = c("sea", "province", "city"),
    "CNEMC surface water" = c("river_basin", "area"),
    "ONLIMO daily" = c("watershed", "province"),
    "ONLIMO historical IP" = c("watershed", "province"),
    character()
  )
  nm <- first_existing_col(df, candidates)
  if (is.na(nm)) return(NA_integer_)
  x <- trimws(as.character(df[[nm]]))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) return(NA_integer_)
  dplyr::n_distinct(x)
}

# -----------------------------------------------------------------------------
# 6. Dataset summary: scale + temporal + spatial coverage
# -----------------------------------------------------------------------------
summary_rows <- vector("list", length(DATASET_ORDER))

for (i in seq_along(DATASET_ORDER)) {
  dataset <- DATASET_ORDER[[i]]
  df <- data_list[[dataset]]

  station_key <- get_station_key(dataset, df)
  station_valid <- !is.na(station_key) & nzchar(trimws(station_key))
  station_n <- n_distinct(station_key[station_valid])

  coord_cols <- get_coord_columns(dataset, df)
  coordinate_station_n <- 0L
  lon_min <- lon_max <- lat_min <- lat_max <- NA_real_

  if (length(coord_cols) == 2L && all(!is.na(coord_cols)) && all(coord_cols %in% names(df))) {
    lon <- suppressWarnings(as.numeric(df[[coord_cols[[1]]]]))
    lat <- suppressWarnings(as.numeric(df[[coord_cols[[2]]]]))
    coord_ok <- valid_lonlat(lon, lat) & station_valid

    if (any(coord_ok)) {
      coordinate_station_n <- n_distinct(station_key[coord_ok])
      lon_min <- min(lon[coord_ok], na.rm = TRUE)
      lon_max <- max(lon[coord_ok], na.rm = TRUE)
      lat_min <- min(lat[coord_ok], na.rm = TRUE)
      lat_max <- max(lat[coord_ok], na.rm = TRUE)
    }
  }

  rng <- get_date_range(dataset, df)

  summary_rows[[i]] <- tibble(
    dataset = dataset,
    rows = nrow(df),
    columns = ncol(df),
    stations = station_n,
    stations_with_coordinates = coordinate_station_n,
    coordinate_completeness_pct = if (station_n > 0L) 100 * coordinate_station_n / station_n else NA_real_,
    spatial_groups = get_spatial_group_count(dataset, df),
    date_start = rng[[1]],
    date_end = rng[[2]],
    longitude_min = lon_min,
    longitude_max = lon_max,
    latitude_min = lat_min,
    latitude_max = lat_max
  )
}

dataset_summary <- bind_rows(summary_rows) %>%
  left_join(DATASET_META, by = "dataset") %>%
  mutate(dataset = factor(dataset, levels = DATASET_ORDER)) %>%
  arrange(dataset)

# CNEMC revision/provenance context ------------------------------------------
cnemc_revision_rows <- if (!is.null(cnemc_history)) nrow(cnemc_history) else NA_integer_
cnemc_revision_keys <- if (!is.null(cnemc_history) && "analysis_revision_count" %in% names(cnemc_history)) {
  if ("observation_key_hash" %in% names(cnemc_history)) {
    n_distinct(cnemc_history$observation_key_hash[cnemc_history$analysis_revision_count > 1L])
  } else {
    NA_integer_
  }
} else {
  NA_integer_
}

total_canonical_rows <- sum(dataset_summary$rows, na.rm = TRUE)
overall_start <- min_date_or_na(dataset_summary$date_start)
overall_end <- max_date_or_na(dataset_summary$date_end)

readr::write_csv(
  dataset_summary %>% mutate(dataset = as.character(dataset)),
  file.path(OUTPUT_DIR, "data_inventory_summary.csv"),
  na = ""
)

readr::write_csv(
  dataset_summary %>%
    select(dataset, date_start, date_end, native_resolution, rows),
  file.path(OUTPUT_DIR, "data_temporal_coverage.csv"),
  na = ""
)

readr::write_csv(
  dataset_summary %>%
    select(
      dataset, stations, stations_with_coordinates, coordinate_completeness_pct,
      spatial_groups, geographic_scope, longitude_min, longitude_max,
      latitude_min, latitude_max
    ),
  file.path(OUTPUT_DIR, "data_station_coverage.csv"),
  na = ""
)

# -----------------------------------------------------------------------------
# 7. Parameter availability + completeness
# -----------------------------------------------------------------------------
parameter_rows <- list()
k <- 0L

for (dataset in DATASET_ORDER) {
  df <- data_list[[dataset]]
  field_map <- PARAMETER_FIELDS[[dataset]]

  for (concept in PARAMETER_DICTIONARY$concept) {
    candidates <- field_map[[concept]]
    field <- if (is.null(candidates)) NA_character_ else first_existing_col(df, candidates)
    present <- !is.na(field)

    completeness <- NA_real_
    nonmissing_n <- 0L
    if (present) {
      mask <- nonmissing_mask(df[[field]])
      nonmissing_n <- sum(mask, na.rm = TRUE)
      completeness <- if (nrow(df) > 0L) 100 * nonmissing_n / nrow(df) else NA_real_
    }

    k <- k + 1L
    parameter_rows[[k]] <- tibble(
      dataset = dataset,
      concept = concept,
      source_field = if (present) field else NA_character_,
      field_present = present,
      nonmissing_n = nonmissing_n,
      completeness_pct = completeness
    )
  }
}

parameter_inventory <- bind_rows(parameter_rows) %>%
  left_join(PARAMETER_DICTIONARY, by = "concept") %>%
  mutate(
    dataset = factor(dataset, levels = DATASET_ORDER),
    concept = factor(concept, levels = PARAMETER_DICTIONARY$concept),
    availability_type = case_when(
      !field_present ~ "Not available",
      parameter_type == "status_or_index" ~ "Status / index",
      TRUE ~ "Measured / published parameter"
    )
  ) %>%
  arrange(dataset, concept)

readr::write_csv(
  parameter_inventory %>% mutate(dataset = as.character(dataset), concept = as.character(concept)),
  file.path(OUTPUT_DIR, "data_parameter_inventory.csv"),
  na = ""
)

# Parameter counts for subtitle/table context --------------------------------
parameter_counts <- parameter_inventory %>%
  filter(field_present) %>%
  group_by(dataset) %>%
  summarise(
    native_parameter_or_status_fields = n(),
    direct_parameter_fields = sum(parameter_type == "measured_or_published_parameter"),
    status_or_index_fields = sum(parameter_type == "status_or_index"),
    .groups = "drop"
  )

dataset_summary <- dataset_summary %>% left_join(parameter_counts, by = "dataset")

# -----------------------------------------------------------------------------
# 8. Panel A: observation volume
# -----------------------------------------------------------------------------
volume_plot_data <- dataset_summary %>%
  mutate(
    dataset = factor(dataset, levels = rev(DATASET_ORDER)),
    row_label = scales::comma(rows)
  )

p_volume <- ggplot(volume_plot_data, aes(x = rows, y = dataset, fill = dataset)) +
  geom_col(width = 0.68, show.legend = FALSE) +
  geom_text(
    aes(label = row_label),
    hjust = -0.08,
    size = 3.4,
    colour = COLORS$text
  ) +
  scale_fill_manual(values = DATASET_COLORS) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = ","),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "A. Canonical observation volume",
    subtitle = "Rows are source-native analytical records; they should not be pooled as one statistical sample.",
    x = "Canonical rows",
    y = NULL
  ) +
  base_theme() +
  theme(panel.grid.major.y = element_blank())

# -----------------------------------------------------------------------------
# 9. Panel B: spatial / station coverage
# -----------------------------------------------------------------------------
spatial_plot_data <- dataset_summary %>%
  transmute(
    dataset,
    `With coordinates` = stations_with_coordinates,
    `Coordinates not yet available` = pmax(stations - stations_with_coordinates, 0L),
    coordinate_completeness_pct
  ) %>%
  pivot_longer(
    cols = c(`With coordinates`, `Coordinates not yet available`),
    names_to = "coordinate_status",
    values_to = "station_n"
  ) %>%
  mutate(
    dataset = factor(dataset, levels = rev(DATASET_ORDER)),
    coordinate_status = factor(
      coordinate_status,
      levels = c("With coordinates", "Coordinates not yet available")
    )
  )

coord_labels <- dataset_summary %>%
  mutate(
    dataset = factor(dataset, levels = rev(DATASET_ORDER)),
    label = paste0(
      scales::comma(stations), " stations | ",
      ifelse(is.na(coordinate_completeness_pct), "NA", sprintf("%.0f%% mapped", coordinate_completeness_pct))
    )
  )

p_spatial <- ggplot(spatial_plot_data, aes(x = station_n, y = dataset, fill = coordinate_status)) +
  geom_col(width = 0.68) +
  geom_text(
    data = coord_labels,
    aes(x = stations, y = dataset, label = label),
    inherit.aes = FALSE,
    hjust = -0.06,
    size = 3.1,
    colour = COLORS$text
  ) +
  scale_fill_manual(values = c(
    "With coordinates" = COLORS$coordinate,
    "Coordinates not yet available" = COLORS$no_coordinate
  )) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = ","),
    expand = expansion(mult = c(0, 0.35))
  ) +
  labs(
    title = "B. Station and coordinate coverage",
    subtitle = "Station counts use source-native station identifiers; mapping completeness is shown separately.",
    x = "Distinct monitoring stations / sections",
    y = NULL,
    fill = NULL
  ) +
  base_theme() +
  theme(panel.grid.major.y = element_blank())

# -----------------------------------------------------------------------------
# 10. Panel C: temporal coverage
# -----------------------------------------------------------------------------
temporal_plot_data <- dataset_summary %>%
  filter(!is.na(date_start), !is.na(date_end)) %>%
  mutate(dataset = factor(dataset, levels = rev(DATASET_ORDER)))

p_temporal <- ggplot(temporal_plot_data, aes(y = dataset, colour = dataset)) +
  geom_segment(
    aes(x = date_start, xend = date_end, yend = dataset),
    linewidth = 4.2,
    lineend = "round",
    alpha = 0.85,
    show.legend = FALSE
  ) +
  geom_point(aes(x = date_start), size = 2.6, show.legend = FALSE) +
  geom_point(aes(x = date_end), size = 2.6, show.legend = FALSE) +
  scale_colour_manual(values = DATASET_COLORS) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.015, 0.03))
  ) +
  labs(
    title = "C. Temporal coverage",
    subtitle = "First-to-last canonical record at each dataset's native temporal resolution.",
    x = NULL,
    y = NULL
  ) +
  base_theme() +
  theme(panel.grid.major.y = element_blank())

# -----------------------------------------------------------------------------
# 11. Panel D: parameter availability matrix
# -----------------------------------------------------------------------------
param_plot_data <- parameter_inventory %>%
  mutate(
    display = case_when(
      availability_type == "Measured / published parameter" ~ "Parameter",
      availability_type == "Status / index" ~ "Status/index",
      TRUE ~ "Absent"
    )
  )

p_parameters <- ggplot(param_plot_data, aes(x = concept, y = dataset, fill = display)) +
  geom_tile(colour = "white", linewidth = 0.7, width = 0.94, height = 0.82) +
  scale_fill_manual(values = c(
    "Parameter" = COLORS$measured,
    "Status/index" = COLORS$status,
    "Absent" = COLORS$missing
  )) +
  labs(
    title = "D. Native parameter availability",
    subtitle = "Presence of source-native fields. Similar names across datasets are not automatically equivalent measurements.",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "* WQ class and Pollution Index are status/index fields rather than direct concentration measurements."
  ) +
  base_theme(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 48, hjust = 1, vjust = 1, size = 8.5),
    axis.text.y = element_text(size = 9.5),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# -----------------------------------------------------------------------------
# 12. Detailed parameter completeness matrix
# -----------------------------------------------------------------------------
completeness_plot_data <- parameter_inventory %>%
  mutate(
    completeness_for_fill = ifelse(field_present, completeness_pct, NA_real_),
    completeness_label = ifelse(
      field_present,
      ifelse(is.na(completeness_pct), "?", paste0(round(completeness_pct), "%")),
      ""
    )
  )

p_parameter_completeness <- ggplot(
  completeness_plot_data,
  aes(x = concept, y = dataset, fill = completeness_for_fill)
) +
  geom_tile(colour = "white", linewidth = 0.8, width = 0.94, height = 0.82) +
  geom_text(aes(label = completeness_label), size = 2.7, colour = COLORS$text) +
  scale_fill_gradient(
    low = "#DCEAF7",
    high = "#08519C",
    limits = c(0, 100),
    na.value = COLORS$missing,
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Parameter completeness across canonical datasets",
    subtitle = "Percentage of canonical rows containing a published value for each native field; grey = field absent.",
    x = NULL,
    y = NULL,
    fill = "Rows with value",
    caption = paste(
      "Completeness describes availability, not analytical validity or cross-dataset equivalence.",
      "Qualified values such as '<0.01' count as published values."
    )
  ) +
  base_theme(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 48, hjust = 1, vjust = 1, size = 9),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# -----------------------------------------------------------------------------
# 13. Assemble overview
# -----------------------------------------------------------------------------
summary_subtitle <- paste0(
  "Five canonical scientific products | ",
  scales::comma(total_canonical_rows),
  " source-native analytical rows | overall coverage ",
  format(overall_start, "%Y"), "–", format(overall_end, "%Y")
)

revision_note <- if (!is.na(cnemc_revision_rows)) {
  paste0(
    "CNEMC provenance archive retains ", scales::comma(cnemc_revision_rows),
    " unique published row versions",
    if (!is.na(cnemc_revision_keys)) paste0(" across ", scales::comma(cnemc_revision_keys), " revised observation keys") else "",
    "."
  )
} else {
  "CNEMC revision history was not available when this figure was built."
}

p_overview <- ((p_volume | p_spatial) / p_temporal / p_parameters) +
  plot_layout(heights = c(1.05, 0.8, 1.35)) +
  plot_annotation(
    title = "Water-quality monitoring data inventory",
    subtitle = summary_subtitle,
    caption = paste(
      revision_note,
      "Canonical datasets are generated by 12_build_analysis_datasets.R; operational collector health remains in script 09."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 21, colour = COLORS$title),
      plot.subtitle = element_text(size = 11, colour = COLORS$muted),
      plot.caption = element_text(size = 9, colour = COLORS$muted)
    )
  )

# -----------------------------------------------------------------------------
# 14. Save outputs
# -----------------------------------------------------------------------------
log_msg("Writing scientific inventory dashboard ...")

ggsave(
  file.path(OUTPUT_DIR, "water_quality_data_inventory_overview.png"),
  p_overview,
  width = OUTPUT_WIDTH,
  height = OUTPUT_HEIGHT,
  dpi = OUTPUT_DPI,
  bg = COLORS$background
)

if (isTRUE(SAVE_PDF)) {
  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
  ggsave(
    file.path(OUTPUT_DIR, "water_quality_data_inventory_overview.pdf"),
    p_overview,
    width = OUTPUT_WIDTH,
    height = OUTPUT_HEIGHT,
    device = pdf_device,
    bg = COLORS$background
  )
}

ggsave(
  file.path(OUTPUT_DIR, "parameter_completeness_matrix.png"),
  p_parameter_completeness,
  width = 16,
  height = 5.8,
  dpi = OUTPUT_DPI,
  bg = COLORS$background
)

if (isTRUE(SAVE_PANELS)) {
  save_panel(p_volume, "panel_dataset_volume.png", 8.2, 5.0)
  save_panel(p_spatial, "panel_spatial_coverage.png", 8.2, 5.0)
  save_panel(p_temporal, "panel_temporal_coverage.png", 13.0, 4.8)
  save_panel(p_parameters, "panel_parameter_availability.png", 16.0, 5.4)
}

# Re-write summary after parameter counts were attached.
readr::write_csv(
  dataset_summary %>% mutate(dataset = as.character(dataset)),
  file.path(OUTPUT_DIR, "data_inventory_summary.csv"),
  na = ""
)

# -----------------------------------------------------------------------------
# 15. Console summary
# -----------------------------------------------------------------------------
log_msg("Scientific data inventory complete.")
message("")
message("Output directory: ", OUTPUT_DIR)
message("Main figure:      ", file.path(OUTPUT_DIR, "water_quality_data_inventory_overview.png"))
message("Parameter detail: ", file.path(OUTPUT_DIR, "parameter_completeness_matrix.png"))
message("")
message("Canonical scientific products:")
for (i in seq_len(nrow(dataset_summary))) {
  z <- dataset_summary[i, ]
  message(
    "  ", as.character(z$dataset), ": ",
    scales::comma(z$rows), " rows | ",
    scales::comma(z$stations), " stations/sections | ",
    ifelse(is.na(z$coordinate_completeness_pct), "NA", sprintf("%.0f%% mapped", z$coordinate_completeness_pct)),
    " | ", as.character(z$date_start), " to ", as.character(z$date_end),
    " | ", ifelse(is.na(z$native_parameter_or_status_fields), 0, z$native_parameter_or_status_fields), " parameter/status fields"
  )
}
message("")
message("Total canonical rows across five products: ", scales::comma(total_canonical_rows))
if (!is.na(cnemc_revision_rows)) {
  message("CNEMC retained row versions: ", scales::comma(cnemc_revision_rows))
}

invisible(list(
  dataset_summary = dataset_summary,
  parameter_inventory = parameter_inventory,
  plots = list(
    overview = p_overview,
    volume = p_volume,
    spatial = p_spatial,
    temporal = p_temporal,
    parameter_availability = p_parameters,
    parameter_completeness = p_parameter_completeness
  )
))