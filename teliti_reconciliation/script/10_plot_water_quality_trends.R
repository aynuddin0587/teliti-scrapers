# ============================================================
# Teliti environmental-trend dashboard
# Revision: 2026-08-19 v3 - bounded ONLIMO focus plots, robust palettes, and export-safe dimensions
#
# Purpose
#   Build static, presentation-ready figures from the environmental
#   data already collected for Teliti. The script is designed for
#   repeated monitoring runs and as a starting point for manuscript
#   exploration.
#
# Main figures
#   1) teliti_environmental_trends_overview.png / .pdf
#      - Fujian long-term water-quality class trend
#      - Fujian long-term parameter trends
#      - ONLIMO historical Pollution Index trend by watershed
#      - Fujian marine water-quality-class trend from NMEMC
#
#   2) teliti_recent_monitoring_trends.png / .pdf
#      - Recent CNEMC Fujian parameter dynamics
#      - Recent ONLIMO daily Pollution Index dynamics
#
# Data tables written for later manuscript use
#   - fujian_wq_class_annual.csv
#   - fujian_parameter_annual.csv
#   - onlimo_ip_monthly.csv
#   - nmemc_fujian_class_annual.csv
#   - cnemc_fujian_recent.csv
#   - onlimo_daily_recent.csv
#   - data_readiness_summary.csv
#   - city_comparison_readiness.csv
#   - trend_direction_summary.csv
#
# Usage
#   Rscript "D:/# R Project/penelitian/teliti_reconciliation/script/10_plot_water_quality_trends.R"
# ============================================================

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 1. USER CONFIGURATION
# -----------------------------------------------------------------------------

PROJECT_DIR <- "D:/# R Project/penelitian"
OUTPUT_DIR <- file.path(
  PROJECT_DIR,
  "teliti_reconciliation",
  "output",
  "water_quality_trends"
)

# Canonical analysis files -----------------------------------------------------
ANALYSIS_DIR <- file.path(
  PROJECT_DIR,
  "teliti_reconciliation",
  "analysis"
)

FUJIAN_MASTER <- file.path(ANALYSIS_DIR, "fujian_weekly_analysis.rds")
ONLIMO_HISTORICAL <- file.path(ANALYSIS_DIR, "onlimo_historical_analysis.rds")
ONLIMO_DAILY <- file.path(ANALYSIS_DIR, "onlimo_daily_analysis.rds")
NMEMC_MARINE_MASTER <- file.path(ANALYSIS_DIR, "nmemc_marine_analysis.rds")
CNEMC_MASTER_RDS <- file.path(ANALYSIS_DIR, "cnemc_latest_analysis.rds")
CNEMC_MASTER_CSV <- NA_character_

# Figure settings -------------------------------------------------------------
OVERVIEW_TITLE <- "Environmental trends: historical context to current monitoring"
OVERVIEW_SUBTITLE <- paste(
  "Descriptive station-balanced summaries from the reconciled canonical",
  "Fujian/China and Indonesia analysis datasets"
)
RECENT_TITLE <- "Recent monitoring dynamics"
RECENT_SUBTITLE <- "Short-term signals from high-frequency CNEMC and ONLIMO daily archives"

OUTPUT_DPI <- 320
OVERVIEW_WIDTH <- 15
OVERVIEW_HEIGHT <- 11
RECENT_WIDTH <- 15
RECENT_HEIGHT <- 8.5
SAVE_PDF <- TRUE
SAVE_INDIVIDUAL_PANELS <- TRUE

# Aggregation settings --------------------------------------------------------
# "station_balanced" first summarizes each station within a time period and
# then summarizes across stations. This reduces bias from stations with more
# observations. Use "all_observations" only for exploratory sensitivity checks.
SUMMARY_METHOD <- "station_balanced"

# Recent-window settings ------------------------------------------------------
CNEMC_RECENT_DAYS <- 60L
ONLIMO_RECENT_DAYS <- 60L

# ONLIMO plotting scope -------------------------------------------------------
# The daily archive may contain many active watersheds. Plotting every one in a
# presentation panel is not useful and can create figures dozens of inches tall.
# The exported CSV summaries still retain ALL watersheds; these settings affect
# only presentation/diagnostic figures.
ONLIMO_FOCUS_WATERSHEDS <- c(
  "Ciliwung",
  "Cisadane",
  "Citarum",
  "Bengawan Solo",
  "Musi"
)

MAX_ONLIMO_FALLBACK_WATERSHEDS <- 8L
MAX_ONLIMO_DETAIL_HEIGHT <- 18

# Fixed colors keep the five research watersheds visually consistent between
# historical and recent figures. Unknown/fallback watersheds receive HCL colors.
ONLIMO_FOCUS_COLORS <- c(
  "Ciliwung" = "#0072B2",
  "Cisadane" = "#009E73",
  "Citarum" = "#D55E00",
  "Bengawan Solo" = "#CC79A7",
  "Musi" = "#E69F00"
)

# Geographic filters ----------------------------------------------------------
# NMEMC marine and CNEMC use Chinese administrative labels. These regexes are
# deliberately permissive to work with Chinese or English labels.
FUJIAN_REGEX <- "福建|Fujian"
JAKARTA_REGEX <- "Jakarta|DKI"
XIAMEN_REGEX <- "厦门|Xiamen"

# Core Fujian weekly parameters ----------------------------------------------
FUJIAN_PARAMETERS <- c(
  "dissolved_oxygen_mg_l_raw",
  "permanganate_index_mg_l_raw",
  "total_phosphorus_mg_l_raw",
  "ammonia_nitrogen_mg_l_raw",
  "total_nitrogen_mg_l_raw"
)

FUJIAN_PARAMETER_LABELS <- c(
  dissolved_oxygen_mg_l_raw = "Dissolved oxygen",
  permanganate_index_mg_l_raw = "CODMn",
  total_phosphorus_mg_l_raw = "Total phosphorus",
  ammonia_nitrogen_mg_l_raw = "Ammonia-N",
  total_nitrogen_mg_l_raw = "Total nitrogen"
)

FUJIAN_PARAMETER_UNITS <- c(
  dissolved_oxygen_mg_l_raw = "mg/L",
  permanganate_index_mg_l_raw = "mg/L",
  total_phosphorus_mg_l_raw = "mg/L",
  ammonia_nitrogen_mg_l_raw = "mg/L",
  total_nitrogen_mg_l_raw = "mg/L"
)

# CNEMC recent parameters -----------------------------------------------------
CNEMC_PARAMETERS <- c(
  "dissolved_oxygen_mg_l_raw",
  "ammonia_nitrogen_mg_l_raw",
  "total_phosphorus_mg_l_raw"
)

CNEMC_PARAMETER_LABELS <- c(
  dissolved_oxygen_mg_l_raw = "Dissolved oxygen",
  ammonia_nitrogen_mg_l_raw = "Ammonia-N",
  total_phosphorus_mg_l_raw = "Total phosphorus"
)

# Presentation colors ---------------------------------------------------------
COLORS <- list(
  text = "#1F2937",
  muted = "#6B7280",
  grid = "#E5E7EB",
  title = "#163A5F",
  primary = "#2563EB",
  secondary = "#059669",
  accent = "#D97706",
  warning = "#DC2626",
  ribbon = "#93C5FD",
  marine = "#0891B2",
  indonesia = "#7C3AED",
  threshold = "#9CA3AF"
)

# -----------------------------------------------------------------------------
# 2. PACKAGE CHECKS
# -----------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "ggplot2",
  "patchwork",
  "stringr",
  "lubridate",
  "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
  library(stringr)
  library(lubridate)
  library(scales)
})

if (!SUMMARY_METHOD %in% c("station_balanced", "all_observations")) {
  stop("SUMMARY_METHOD must be 'station_balanced' or 'all_observations'.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 3. GENERAL HELPERS
# -----------------------------------------------------------------------------

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE, ...),
    error = function(e) {
      message("Could not read ", path, ": ", conditionMessage(e))
      NULL
    }
  )
}

safe_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    readRDS(path),
    error = function(e) {
      message("Could not read ", path, ": ", conditionMessage(e))
      NULL
    }
  )
}

safe_read_dataset <- function(path, ...) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    return(safe_read_rds(path))
  }
  safe_read_csv(path, ...)
}

safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE, type = 7))
}

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

# Keep geom_line() away from groups that contain only one time point.
# Points are still plotted for those groups, but a one-point "line" is not requested.
line_ready <- function(data, group_cols = NULL) {
  if (is.null(data) || nrow(data) == 0L) return(data)

  if (is.null(group_cols) || length(group_cols) == 0L) {
    if (nrow(data) >= 2L) return(data)
    return(data[0, , drop = FALSE])
  }

  data %>%
    group_by(across(all_of(group_cols))) %>%
    filter(dplyr::n() >= 2L) %>%
    ungroup()
}

# Standardize the configured research watersheds without altering other source
# labels. This protects plots from harmless case/whitespace variants.
canonicalize_onlimo_watershed <- function(x) {
  raw <- stringr::str_squish(as.character(x))
  raw[raw == ""] <- NA_character_

  key <- stringr::str_to_lower(raw)
  focus_key <- stringr::str_to_lower(ONLIMO_FOCUS_WATERSHEDS)
  idx <- match(key, focus_key)

  out <- raw
  matched <- !is.na(idx)
  out[matched] <- ONLIMO_FOCUS_WATERSHEDS[idx[matched]]
  out
}

# Choose a bounded plotting scope. If any configured research watersheds are
# present, plot those. Otherwise fall back to the watersheds with the greatest
# observed station coverage. The full summary table is never filtered.
select_onlimo_plot_data <- function(data) {
  if (is.null(data) || nrow(data) == 0L || !"watershed" %in% names(data)) {
    return(data)
  }

  present_focus <- ONLIMO_FOCUS_WATERSHEDS[
    ONLIMO_FOCUS_WATERSHEDS %in% unique(as.character(data$watershed))
  ]

  if (length(present_focus) > 0L) {
    return(
      data %>%
        filter(watershed %in% present_focus) %>%
        mutate(watershed = factor(watershed, levels = present_focus))
    )
  }

  ranking <- data %>%
    group_by(watershed) %>%
    summarise(
      station_coverage = if ("n_stations" %in% names(data)) {
        max(n_stations, na.rm = TRUE)
      } else {
        dplyr::n()
      },
      .groups = "drop"
    ) %>%
    arrange(desc(station_coverage), watershed) %>%
    slice_head(n = MAX_ONLIMO_FALLBACK_WATERSHEDS)

  selected <- as.character(ranking$watershed)
  data %>%
    filter(watershed %in% selected) %>%
    mutate(watershed = factor(watershed, levels = selected))
}

# Build a named qualitative palette of any required length. This avoids the
# RColorBrewer Dark2 eight-category limit that previously turned extra groups
# into NA colours and caused geom_point() rows to be removed.
watershed_palette <- function(watersheds) {
  ws <- unique(as.character(stats::na.omit(watersheds)))
  if (length(ws) == 0L) return(character())

  cols <- rep(NA_character_, length(ws))
  names(cols) <- ws

  known <- ws %in% names(ONLIMO_FOCUS_COLORS)
  cols[known] <- ONLIMO_FOCUS_COLORS[ws[known]]

  unknown <- ws[!known]
  if (length(unknown) > 0L) {
    cols[unknown] <- grDevices::hcl.colors(length(unknown), palette = "Dark 3")
  }

  cols
}

# Pollution Index reference lines are deliberately separate scalar layers.
# Do not replace these with vector-valued linetype parameters: older/newer
# ggplot2 versions can interpret those vectors against the plot data rows.
ip_reference_layers <- function() {
  list(
    geom_hline(
      yintercept = 1, linetype = "dashed",
      colour = COLORS$threshold, linewidth = 0.45
    ),
    geom_hline(
      yintercept = 5, linetype = "dotted",
      colour = COLORS$threshold, linewidth = 0.45
    ),
    geom_hline(
      yintercept = 10, linetype = "dotdash",
      colour = COLORS$threshold, linewidth = 0.45
    )
  )
}

parse_measurement <- function(x) {
  z <- trimws(as.character(x))
  z[z %in% c("", "-", "--", "NA", "N/A")] <- NA_character_
  suppressWarnings(readr::parse_number(z, na = c("", "NA", "N/A", "-", "--")))
}

is_censored_measurement <- function(x) {
  z <- trimws(as.character(x))
  stringr::str_detect(z, "^(<|>|<=|>=|≤|≥)")
}

parse_water_quality_class <- function(x) {
  z <- as.character(x)
  z <- stringr::str_trim(z)
  z <- stringr::str_to_upper(z)
  z <- stringr::str_replace_all(
    z,
    "\\s+|水质|水質|海水|类别|類別|类|類|级|級|CLASS|GRADE",
    ""
  )

  parse_one <- function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_integer_)
    if (stringr::str_detect(s, "劣")) return(6L)

    # Exact values first.
    exact_map <- c(
      "Ⅰ" = 1L, "I" = 1L, "1" = 1L, "一" = 1L,
      "Ⅱ" = 2L, "II" = 2L, "2" = 2L, "二" = 2L,
      "Ⅲ" = 3L, "III" = 3L, "3" = 3L, "三" = 3L,
      "Ⅳ" = 4L, "IV" = 4L, "4" = 4L, "四" = 4L,
      "Ⅴ" = 5L, "V" = 5L, "5" = 5L, "五" = 5L,
      "Ⅵ" = 6L, "VI" = 6L, "6" = 6L, "六" = 6L
    )
    if (s %in% names(exact_map)) return(unname(exact_map[[s]]))

    # Some source values are ranges such as Ⅱ-Ⅲ. For descriptive plotting,
    # use the worse (higher-numbered) class in the published range.
    tests <- list(
      `6` = c("Ⅵ", "(^|[^A-Z])VI([^A-Z]|$)", "六"),
      `5` = c("Ⅴ", "(^|[^A-Z])V([^A-Z]|$)", "五"),
      `4` = c("Ⅳ", "(^|[^A-Z])IV([^A-Z]|$)", "四"),
      `3` = c("Ⅲ", "(^|[^A-Z])III([^A-Z]|$)", "三"),
      `2` = c("Ⅱ", "(^|[^A-Z])II([^A-Z]|$)", "二"),
      `1` = c("Ⅰ", "(^|[^A-Z])I([^A-Z]|$)", "一")
    )
    for (score in names(tests)) {
      if (any(vapply(tests[[score]], function(p) stringr::str_detect(s, p), logical(1)))) {
        return(as.integer(score))
      }
    }
    NA_integer_
  }

  vapply(z, parse_one, integer(1))
}

base_theme <- function(base_size = 11.5) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 15, colour = COLORS$title),
      plot.subtitle = element_text(size = 10, colour = COLORS$muted),
      plot.caption = element_text(size = 8.5, colour = COLORS$muted),
      axis.title = element_text(face = "bold", colour = COLORS$text),
      axis.text = element_text(colour = COLORS$text),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = COLORS$grid, linewidth = 0.25),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", colour = COLORS$text),
      strip.background = element_rect(fill = "#F8FAFC", colour = NA)
    )
}

empty_panel <- function(title, message_text) {
  ggplot() +
    annotate("text", x = 0, y = 0.15, label = title, hjust = 0, fontface = "bold", size = 5, colour = COLORS$title) +
    annotate("text", x = 0, y = -0.05, label = stringr::str_wrap(message_text, 60), hjust = 0, vjust = 1, size = 3.8, colour = COLORS$muted) +
    xlim(0, 1) +
    ylim(-1, 0.5) +
    theme_void()
}

save_plot_pair <- function(plot_obj, stem, width, height) {
  png_path <- file.path(OUTPUT_DIR, paste0(stem, ".png"))
  ggsave(
    filename = png_path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = OUTPUT_DPI,
    bg = "white"
  )

  if (isTRUE(SAVE_PDF)) {
    pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
    ggsave(
      filename = file.path(OUTPUT_DIR, paste0(stem, ".pdf")),
      plot = plot_obj,
      width = width,
      height = height,
      bg = "white",
      device = pdf_device
    )
  }
}

# -----------------------------------------------------------------------------
# 4. FUJIAN WEEKLY: LONG-TERM WATER-QUALITY CLASS
# -----------------------------------------------------------------------------

build_fujian_class <- function() {
  dat <- safe_read_dataset(FUJIAN_MASTER)
  if (is.null(dat) || nrow(dat) == 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "Fujian weekly water-quality class",
      paste0("No readable data found at: ", FUJIAN_MASTER)
    )))
  }

  needed <- c("year", "station_name", "current_week_water_quality")
  missing <- setdiff(needed, names(dat))
  if (length(missing) > 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "Fujian weekly water-quality class",
      paste("Missing columns:", paste(missing, collapse = ", "))
    )))
  }

  x <- dat %>%
    transmute(
      year = suppressWarnings(as.integer(year)),
      station_name = as.character(station_name),
      class_raw = as.character(current_week_water_quality),
      class_score = parse_water_quality_class(class_raw)
    ) %>%
    filter(!is.na(year), !is.na(station_name), !is.na(class_score))

  if (nrow(x) == 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "Fujian weekly water-quality class",
      "Water-quality class values could not be parsed."
    )))
  }

  if (SUMMARY_METHOD == "station_balanced") {
    station_year <- x %>%
      group_by(year, station_name) %>%
      summarise(
        good_share = mean(class_score <= 3, na.rm = TRUE),
        median_class = median(class_score, na.rm = TRUE),
        n_observations = n(),
        .groups = "drop"
      )

    annual <- station_year %>%
      group_by(year) %>%
      summarise(
        median_good_pct = 100 * median(good_share, na.rm = TRUE),
        q25_good_pct = 100 * safe_quantile(good_share, 0.25),
        q75_good_pct = 100 * safe_quantile(good_share, 0.75),
        median_class_score = median(median_class, na.rm = TRUE),
        n_stations = n_distinct(station_name),
        .groups = "drop"
      )
  } else {
    annual <- x %>%
      group_by(year) %>%
      summarise(
        median_good_pct = 100 * mean(class_score <= 3, na.rm = TRUE),
        q25_good_pct = NA_real_,
        q75_good_pct = NA_real_,
        median_class_score = median(class_score, na.rm = TRUE),
        n_stations = n_distinct(station_name),
        .groups = "drop"
      )
  }

  p <- ggplot(annual, aes(x = year, y = median_good_pct)) +
    geom_ribbon(
      aes(ymin = q25_good_pct, ymax = q75_good_pct),
      fill = COLORS$ribbon,
      alpha = 0.30,
      na.rm = TRUE
    ) +
    geom_line(data = line_ready(annual), aes(group = 1), linewidth = 1.0, colour = COLORS$primary) +
    geom_point(size = 2.0, colour = COLORS$primary) +
    scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    scale_y_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
    labs(
      title = "A. Fujian long-term surface-water quality",
      subtitle = "Annual station-balanced share of observations classified I-III; ribbon = station IQR",
      x = NULL,
      y = "Class I-III (%)",
      caption = "Higher values indicate a larger share of observations in classes I-III."
    ) +
    base_theme()

  list(data = annual, plot = p)
}

# -----------------------------------------------------------------------------
# 5. FUJIAN WEEKLY: LONG-TERM PARAMETER TRENDS
# -----------------------------------------------------------------------------

build_fujian_parameters <- function() {
  dat <- safe_read_dataset(FUJIAN_MASTER)
  if (is.null(dat) || nrow(dat) == 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "Fujian weekly parameters",
      paste0("No readable data found at: ", FUJIAN_MASTER)
    )))
  }

  needed <- c("year", "station_name", FUJIAN_PARAMETERS)
  missing <- setdiff(needed, names(dat))
  if (length(missing) > 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "Fujian weekly parameters",
      paste("Missing columns:", paste(missing, collapse = ", "))
    )))
  }

  long <- dat %>%
    select(year, station_name, all_of(FUJIAN_PARAMETERS)) %>%
    pivot_longer(
      cols = all_of(FUJIAN_PARAMETERS),
      names_to = "parameter",
      values_to = "raw_value"
    ) %>%
    mutate(
      year = suppressWarnings(as.integer(year)),
      value = parse_measurement(raw_value),
      censored = is_censored_measurement(raw_value),
      parameter_label = unname(FUJIAN_PARAMETER_LABELS[parameter]),
      unit = unname(FUJIAN_PARAMETER_UNITS[parameter])
    ) %>%
    filter(!is.na(year), !is.na(station_name), is.finite(value))

  if (nrow(long) == 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "Fujian weekly parameters",
      "Selected parameter values could not be parsed."
    )))
  }

  if (SUMMARY_METHOD == "station_balanced") {
    station_year <- long %>%
      group_by(year, station_name, parameter, parameter_label, unit) %>%
      summarise(
        station_median = median(value, na.rm = TRUE),
        station_censored_pct = 100 * mean(censored, na.rm = TRUE),
        n_observations = n(),
        .groups = "drop"
      )

    annual <- station_year %>%
      group_by(year, parameter, parameter_label, unit) %>%
      summarise(
        median = median(station_median, na.rm = TRUE),
        q25 = safe_quantile(station_median, 0.25),
        q75 = safe_quantile(station_median, 0.75),
        censored_pct = mean(station_censored_pct, na.rm = TRUE),
        n_stations = n_distinct(station_name),
        .groups = "drop"
      )
  } else {
    annual <- long %>%
      group_by(year, parameter, parameter_label, unit) %>%
      summarise(
        median = median(value, na.rm = TRUE),
        q25 = safe_quantile(value, 0.25),
        q75 = safe_quantile(value, 0.75),
        censored_pct = 100 * mean(censored, na.rm = TRUE),
        n_stations = n_distinct(station_name),
        .groups = "drop"
      )
  }

  p <- ggplot(annual, aes(x = year, y = median)) +
    geom_ribbon(
      aes(ymin = q25, ymax = q75),
      fill = COLORS$ribbon,
      alpha = 0.25,
      na.rm = TRUE
    ) +
    geom_line(data = line_ready(annual, "parameter"), aes(group = parameter), linewidth = 0.8, colour = COLORS$secondary) +
    geom_point(size = 1.4, colour = COLORS$secondary) +
    facet_wrap(~ parameter_label, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = scales::pretty_breaks(6)) +
    labs(
      title = "B. Fujian core parameter trajectories",
      subtitle = "Annual station-balanced medians; ribbons show the interquartile range across stations",
      x = NULL,
      y = "Published concentration (mg/L)",
      caption = paste(
        "Qualified values such as <x are plotted at the reported numerical threshold for descriptive visualization.",
        "Formal analysis should treat censoring explicitly."
      )
    ) +
    base_theme(base_size = 10.5) +
    theme(legend.position = "none")

  list(data = annual, plot = p)
}

# -----------------------------------------------------------------------------
# 6. ONLIMO HISTORICAL: POLLUTION INDEX BY WATERSHED
# -----------------------------------------------------------------------------

build_onlimo_historical <- function() {
  dat <- safe_read_dataset(
    ONLIMO_HISTORICAL,
    col_types = cols(
      station_id = col_character(),
      date = col_date(),
      retrieved_at = col_character(),
      .default = col_guess()
    )
  )

  if (is.null(dat) || nrow(dat) == 0L) {
    blank <- empty_panel(
      "ONLIMO historical Pollution Index",
      paste0("No readable data found at: ", ONLIMO_HISTORICAL)
    )
    return(list(data = tibble(), plot = blank, plot_detail = blank, n_watersheds = 0L, n_plot_watersheds = 0L))
  }

  needed <- c("station_id", "date", "pollution_index", "watershed")
  missing <- setdiff(needed, names(dat))
  if (length(missing) > 0L) {
    blank <- empty_panel(
      "ONLIMO historical Pollution Index",
      paste("Missing columns:", paste(missing, collapse = ", "))
    )
    return(list(data = tibble(), plot = blank, plot_detail = blank, n_watersheds = 0L, n_plot_watersheds = 0L))
  }

  x <- dat %>%
    transmute(
      station_id = as.character(station_id),
      date = as.Date(date),
      watershed = canonicalize_onlimo_watershed(watershed),
      pollution_index = suppressWarnings(as.numeric(pollution_index)),
      month = as.Date(lubridate::floor_date(as.Date(date), unit = "month"))
    ) %>%
    filter(
      !is.na(station_id),
      !is.na(date),
      !is.na(month),
      !is.na(watershed),
      nzchar(watershed),
      is.finite(pollution_index)
    )

  if (nrow(x) == 0L) {
    blank <- empty_panel(
      "ONLIMO historical Pollution Index",
      "No valid station-date Pollution Index values were available."
    )
    return(list(data = tibble(), plot = blank, plot_detail = blank, n_watersheds = 0L, n_plot_watersheds = 0L))
  }

  # One station contributes one value per month before watershed aggregation.
  station_month <- x %>%
    group_by(month, watershed, station_id) %>%
    summarise(
      station_median_ip = median(pollution_index, na.rm = TRUE),
      n_days = n_distinct(date),
      .groups = "drop"
    )

  monthly <- station_month %>%
    group_by(month, watershed) %>%
    summarise(
      median_ip = median(station_median_ip, na.rm = TRUE),
      q25_ip = safe_quantile(station_median_ip, 0.25),
      q75_ip = safe_quantile(station_median_ip, 0.75),
      n_stations = n_distinct(station_id),
      .groups = "drop"
    ) %>%
    arrange(watershed, month)

  # Keep all watershed summaries for export, but use a bounded research-focused
  # subset for visual presentation and diagnostic facets.
  plot_data <- select_onlimo_plot_data(monthly)
  line_data <- line_ready(plot_data, "watershed")
  n_watersheds <- n_distinct(monthly$watershed)
  n_plot_watersheds <- n_distinct(plot_data$watershed)
  ws_colors <- watershed_palette(plot_data$watershed)

  message(
    "  ONLIMO historical: plotting ", n_plot_watersheds,
    " watershed(s) from ", n_watersheds, " summarized watershed label(s)."
  )

  # Presentation plot: compact, research-focused, no ribbons/facets.
  p_compact <- ggplot(
    plot_data,
    aes(x = month, y = median_ip, colour = watershed, group = watershed)
  ) +
    ip_reference_layers() +
    geom_line(data = line_data, linewidth = 0.85, na.rm = TRUE) +
    geom_point(size = 1.45, alpha = 0.90, na.rm = TRUE) +
    scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
    scale_colour_manual(values = ws_colors, drop = FALSE) +
    labs(
      title = "C. ONLIMO historical Pollution Index",
      subtitle = "Monthly station-balanced median for configured research watersheds",
      x = NULL,
      y = "Pollution Index (IP)",
      colour = "Watershed",
      caption = paste(
        "Reference lines: IP = 1, 5, and 10.",
        "CSV output retains all summarized watersheds; the figure uses the configured focus set."
      )
    ) +
    base_theme(base_size = 10.0) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom",
      legend.key.width = grid::unit(1.2, "lines")
    )

  # Diagnostic plot: one full-width panel per plotted watershed with station IQR.
  p_detail <- ggplot(plot_data, aes(x = month, y = median_ip, group = watershed)) +
    ip_reference_layers() +
    geom_ribbon(
      aes(ymin = q25_ip, ymax = q75_ip),
      fill = COLORS$indonesia,
      alpha = 0.12,
      colour = NA,
      na.rm = TRUE
    ) +
    geom_line(data = line_data, linewidth = 0.85, colour = COLORS$indonesia, na.rm = TRUE) +
    geom_point(size = 1.35, colour = COLORS$indonesia, alpha = 0.90, na.rm = TRUE) +
    facet_wrap(~ watershed, ncol = 1, scales = "free_x") +
    scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 months") +
    labs(
      title = "ONLIMO historical Pollution Index by research watershed",
      subtitle = "Monthly station-balanced median; ribbon = interquartile range across stations",
      x = NULL,
      y = "Pollution Index (IP)",
      caption = paste(
        "Reference lines: IP = 1, 5, and 10.",
        "Detailed figure is intentionally limited to the configured research watersheds."
      )
    ) +
    base_theme(base_size = 10.5) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "none",
      panel.spacing.y = grid::unit(0.9, "lines")
    )

  list(
    data = monthly,
    plot = p_compact,
    plot_detail = p_detail,
    n_watersheds = n_watersheds,
    n_plot_watersheds = n_plot_watersheds
  )
}

# -----------------------------------------------------------------------------
# 7. NMEMC MARINE: FUJIAN COASTAL WATER-QUALITY CLASS
# -----------------------------------------------------------------------------

build_nmemc_fujian <- function() {
  dat <- safe_read_dataset(NMEMC_MARINE_MASTER)
  if (is.null(dat) || nrow(dat) == 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "NMEMC Fujian marine water quality",
      paste0("No readable data found at: ", NMEMC_MARINE_MASTER)
    )))
  }

  needed <- c("source_year", "province", "site_code", "water_quality_class")
  missing <- setdiff(needed, names(dat))
  if (length(missing) > 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "NMEMC Fujian marine water quality",
      paste("Missing columns:", paste(missing, collapse = ", "))
    )))
  }

  x <- dat %>%
    transmute(
      year = suppressWarnings(as.integer(source_year)),
      province = as.character(province),
      site_code = as.character(site_code),
      class_raw = as.character(water_quality_class),
      class_score = parse_water_quality_class(class_raw)
    ) %>%
    filter(
      !is.na(year),
      !is.na(site_code),
      stringr::str_detect(province %||% "", regex(FUJIAN_REGEX, ignore_case = TRUE)),
      !is.na(class_score)
    )

  if (nrow(x) == 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "D. Fujian coastal marine water quality",
      paste0(
        "NMEMC data were found, but no rows matched FUJIAN_REGEX = '",
        FUJIAN_REGEX,
        "'. Check the province labels if you expect Fujian records."
      )
    )))
  }

  site_year <- x %>%
    group_by(year, site_code) %>%
    summarise(
      site_median_class = median(class_score, na.rm = TRUE),
      n_records = n(),
      .groups = "drop"
    )

  annual <- site_year %>%
    group_by(year) %>%
    summarise(
      median_class = median(site_median_class, na.rm = TRUE),
      q25_class = safe_quantile(site_median_class, 0.25),
      q75_class = safe_quantile(site_median_class, 0.75),
      n_sites = n_distinct(site_code),
      .groups = "drop"
    )

  max_class <- max(c(4, annual$q75_class), na.rm = TRUE)
  y_breaks <- seq_len(max(4L, ceiling(max_class)))
  roman_labels <- c("I", "II", "III", "IV", "V", "Worse than V")
  roman_labels <- roman_labels[seq_along(y_breaks)]

  p <- ggplot(annual, aes(x = year, y = median_class)) +
    geom_ribbon(
      aes(ymin = q25_class, ymax = q75_class),
      fill = COLORS$marine,
      alpha = 0.18
    ) +
    geom_line(data = line_ready(annual), aes(group = 1), linewidth = 1.0, colour = COLORS$marine) +
    geom_point(size = 2.0, colour = COLORS$marine) +
    scale_x_continuous(breaks = scales::pretty_breaks(6)) +
    scale_y_reverse(breaks = y_breaks, labels = roman_labels) +
    labs(
      title = "D. Fujian coastal marine water quality",
      subtitle = "Annual station-balanced NMEMC marine class; lower class number indicates better reported quality",
      x = NULL,
      y = "Reported class",
      caption = "This provides a coastal receiving-water counterpart to the inland Fujian weekly archive."
    ) +
    base_theme()

  list(data = annual, plot = p)
}

# -----------------------------------------------------------------------------
# 8. CNEMC FUJIAN: RECENT HIGH-FREQUENCY PARAMETER DYNAMICS
# -----------------------------------------------------------------------------

read_cnemc_master <- function() {
  x <- safe_read_dataset(CNEMC_MASTER_RDS)
  if (!is.null(x)) return(tibble::as_tibble(x))
  NULL
}

build_cnemc_recent <- function() {
  dat <- read_cnemc_master()
  if (is.null(dat) || nrow(dat) == 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "CNEMC Fujian recent monitoring",
      "No readable cumulative CNEMC archive was found."
    )))
  }

  needed <- c("area", "monitoring_section", "observation_datetime", CNEMC_PARAMETERS)
  missing <- setdiff(needed, names(dat))
  if (length(missing) > 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "CNEMC Fujian recent monitoring",
      paste("Missing columns:", paste(missing, collapse = ", "))
    )))
  }

  # Preferred/latest published version for each observation key.
  if ("observation_key_hash" %in% names(dat)) {
    ordering_col <- if ("last_seen" %in% names(dat)) {
      "last_seen"
    } else if ("collected_at" %in% names(dat)) {
      "collected_at"
    } else {
      NULL
    }

    if (!is.null(ordering_col)) {
      dat <- dat %>%
        mutate(.version_time = suppressWarnings(as.POSIXct(.data[[ordering_col]], tz = "Asia/Shanghai"))) %>%
        group_by(observation_key_hash) %>%
        arrange(.version_time, .by_group = TRUE) %>%
        slice_tail(n = 1L) %>%
        ungroup()
    } else {
      dat <- dat %>%
        group_by(observation_key_hash) %>%
        slice_tail(n = 1L) %>%
        ungroup()
    }
  }

  long <- dat %>%
    filter(stringr::str_detect(as.character(area), regex(FUJIAN_REGEX, ignore_case = TRUE))) %>%
    transmute(
      monitoring_section = as.character(monitoring_section),
      observation_datetime = suppressWarnings(as.POSIXct(observation_datetime, tz = "Asia/Shanghai")),
      across(all_of(CNEMC_PARAMETERS), as.character)
    ) %>%
    filter(!is.na(observation_datetime), !is.na(monitoring_section)) %>%
    pivot_longer(
      cols = all_of(CNEMC_PARAMETERS),
      names_to = "parameter",
      values_to = "raw_value"
    ) %>%
    mutate(
      date = as.Date(observation_datetime),
      value = parse_measurement(raw_value),
      parameter_label = unname(CNEMC_PARAMETER_LABELS[parameter])
    ) %>%
    filter(is.finite(value))

  if (nrow(long) == 0L) {
    return(list(data = tibble(), plot = empty_panel(
      "CNEMC Fujian recent monitoring",
      paste0("No recent rows matched FUJIAN_REGEX = '", FUJIAN_REGEX, "'.")
    )))
  }

  latest_date <- max(long$date, na.rm = TRUE)
  cutoff <- latest_date - (CNEMC_RECENT_DAYS - 1L)
  long <- long %>% filter(date >= cutoff)

  station_day <- long %>%
    group_by(date, monitoring_section, parameter, parameter_label) %>%
    summarise(station_median = median(value, na.rm = TRUE), .groups = "drop")

  daily <- station_day %>%
    group_by(date, parameter, parameter_label) %>%
    summarise(
      median = median(station_median, na.rm = TRUE),
      q25 = safe_quantile(station_median, 0.25),
      q75 = safe_quantile(station_median, 0.75),
      n_stations = n_distinct(monitoring_section),
      .groups = "drop"
    )

  p <- ggplot(daily, aes(x = date, y = median)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = COLORS$ribbon, alpha = 0.25) +
    geom_line(data = line_ready(daily, "parameter"), aes(group = parameter), linewidth = 0.85, colour = COLORS$primary) +
    facet_wrap(~ parameter_label, scales = "free_y", ncol = 1) +
    scale_x_date(date_labels = "%d %b", date_breaks = "1 week") +
    labs(
      title = "A. CNEMC Fujian recent dynamics",
      subtitle = paste0("Latest published row version; station-balanced daily medians over the most recent ", CNEMC_RECENT_DAYS, " days"),
      x = NULL,
      y = "Published concentration (mg/L)",
      caption = "Revision-aware analytical view: for each observation key, the latest retained published version is used."
    ) +
    base_theme(base_size = 10.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  list(data = daily, plot = p)
}

# -----------------------------------------------------------------------------
# 9. ONLIMO DAILY: RECENT POLLUTION INDEX DYNAMICS
# -----------------------------------------------------------------------------

build_onlimo_daily_recent <- function() {
  dat <- safe_read_dataset(
    ONLIMO_DAILY,
    col_types = cols(
      station_id = col_character(),
      date = col_date(),
      retrieved_at = col_character(),
      .default = col_guess()
    )
  )

  if (is.null(dat) || nrow(dat) == 0L) {
    blank <- empty_panel(
      "ONLIMO recent monitoring",
      paste0("No readable data found at: ", ONLIMO_DAILY)
    )
    return(list(data = tibble(), plot = blank, plot_detail = blank, n_watersheds = 0L, n_plot_watersheds = 0L))
  }

  needed <- c("station_id", "date", "watershed", "pollution_index")
  missing <- setdiff(needed, names(dat))
  if (length(missing) > 0L) {
    blank <- empty_panel(
      "ONLIMO recent monitoring",
      paste("Missing columns:", paste(missing, collapse = ", "))
    )
    return(list(data = tibble(), plot = blank, plot_detail = blank, n_watersheds = 0L, n_plot_watersheds = 0L))
  }

  x <- dat %>%
    transmute(
      station_id = as.character(station_id),
      date = as.Date(date),
      watershed = canonicalize_onlimo_watershed(watershed),
      pollution_index = suppressWarnings(as.numeric(pollution_index))
    ) %>%
    filter(
      !is.na(station_id),
      !is.na(date),
      !is.na(watershed),
      nzchar(watershed),
      is.finite(pollution_index)
    )

  if (nrow(x) == 0L) {
    blank <- empty_panel(
      "ONLIMO recent monitoring",
      "No valid recent Pollution Index values were available."
    )
    return(list(data = tibble(), plot = blank, plot_detail = blank, n_watersheds = 0L, n_plot_watersheds = 0L))
  }

  latest_date <- max(x$date, na.rm = TRUE)
  cutoff <- latest_date - (ONLIMO_RECENT_DAYS - 1L)
  x <- x %>% filter(date >= cutoff)

  # First reduce multiple same-day records to one value per station-day.
  station_day <- x %>%
    group_by(date, watershed, station_id) %>%
    summarise(
      station_median_ip = median(pollution_index, na.rm = TRUE),
      .groups = "drop"
    )

  # Then summarize across stations so heavily sampled stations do not dominate.
  daily <- station_day %>%
    group_by(date, watershed) %>%
    summarise(
      median_ip = median(station_median_ip, na.rm = TRUE),
      q25_ip = safe_quantile(station_median_ip, 0.25),
      q75_ip = safe_quantile(station_median_ip, 0.75),
      n_stations = n_distinct(station_id),
      .groups = "drop"
    ) %>%
    arrange(watershed, date)

  # Preserve all daily watershed summaries in the exported CSV, but keep plots
  # bounded to the configured research watersheds (or a small fallback set).
  plot_data <- select_onlimo_plot_data(daily)
  line_data <- line_ready(plot_data, "watershed")
  n_watersheds <- n_distinct(daily$watershed)
  n_plot_watersheds <- n_distinct(plot_data$watershed)
  ws_colors <- watershed_palette(plot_data$watershed)

  message(
    "  ONLIMO recent: plotting ", n_plot_watersheds,
    " watershed(s) from ", n_watersheds, " summarized watershed label(s)."
  )

  # Presentation plot: compact research-watershed comparison.
  p_compact <- ggplot(
    plot_data,
    aes(x = date, y = median_ip, colour = watershed, group = watershed)
  ) +
    ip_reference_layers() +
    geom_line(data = line_data, linewidth = 0.8, na.rm = TRUE) +
    geom_point(size = 1.35, alpha = 0.85, na.rm = TRUE) +
    scale_colour_manual(values = ws_colors, drop = FALSE) +
    scale_x_date(date_labels = "%d %b", date_breaks = "1 week") +
    labs(
      title = "B. ONLIMO recent Pollution Index",
      subtitle = paste0(
        "Station-balanced daily medians for configured research watersheds; most recent ",
        ONLIMO_RECENT_DAYS, " days"
      ),
      x = NULL,
      y = "Pollution Index (IP)",
      colour = "Watershed",
      caption = paste(
        "Reference lines: IP = 1, 5, and 10.",
        "CSV output retains all active watershed summaries."
      )
    ) +
    base_theme(base_size = 10.0) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  # Diagnostic plot: one full-width panel per plotted research watershed.
  p_detail <- ggplot(plot_data, aes(x = date, y = median_ip, group = watershed)) +
    ip_reference_layers() +
    geom_ribbon(
      aes(ymin = q25_ip, ymax = q75_ip),
      fill = COLORS$indonesia,
      alpha = 0.12,
      colour = NA,
      na.rm = TRUE
    ) +
    geom_line(data = line_data, linewidth = 0.8, colour = COLORS$indonesia, na.rm = TRUE) +
    geom_point(size = 1.25, colour = COLORS$indonesia, alpha = 0.85, na.rm = TRUE) +
    facet_wrap(~ watershed, ncol = 1) +
    scale_x_date(date_labels = "%d %b", date_breaks = "1 week") +
    labs(
      title = "ONLIMO recent Pollution Index by research watershed",
      subtitle = paste0(
        "Daily station-balanced median; ribbon = interquartile range across stations; most recent ",
        ONLIMO_RECENT_DAYS, " days"
      ),
      x = NULL,
      y = "Pollution Index (IP)",
      caption = paste(
        "Reference lines: IP = 1, 5, and 10.",
        "Detailed figure is intentionally limited to the configured research watersheds."
      )
    ) +
    base_theme(base_size = 10.5) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.spacing.y = grid::unit(0.9, "lines")
    )

  list(
    data = daily,
    plot = p_compact,
    plot_detail = p_detail,
    n_watersheds = n_watersheds,
    n_plot_watersheds = n_plot_watersheds
  )
}

# -----------------------------------------------------------------------------
# 10. CITY-COMPARISON READINESS CHECK
# -----------------------------------------------------------------------------

build_city_readiness <- function() {
  # Fujian station crosswalk is optional and may live in different project
  # subdirectories. The script searches for it rather than assuming one path.
  fujian_root <- file.path(PROJECT_DIR, "fujian_surfacewater")
  crosswalk_candidates <- if (dir.exists(fujian_root)) {
    list.files(
      fujian_root,
      pattern = "fujian_station_crosswalk\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
  } else {
    character()
  }

  xiamen_station_n <- NA_integer_
  crosswalk_path <- NA_character_
  crosswalk_note <- "No Fujian station crosswalk found; city-level Xiamen filtering is not yet enabled."

  if (length(crosswalk_candidates) > 0L) {
    crosswalk_path <- crosswalk_candidates[[1]]
    cw <- safe_read_csv(crosswalk_path)

    if (!is.null(cw) && nrow(cw) > 0L) {
      location_cols <- intersect(
        c("city", "municipality", "prefecture", "city_name", "location", "admin_city"),
        names(cw)
      )
      station_cols <- intersect(
        c("station_name", "station", "station_name_cn", "station_name_en"),
        names(cw)
      )

      if (length(location_cols) > 0L && length(station_cols) > 0L) {
        loc_text <- apply(cw[, location_cols, drop = FALSE], 1, paste, collapse = " | ")
        keep <- str_detect(loc_text, regex(XIAMEN_REGEX, ignore_case = TRUE))
        keep[is.na(keep)] <- FALSE
        station_text <- apply(cw[, station_cols, drop = FALSE], 1, paste, collapse = " | ")
        xiamen_station_n <- length(unique(station_text[keep & nzchar(station_text)]))
        crosswalk_note <- paste0("Crosswalk found: ", crosswalk_path)
      } else {
        crosswalk_note <- paste0(
          "Crosswalk found but no recognizable city/station columns were detected: ",
          crosswalk_path
        )
      }
    }
  }

  onlimo_hist <- safe_read_dataset(ONLIMO_HISTORICAL)
  jakarta_station_n <- NA_integer_
  jakarta_note <- "ONLIMO historical archive unavailable."

  if (!is.null(onlimo_hist) && nrow(onlimo_hist) > 0L) {
    location_cols <- intersect(
      c("province", "kabupaten_kota", "station_name", "watershed", "river"),
      names(onlimo_hist)
    )
    if (length(location_cols) > 0L && "station_id" %in% names(onlimo_hist)) {
      loc_text <- apply(onlimo_hist[, location_cols, drop = FALSE], 1, paste, collapse = " | ")
      keep <- str_detect(loc_text, regex(JAKARTA_REGEX, ignore_case = TRUE))
      keep[is.na(keep)] <- FALSE
      jakarta_station_n <- n_distinct(onlimo_hist$station_id[keep])
      jakarta_note <- paste0("Jakarta regex matched across: ", paste(location_cols, collapse = ", "))
    }
  }

  tibble(
    city = c("Xiamen", "Jakarta"),
    source = c("Fujian station crosswalk", "ONLIMO historical metadata"),
    matched_station_count = c(xiamen_station_n, jakarta_station_n),
    ready_for_city_filter = c(!is.na(xiamen_station_n) && xiamen_station_n > 0L,
                              !is.na(jakarta_station_n) && jakarta_station_n > 0L),
    note = c(crosswalk_note, jakarta_note)
  )
}

# -----------------------------------------------------------------------------
# 11. DESCRIPTIVE TREND-DIRECTION HELPER
# -----------------------------------------------------------------------------

descriptive_slope <- function(time_value, metric_value) {
  ok <- is.finite(time_value) & is.finite(metric_value)
  time_value <- as.numeric(time_value[ok])
  metric_value <- as.numeric(metric_value[ok])
  if (length(metric_value) < 3L || length(unique(time_value)) < 2L) return(NA_real_)
  fit <- stats::lm(metric_value ~ time_value)
  unname(stats::coef(fit)[[2]])
}

trend_direction <- function(slope, tolerance = 1e-12) {
  dplyr::case_when(
    is.na(slope) ~ "insufficient_data",
    slope > tolerance ~ "increasing",
    slope < -tolerance ~ "decreasing",
    TRUE ~ "approximately_flat"
  )
}

# -----------------------------------------------------------------------------
# 12. BUILD ALL SUMMARIES AND FIGURES
# -----------------------------------------------------------------------------

message("Building Fujian water-quality class trend ...")
fujian_class <- build_fujian_class()

message("Building Fujian parameter trends ...")
fujian_params <- build_fujian_parameters()

message("Building ONLIMO historical Pollution Index trend ...")
onlimo_hist <- build_onlimo_historical()

message("Building NMEMC Fujian marine trend ...")
nmemc_fujian <- build_nmemc_fujian()

message("Building recent CNEMC Fujian dynamics ...")
cnemc_recent <- build_cnemc_recent()

message("Building recent ONLIMO daily dynamics ...")
onlimo_recent <- build_onlimo_daily_recent()

message("Checking city-comparison readiness ...")
city_readiness <- build_city_readiness()

# Save summary tables ---------------------------------------------------------
readr::write_csv(fujian_class$data, file.path(OUTPUT_DIR, "fujian_wq_class_annual.csv"), na = "")
readr::write_csv(fujian_params$data, file.path(OUTPUT_DIR, "fujian_parameter_annual.csv"), na = "")
readr::write_csv(onlimo_hist$data, file.path(OUTPUT_DIR, "onlimo_ip_monthly.csv"), na = "")
readr::write_csv(nmemc_fujian$data, file.path(OUTPUT_DIR, "nmemc_fujian_class_annual.csv"), na = "")
readr::write_csv(cnemc_recent$data, file.path(OUTPUT_DIR, "cnemc_fujian_recent.csv"), na = "")
readr::write_csv(onlimo_recent$data, file.path(OUTPUT_DIR, "onlimo_daily_recent.csv"), na = "")
readr::write_csv(city_readiness, file.path(OUTPUT_DIR, "city_comparison_readiness.csv"), na = "")

onlimo_plot_scope <- tibble(
  source = c("ONLIMO historical", "ONLIMO daily recent"),
  total_watershed_labels = c(onlimo_hist$n_watersheds, onlimo_recent$n_watersheds),
  plotted_watersheds = c(onlimo_hist$n_plot_watersheds, onlimo_recent$n_plot_watersheds),
  configured_focus = paste(ONLIMO_FOCUS_WATERSHEDS, collapse = "; "),
  note = c(
    "Full monthly summary retains all watershed labels; figures use configured focus watersheds when present.",
    "Full recent daily summary retains all active watershed labels; figures use configured focus watersheds when present."
  )
)
readr::write_csv(onlimo_plot_scope, file.path(OUTPUT_DIR, "onlimo_plot_scope.csv"), na = "")

readiness <- tibble(
  source = c(
    "Fujian weekly",
    "ONLIMO historical",
    "NMEMC Fujian marine",
    "CNEMC Fujian recent",
    "ONLIMO daily recent"
  ),
  summary_rows = c(
    nrow(fujian_class$data),
    nrow(onlimo_hist$data),
    nrow(nmemc_fujian$data),
    nrow(cnemc_recent$data),
    nrow(onlimo_recent$data)
  ),
  available = summary_rows > 0L
)
readr::write_csv(readiness, file.path(OUTPUT_DIR, "data_readiness_summary.csv"), na = "")

# Descriptive trend-direction table -------------------------------------------
trend_rows <- list()

if (nrow(fujian_class$data) > 0L) {
  s <- descriptive_slope(fujian_class$data$year, fujian_class$data$median_good_pct)
  trend_rows[[length(trend_rows) + 1L]] <- tibble(
    source = "Fujian weekly",
    series = "Class I-III share",
    time_unit = "year",
    slope_per_time_unit = s,
    direction = trend_direction(s),
    orientation = "higher_generally_better",
    note = "Descriptive slope only; no inferential trend test applied."
  )
}

if (nrow(fujian_params$data) > 0L) {
  for (prm in unique(fujian_params$data$parameter)) {
    z <- fujian_params$data %>% filter(parameter == prm)
    s <- descriptive_slope(z$year, z$median)
    orientation <- dplyr::case_when(
      prm == "dissolved_oxygen_mg_l_raw" ~ "higher_often_better_context_dependent",
      TRUE ~ "lower_generally_better_for_pollution_pressure"
    )
    trend_rows[[length(trend_rows) + 1L]] <- tibble(
      source = "Fujian weekly",
      series = unique(z$parameter_label)[1],
      time_unit = "year",
      slope_per_time_unit = s,
      direction = trend_direction(s),
      orientation = orientation,
      note = "Descriptive station-balanced annual slope."
    )
  }
}

if (nrow(onlimo_hist$data) > 0L) {
  for (ws in unique(onlimo_hist$data$watershed)) {
    z <- onlimo_hist$data %>% filter(watershed == ws)
    time_num <- as.numeric(z$month) / 365.25
    s <- descriptive_slope(time_num, z$median_ip)
    trend_rows[[length(trend_rows) + 1L]] <- tibble(
      source = "ONLIMO historical",
      series = paste0("Pollution Index - ", ws),
      time_unit = "year",
      slope_per_time_unit = s,
      direction = trend_direction(s),
      orientation = "lower_better",
      note = "Descriptive monthly station-balanced slope."
    )
  }
}

if (nrow(nmemc_fujian$data) > 0L) {
  s <- descriptive_slope(nmemc_fujian$data$year, nmemc_fujian$data$median_class)
  trend_rows[[length(trend_rows) + 1L]] <- tibble(
    source = "NMEMC Fujian marine",
    series = "Marine water-quality class",
    time_unit = "year",
    slope_per_time_unit = s,
    direction = trend_direction(s),
    orientation = "lower_class_number_better",
    note = "Descriptive annual station-balanced class slope."
  )
}

if (nrow(cnemc_recent$data) > 0L) {
  for (prm in unique(cnemc_recent$data$parameter)) {
    z <- cnemc_recent$data %>% filter(parameter == prm)
    s <- descriptive_slope(as.numeric(z$date), z$median)
    orientation <- if (prm == "dissolved_oxygen_mg_l_raw") {
      "higher_often_better_context_dependent"
    } else {
      "lower_generally_better_for_pollution_pressure"
    }
    trend_rows[[length(trend_rows) + 1L]] <- tibble(
      source = "CNEMC Fujian recent",
      series = unique(z$parameter_label)[1],
      time_unit = "day",
      slope_per_time_unit = s,
      direction = trend_direction(s),
      orientation = orientation,
      note = "Short-term monitoring slope; not a long-term trend."
    )
  }
}

if (nrow(onlimo_recent$data) > 0L) {
  for (ws in unique(onlimo_recent$data$watershed)) {
    z <- onlimo_recent$data %>% filter(watershed == ws)
    s <- descriptive_slope(as.numeric(z$date), z$median_ip)
    trend_rows[[length(trend_rows) + 1L]] <- tibble(
      source = "ONLIMO daily recent",
      series = paste0("Pollution Index - ", ws),
      time_unit = "day",
      slope_per_time_unit = s,
      direction = trend_direction(s),
      orientation = "lower_better",
      note = "Short-term monitoring slope; not a long-term trend."
    )
  }
}

trend_summary <- if (length(trend_rows) > 0L) bind_rows(trend_rows) else tibble()
readr::write_csv(trend_summary, file.path(OUTPUT_DIR, "trend_direction_summary.csv"), na = "")

# Main historical/environmental overview -------------------------------------
overview <- (fujian_class$plot | nmemc_fujian$plot) /
  (fujian_params$plot | onlimo_hist$plot)

overview <- overview +
  plot_annotation(
    title = OVERVIEW_TITLE,
    subtitle = paste0(
      OVERVIEW_SUBTITLE,
      "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M"),
      " | Aggregation: ", SUMMARY_METHOD
    ),
    caption = paste(
      "Interpretation is descriptive. Sources retain their native metrics and temporal resolutions;",
      "no direct Xiamen-Jakarta composite index is imposed at this stage."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, colour = COLORS$title),
      plot.subtitle = element_text(size = 11, colour = COLORS$muted),
      plot.caption = element_text(size = 9, colour = COLORS$muted)
    )
  )

save_plot_pair(
  overview,
  "teliti_environmental_trends_overview",
  OVERVIEW_WIDTH,
  OVERVIEW_HEIGHT
)

# Recent-monitoring figure ----------------------------------------------------
recent <- cnemc_recent$plot | onlimo_recent$plot
recent <- recent +
  plot_annotation(
    title = RECENT_TITLE,
    subtitle = paste0(
      RECENT_SUBTITLE,
      "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M")
    ),
    caption = paste(
      "Recent high-frequency signals are intended for monitoring and hypothesis generation.",
      "They should not be treated as long-term trends until a longer independent record accumulates."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, colour = COLORS$title),
      plot.subtitle = element_text(size = 11, colour = COLORS$muted),
      plot.caption = element_text(size = 9, colour = COLORS$muted)
    )
  )

save_plot_pair(
  recent,
  "teliti_recent_monitoring_trends",
  RECENT_WIDTH,
  RECENT_HEIGHT
)

# Optional individual panels --------------------------------------------------
if (isTRUE(SAVE_INDIVIDUAL_PANELS)) {
  ggsave(file.path(OUTPUT_DIR, "panel_fujian_wq_class.png"), fujian_class$plot, width = 8.5, height = 5.2, dpi = OUTPUT_DPI, bg = "white")
  ggsave(file.path(OUTPUT_DIR, "panel_fujian_parameters.png"), fujian_params$plot, width = 9.5, height = 8.0, dpi = OUTPUT_DPI, bg = "white")
  onlimo_hist_height <- min(
    MAX_ONLIMO_DETAIL_HEIGHT,
    max(7.0, 2.25 * max(1L, onlimo_hist$n_plot_watersheds %||% onlimo_hist$n_watersheds))
  )
  onlimo_recent_height <- min(
    MAX_ONLIMO_DETAIL_HEIGHT,
    max(7.0, 2.10 * max(1L, onlimo_recent$n_plot_watersheds %||% onlimo_recent$n_watersheds))
  )

  ggsave(
    file.path(OUTPUT_DIR, "panel_onlimo_historical_ip.png"),
    onlimo_hist$plot_detail,
    width = 10.5,
    height = onlimo_hist_height,
    dpi = OUTPUT_DPI,
    bg = "white"
  )
  ggsave(file.path(OUTPUT_DIR, "panel_nmemc_fujian_marine.png"), nmemc_fujian$plot, width = 8.5, height = 5.2, dpi = OUTPUT_DPI, bg = "white")
  ggsave(file.path(OUTPUT_DIR, "panel_cnemc_fujian_recent.png"), cnemc_recent$plot, width = 8.5, height = 8.0, dpi = OUTPUT_DPI, bg = "white")
  ggsave(
    file.path(OUTPUT_DIR, "panel_onlimo_daily_recent.png"),
    onlimo_recent$plot_detail,
    width = 10.5,
    height = onlimo_recent_height,
    dpi = OUTPUT_DPI,
    bg = "white"
  )
}

# Console summary -------------------------------------------------------------
message("\n================ Teliti environmental trend dashboard ================")
message("Output directory: ", OUTPUT_DIR)
message("Fujian class annual rows:       ", nrow(fujian_class$data))
message("Fujian parameter summary rows:  ", nrow(fujian_params$data))
message("ONLIMO historical monthly rows: ", nrow(onlimo_hist$data))
message("ONLIMO historical watersheds:   ", onlimo_hist$n_watersheds, " total; ", onlimo_hist$n_plot_watersheds, " plotted")
message("ONLIMO recent watersheds:       ", onlimo_recent$n_watersheds, " total; ", onlimo_recent$n_plot_watersheds, " plotted")
message("NMEMC Fujian annual rows:       ", nrow(nmemc_fujian$data))
message("CNEMC recent summary rows:      ", nrow(cnemc_recent$data))
message("ONLIMO recent summary rows:     ", nrow(onlimo_recent$data))
message("\nCity-comparison readiness:")
print(city_readiness, n = Inf)
message("\nMain figure: ", file.path(OUTPUT_DIR, "teliti_environmental_trends_overview.png"))
message("Recent figure: ", file.path(OUTPUT_DIR, "teliti_recent_monitoring_trends.png"))
message("=======================================================================")

invisible(list(
  fujian_class = fujian_class$data,
  fujian_parameters = fujian_params$data,
  onlimo_historical = onlimo_hist$data,
  nmemc_fujian = nmemc_fujian$data,
  cnemc_recent = cnemc_recent$data,
  onlimo_recent = onlimo_recent$data,
  city_readiness = city_readiness
))