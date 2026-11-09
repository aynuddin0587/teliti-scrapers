# =============================================================================
# 17_plot_integrated_water_quality_report.R
# Teliti | Integrated, reproducible water-quality evidence visualisation
# =============================================================================
# PURPOSE
#   Six report sections, each saved as an independent, publication-quality
#   PNG and vector PDF, plus a six-page PDF for reporting. Individual panels,
#   descriptive CSV summaries, and a machine-readable figure manifest are also
#   exported. No collector, reconciliation script, or source archive is run or
#   modified by this script.
#
# REQUIRED UPSTREAM STEP
#   12_build_analysis_datasets.R (following reconciliation scripts 05-08).
#
# SECTIONS
#   01 Evidence inventory, coverage, and station-location maps
#   02 CNEMC surface water (latest canonical observations from script 12)
#   03 NMEMC marine (reconciled through script 06)
#   04 ONLIMO daily (reconciled through script 05)
#   05 ONLIMO historical Pollution Index (reconciled through script 08)
#   06 Fujian weekly surface water (reconciled through script 07)
#
# RUN (Windows / RStudio)
#   Rscript "D:/# R Project/penelitian/teliti_reconciliation/script/17_plot_integrated_water_quality_report.R"
#   source("teliti_reconciliation/script/17_plot_integrated_water_quality_report.R")
#
# OPTIONAL ENVIRONMENT OVERRIDES
#   TELITI_DATA_ROOT       (default D:/# R Project/penelitian)
#   TELITI_ANALYSIS_DIR    (default <root>/teliti_reconciliation/analysis)
#   TELITI_VIZ_OUTPUT_DIR  (default <root>/teliti_reconciliation/output/17_water_quality_report)
#
# INTERPRETATION SAFEGUARDS
#   * These datasets have different native temporal/spatial coverage.
#   * Monitoring sites and dates are not necessarily representative samples.
#   * River/sea WQ classes and Indonesia's Pollution Index are NOT equivalent.
#   * Source values with censoring signs (<, <=, >, >=, <=/>= unicode)
#     and impossible negative concentrations are omitted from numeric medians;
#     omission counts are exported.
#   * First aggregate within each station/period, then across stations, so
#     high-frequency monitoring does not automatically dominate a summary.
#   * A missing source produces labelled no-data panels, never invented data.
# =============================================================================

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 01 | Configuration
# -----------------------------------------------------------------------------
PROJECT_DIR <- Sys.getenv("TELITI_DATA_ROOT", unset = "D:/# R Project/penelitian")
ANALYSIS_DIR <- Sys.getenv("TELITI_ANALYSIS_DIR", unset = file.path(
  PROJECT_DIR, "teliti_reconciliation", "analysis"
))
OUTPUT_DIR <- Sys.getenv("TELITI_VIZ_OUTPUT_DIR", unset = file.path(
  PROJECT_DIR, "teliti_reconciliation", "output", "17_water_quality_report"
))
PANEL_DIR <- file.path(OUTPUT_DIR, "panels")
TABLE_DIR <- file.path(OUTPUT_DIR, "tables")

OUTPUT_DPI <- 320L
REPORT_WIDTH <- 16
REPORT_HEIGHT <- 11.3
PANEL_WIDTH <- 9.2
PANEL_HEIGHT <- 5.8
SAVE_PANELS <- TRUE
SAVE_COMBINED_PDF <- TRUE
CNEMC_RECENT_DAYS <- 90L
ONLIMO_RECENT_DAYS <- 90L
CNEMC_MONTHS <- 18L
# "fujian" is suitable for the Xiamen context; "national" plots all CNEMC rows.
CNEMC_SCOPE <- "fujian"       # "fujian" or "national"
NMEMC_SCOPE <- "fujian"       # "fujian" or "national"
MAX_FOCUS_WATERSHEDS <- 5L

FUJIAN_REGEX <- "福建|Fujian"
XIAMEN_REGEX <- "厦门|Xiamen"
FOCUS_WATERSHEDS <- c("Ciliwung", "Cisadane", "Citarum", "Bengawan Solo", "Musi")

DATASETS <- data.frame(
  key = c("cnemc", "nmemc", "onlimo_daily", "onlimo_historical", "fujian"),
  label = c("CNEMC surface water", "NMEMC marine", "ONLIMO daily",
            "ONLIMO historical IP", "Fujian weekly"),
  native_resolution = c("High frequency", "Periodic / monthly", "Daily", "Daily IP", "Weekly"),
  file = file.path(ANALYSIS_DIR, c(
    "cnemc_latest_analysis.rds", "nmemc_marine_analysis.rds",
    "onlimo_daily_analysis.rds", "onlimo_historical_analysis.rds",
    "fujian_weekly_analysis.rds"
  )),
  stringsAsFactors = FALSE
)
STATION_FILE <- file.path(ANALYSIS_DIR, "station_metadata.rds")
if (!CNEMC_SCOPE %in% c("fujian", "national") || !NMEMC_SCOPE %in% c("fujian", "national")) {
  stop("CNEMC_SCOPE and NMEMC_SCOPE must be 'fujian' or 'national'", call. = FALSE)
}

DATA_COLORS <- c(
  "CNEMC surface water" = "#D55E00",
  "NMEMC marine" = "#0072B2",
  "ONLIMO daily" = "#CC79A7",
  "ONLIMO historical IP" = "#E69F00",
  "Fujian weekly" = "#009E73"
)
NETWORK_COLORS <- c(
  "CNEMC surface water" = "#D55E00",
  "NMEMC marine" = "#0072B2",
  "ONLIMO" = "#CC79A7",
  "Fujian weekly" = "#009E73"
)
WATERSHED_COLORS <- c(
  "Ciliwung" = "#0072B2", "Cisadane" = "#009E73",
  "Citarum" = "#D55E00", "Bengawan Solo" = "#CC79A7", "Musi" = "#E69F00"
)
COL <- list(title = "#153E75", text = "#202C3A", muted = "#627083",
            grid = "#E6EBF0", ribbon = "#9AC9E3", pale = "#F3F6F9",
            line = "#15679A", marine = "#087EAB", fujian = "#008568",
            indonesia = "#9A4D93")

required_packages <- c("dplyr", "tidyr", "readr", "tibble", "ggplot2",
                       "patchwork", "scales", "maps")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages) > 0L) stop(
  "Missing R packages: ", paste(missing_packages, collapse = ", "),
  "\nInstall once using install.packages(c(",
  paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
  call. = FALSE
)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

invisible(lapply(c(OUTPUT_DIR, PANEL_DIR, TABLE_DIR), dir.create,
                 recursive = TRUE, showWarnings = FALSE))
RUN_AT <- format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")
FIGURES <- list()

# -----------------------------------------------------------------------------
# 02 | General-purpose helpers
# -----------------------------------------------------------------------------
log_msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " | ", paste0(..., collapse = ""))

get_col <- function(dat, choices, default = NA_character_) {
  hit <- choices[choices %in% names(dat)]
  if (length(hit)) return(dat[[hit[[1L]]]])
  rep(default, nrow(dat))
}

clean_text <- function(x) {
  y <- trimws(as.character(x))
  y[is.na(y) | !nzchar(y) | tolower(y) %in% c("na", "nan", "null", "n/a", "-")] <- NA_character_
  y
}

as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "Asia/Shanghai"))
  z <- clean_text(x)
  out <- suppressWarnings(as.Date(substr(z, 1L, 10L), format = "%Y-%m-%d"))
  needed <- is.na(out) & !is.na(z)
  if (any(needed)) out[needed] <- suppressWarnings(as.Date(z[needed], format = "%Y/%m/%d"))
  out
}

numeric_strict <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  z <- clean_text(x)
  # Do not parse inequality thresholds as measured values or arbitrary HTML.
  valid <- !is.na(z) & grepl(
    "^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", z
  )
  out <- rep(NA_real_, length(z))
  out[valid] <- suppressWarnings(as.numeric(z[valid]))
  out
}

is_censored <- function(x) {
  z <- clean_text(x)
  !is.na(z) & grepl("^(<|>|≤|≥|＝|~=)", z)
}

q_safe <- function(x, p) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  unname(as.numeric(stats::quantile(z, p, names = FALSE)))
}

median_safe <- function(x) {
  z <- x[is.finite(x)]
  if (!length(z)) return(NA_real_)
  stats::median(z)
}

nice_n <- function(x) scales::comma(x, accuracy = 1)

read_canonical <- function(key) {
  path <- DATASETS$file[match(key, DATASETS$key)]
  if (!length(path) || !file.exists(path)) {
    log_msg("MISSING input for ", key, ": ", path)
    return(NULL)
  }
  x <- tryCatch(readRDS(path), error = function(e) {
    warning("Could not read ", path, ": ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(x)) return(NULL)
  if (!inherits(x, "data.frame")) {
    warning("Canonical input is not a data.frame: ", path, call. = FALSE)
    return(NULL)
  }
  tibble::as_tibble(x)
}

get_year <- function(dat, key) {
  if (key == "fujian") return(suppressWarnings(as.integer(get_col(dat, "year"))))
  if (key == "nmemc") return(suppressWarnings(as.integer(get_col(dat, "source_year"))))
  d <- get_date(dat, key)
  suppressWarnings(as.integer(format(d, "%Y")))
}

get_date <- function(dat, key) {
  field <- if (key == "cnemc") "observation_datetime" else if (key == "fujian") {
    "report_period_end"
  } else "date"
  d <- as_date_safe(get_col(dat, field))
  if (key == "fujian") {
    fallback <- suppressWarnings(as.integer(get_col(dat, "year")))
    missing <- is.na(d) & !is.na(fallback) & fallback >= 1900 & fallback <= 2100
    if (any(missing)) d[missing] <- as.Date(sprintf("%04d-01-01", fallback[missing]))
  }
  d
}

get_station <- function(dat, key) {
  if (key == "cnemc") {
    key_val <- clean_text(get_col(dat, c("analysis_station_key", "station_key")))
    area <- clean_text(get_col(dat, c("area_cn", "area")))
    section <- clean_text(get_col(dat, c("monitoring_section_cn", "monitoring_section")))
    fallback <- ifelse(!is.na(area) & !is.na(section), paste(area, section, sep = "|"), NA_character_)
    key_val[is.na(key_val)] <- fallback[is.na(key_val)]
    return(key_val)
  }
  if (key == "nmemc") {
    site <- clean_text(get_col(dat, c("site_code", "site")))
    province <- clean_text(get_col(dat, "province"))
    return(ifelse(!is.na(site), paste(ifelse(is.na(province), "Unknown", province), site, sep = "|"), NA_character_))
  }
  if (key == "fujian") return(clean_text(get_col(dat, c("station_name_key", "station_name_canonical", "station_name"))))
  clean_text(get_col(dat, "station_id"))
}

valid_station <- function(x) !is.na(x) & nzchar(x)

base_theme <- function(sz = 10.5) {
  theme_minimal(base_size = sz) +
    theme(
      text = element_text(family = "sans", colour = COL$text),
      plot.title = element_text(face = "bold", colour = COL$title, size = sz + 2),
      plot.subtitle = element_text(colour = COL$muted, size = sz - 0.5),
      plot.caption = element_text(colour = COL$muted, size = sz - 1.5),
      axis.title = element_text(face = "bold"),
      panel.grid.major = element_line(colour = COL$grid, linewidth = 0.27),
      panel.grid.minor = element_blank(),
      legend.position = "bottom", legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = sz - 0.4),
      strip.background = element_rect(fill = COL$pale, colour = NA),
      plot.margin = margin(7, 9, 7, 9)
    )
}

blank_plot <- function(title, details = "No usable observations in this canonical input") {
  ggplot() +
    annotate("text", x = 0.5, y = 0.61, label = title, fontface = "bold",
             size = 5.3, colour = COL$title) +
    annotate("text", x = 0.5, y = 0.40,
             label = paste(strwrap(details, width = 68), collapse = "\n"),
             size = 3.55, colour = COL$muted) +
    xlim(0, 1) + ylim(0, 1) + theme_void()
}

line_ready <- function(dat, group = NULL, x = "date") {
  if (!nrow(dat)) return(dat)
  if (is.null(group)) return(if (dplyr::n_distinct(dat[[x]]) >= 2) dat else dat[0, , drop = FALSE])
  dat %>% group_by(across(all_of(group))) %>%
    filter(n_distinct(.data[[x]]) >= 2L) %>% ungroup()
}

wrap_caption <- function(text, width = 138L) {
  paste(strwrap(text, width = width), collapse = "\n")
}

section_page <- function(plots, title, subtitle, caption, top_fraction = 0.94) {
  # Layout: first element across top; remaining elements in second row.
  if (length(plots) == 1L) {
    assembled <- plots[[1L]]
  } else if (length(plots) == 2L) {
    assembled <- plots[[1L]] / plots[[2L]] +
      plot_layout(heights = c(top_fraction, 1.0))
  } else {
    lower <- Reduce(`|`, plots[-1L])
    assembled <- plots[[1L]] / lower +
      plot_layout(heights = c(top_fraction, 1.0))
  }
  assembled + plot_annotation(
    title = title, subtitle = subtitle, caption = wrap_caption(caption),
    theme = theme(
      plot.title = element_text(face = "bold", size = 19, colour = COL$title,
                                margin = margin(b = 6)),
      plot.subtitle = element_text(size = 10.5, colour = COL$muted,
                                   margin = margin(b = 12)),
      plot.caption = element_text(size = 8.8, colour = COL$muted, hjust = 0,
                                  margin = margin(t = 11)),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )
}

save_pair <- function(plot, stem, folder = OUTPUT_DIR,
                      width = REPORT_WIDTH, height = REPORT_HEIGHT,
                      section = "", kind = "page") {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  png_name <- file.path(folder, paste0(stem, ".png"))
  pdf_name <- file.path(folder, paste0(stem, ".pdf"))
  ggsave(png_name, plot = plot, width = width, height = height,
         units = "in", dpi = OUTPUT_DPI, bg = "white", limitsize = FALSE)
  ggsave(pdf_name, plot = plot, width = width, height = height,
         units = "in", device = "pdf", bg = "white", limitsize = FALSE)
  FIGURES[[length(FIGURES) + 1L]] <<- tibble(
    section = section, kind = kind, figure_id = stem,
    png = normalizePath(png_name, winslash = "/", mustWork = TRUE),
    pdf = normalizePath(pdf_name, winslash = "/", mustWork = TRUE),
    width_in = width, height_in = height, dpi = OUTPUT_DPI
  )
  invisible(c(png = png_name, pdf = pdf_name))
}

write_table <- function(x, stem) {
  if (is.null(x) || !inherits(x, "data.frame") || !ncol(x)) return(invisible(NULL))
  readr::write_csv(x, file.path(TABLE_DIR, paste0(stem, ".csv")), na = "")
}

# Return one station-balanced median per group and period, with across-station IQR.
station_balanced <- function(dat, period_col = "date") {
  if (is.null(dat) || !nrow(dat) || !all(c(period_col, "station", "parameter", "value") %in% names(dat))) {
    return(tibble())
  }
  y <- dat %>%
    filter(!is.na(.data[[period_col]]), valid_station(station), is.finite(value)) %>%
    group_by(period = .data[[period_col]], parameter, station) %>%
    summarise(station_median = median(value), n_samples = n(), .groups = "drop")
  if (!nrow(y)) return(tibble())
  y %>% group_by(period, parameter) %>%
    summarise(median = median(station_median),
              q25 = q_safe(station_median, 0.25),
              q75 = q_safe(station_median, 0.75),
              n_stations = n_distinct(station),
              n_source_values = sum(n_samples), .groups = "drop")
}

# Numeric data are from Script 12's parsed fields where available. Otherwise
# parse only plain, unqualified numbers. Censored raw/qualifier rows are NA.
parameter_long <- function(dat, spec, dates, station) {
  if (is.null(dat) || !nrow(dat)) return(tibble())
  pieces <- lapply(seq_len(nrow(spec)), function(i) {
    item <- spec[i, ]
    value_col <- item$value_col[[1L]]
    raw_col <- item$raw_col[[1L]]
    if (!value_col %in% names(dat) && !raw_col %in% names(dat)) return(NULL)
    raw <- if (raw_col %in% names(dat)) dat[[raw_col]] else rep(NA_character_, nrow(dat))
    parsed <- if (value_col %in% names(dat)) numeric_strict(dat[[value_col]]) else numeric_strict(raw)
    qual_col <- paste0(value_col, "_qualifier")
    qualifier <- if (qual_col %in% names(dat)) clean_text(dat[[qual_col]]) else rep(NA_character_, nrow(dat))
    censored <- is_censored(raw) | (!is.na(qualifier) & qualifier %in% c("<", "<=", ">", ">="))
    invalid_negative <- is.finite(parsed) & parsed < 0
    parsed[censored | invalid_negative] <- NA_real_
    tibble(date = dates, year = as.integer(format(dates, "%Y")),
           station = station, parameter = item$parameter[[1L]],
           value = parsed, censored = censored,
           invalid_negative = invalid_negative,
           numeric_available = is.finite(parsed))
  })
  bind_rows(pieces)
}

parameter_quality <- function(long) {
  if (!nrow(long)) return(tibble())
  long %>% group_by(parameter) %>% summarise(
    rows_checked = n(), numeric_rows = sum(numeric_available),
    censored_rows = sum(censored),
    physically_invalid_negative_rows = sum(invalid_negative),
    numeric_pct = 100 * mean(numeric_available),
    stations_numeric = n_distinct(station[numeric_available & valid_station(station)]),
    .groups = "drop"
  )
}

# Stand-alone concise class decoder; no source text is overwritten.
class_score <- function(x) {
  parse_one <- function(y) {
    if (is.na(y)) return(NA_integer_)
    z <- toupper(trimws(as.character(y)))
    if (!nzchar(z)) return(NA_integer_)
    if (grepl("劣", z, fixed = TRUE)) return(6L)
    z <- gsub("水质|水質|海水|类别|類別|CLASS|GRADE|类|類|级|級|[[:space:]]", "", z, perl = TRUE)
    parts <- strsplit(z, "[-~/～至]", perl = TRUE)[[1L]]
    one <- function(v) {
      dict <- c("Ⅰ" = 1L, "Ⅱ" = 2L, "Ⅲ" = 3L, "Ⅳ" = 4L, "Ⅴ" = 5L,
                "Ⅵ" = 6L, "I" = 1L, "II" = 2L, "III" = 3L,
                "IV" = 4L, "V" = 5L, "VI" = 6L,
                "一" = 1L, "二" = 2L, "三" = 3L, "四" = 4L,
                "五" = 5L, "六" = 6L,
                "1" = 1L, "2" = 2L, "3" = 3L, "4" = 4L, "5" = 5L, "6" = 6L)
      ans <- unname(dict[v])
      if (!length(ans) || is.na(ans)) NA_integer_ else as.integer(ans)
    }
    vals <- vapply(parts, one, integer(1))
    if (all(is.na(vals))) NA_integer_ else max(vals, na.rm = TRUE)
  }
  vapply(clean_text(x), parse_one, integer(1))
}

focus_scope <- function(dat, field = "watershed") {
  if (is.null(dat) || !nrow(dat) || !field %in% names(dat)) return(character())
  z <- clean_text(dat[[field]])
  counts <- sort(table(z[!is.na(z)]), decreasing = TRUE)
  eligible <- names(counts)
  pick <- FOCUS_WATERSHEDS[tolower(FOCUS_WATERSHEDS) %in% tolower(eligible)]
  if (length(pick)) return(pick)
  head(eligible, MAX_FOCUS_WATERSHEDS)
}

canonical_watershed <- function(x) {
  z <- clean_text(x)
  hit <- match(tolower(z), tolower(FOCUS_WATERSHEDS))
  matched <- !is.na(hit)
  z[matched] <- FOCUS_WATERSHEDS[hit[matched]]
  z
}

watershed_palette <- function(labels) {
  x <- unique(as.character(labels))
  x <- x[!is.na(x)]
  colors <- setNames(grDevices::hcl.colors(length(x), palette = "Dark 3"), x)
  overlap <- intersect(x, names(WATERSHED_COLORS))
  colors[overlap] <- WATERSHED_COLORS[overlap]
  colors
}

period_breaks <- function(dates) {
  dates <- dates[!is.na(dates)]
  if (!length(dates)) return("1 month")
  span <- as.numeric(max(dates) - min(dates))
  if (span <= 180) "1 month" else if (span <= 800) "3 months" else "1 year"
}

# -----------------------------------------------------------------------------
# 03 | Section 01: evidence inventory and geographic footprint
# -----------------------------------------------------------------------------
profile_one <- function(dat, key) {
  lookup <- DATASETS[DATASETS$key == key, ]
  if (is.null(dat)) {
    return(list(
      profile = tibble(dataset = lookup$label, key = key, status = "MISSING",
                       observations = NA_real_, stations = NA_real_,
                       first_year = NA_integer_, last_year = NA_integer_,
                       years_observed = NA_integer_, source_file = lookup$file),
      annual = tibble()
    ))
  }
  yr <- get_year(dat, key)
  station <- get_station(dat, key)
  valid_year <- is.finite(yr) & yr >= 1900 & yr <= as.integer(format(Sys.Date(), "%Y")) + 1L
  annual <- tibble(dataset = rep(lookup$label, sum(valid_year)),
                   year = yr[valid_year]) %>%
    count(dataset, year, name = "observations")
  yrs <- unique(yr[valid_year])
  list(
    profile = tibble(
      dataset = lookup$label, key = key, status = if (nrow(dat)) "AVAILABLE" else "EMPTY",
      observations = nrow(dat), stations = n_distinct(station[valid_station(station)]),
      first_year = if (length(yrs)) min(yrs) else NA_integer_,
      last_year = if (length(yrs)) max(yrs) else NA_integer_,
      years_observed = length(yrs), source_file = lookup$file
    ),
    annual = annual
  )
}

load_stations <- function() {
  if (!file.exists(STATION_FILE)) {
    log_msg("Station metadata missing; all site maps will display basemap only: ", STATION_FILE)
    return(tibble())
  }
  x <- tryCatch(readRDS(STATION_FILE), error = function(e) {
    warning("Could not read station metadata: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(x) || !inherits(x, "data.frame") ||
      !all(c("network", "longitude", "latitude") %in% names(x))) return(tibble())
  x <- tibble::as_tibble(x)
  x$network <- as.character(x$network)
  x$longitude <- numeric_strict(x$longitude)
  x$latitude <- numeric_strict(x$latitude)
  x$coordinate_valid <- is.finite(x$longitude) & is.finite(x$latitude) &
    between(x$longitude, -180, 180) & between(x$latitude, -90, 90)
  x
}

map_panel <- function(stations, world, box, city, city_lon, city_lat, title,
                      networks = names(NETWORK_COLORS)) {
  x <- if (nrow(stations)) stations %>%
    filter(coordinate_valid, network %in% networks,
           between(longitude, box[[1L]], box[[2L]]),
           between(latitude, box[[3L]], box[[4L]])) else tibble()
  network_key <- if (identical(networks, "ONLIMO")) {
    "ONLIMO pink"
  } else {
    "CNEMC orange | NMEMC blue | Fujian green"
  }
  p <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "#F4F1EA", colour = "#8F979E", linewidth = 0.24) +
    coord_quickmap(xlim = box[1:2], ylim = box[3:4], expand = FALSE) +
    labs(title = title, subtitle = paste0(nice_n(nrow(x)), " mapped sites | ", network_key),
         x = NULL, y = NULL) +
    base_theme(10.2) +
    theme(axis.text = element_text(size = 8), legend.position = "none",
          panel.grid.major = element_line(colour = "#D9E2EA", linewidth = 0.18))
  if (nrow(x)) {
    p <- p + geom_point(
      data = x, aes(x = longitude, y = latitude, colour = network, shape = network),
      size = if (grepl("Fujian", title)) 1.65 else 1.12,
      alpha = 0.70, stroke = 0.30
    ) +
      scale_colour_manual(values = NETWORK_COLORS, drop = TRUE) +
      scale_shape_manual(values = c("CNEMC surface water" = 16, "NMEMC marine" = 15,
                                    "ONLIMO" = 18, "Fujian weekly" = 17), drop = TRUE)
  }
  p +
    annotate("point", x = city_lon, y = city_lat, shape = 4,
             size = 3.4, stroke = 1.1, colour = COL$title) +
    annotate("text", x = city_lon, y = city_lat, label = city,
             hjust = -0.08, vjust = -0.7, colour = COL$title,
             fontface = "bold", size = 3.25)
}

build_overview <- function() {
  log_msg("Section 01: dataset inventory and maps")
  profiles <- vector("list", nrow(DATASETS))
  for (i in seq_len(nrow(DATASETS))) {
    key <- DATASETS$key[[i]]
    dat <- read_canonical(key)
    profiles[[i]] <- profile_one(dat, key)
    rm(dat)
    invisible(gc(verbose = FALSE))
  }
  inv <- bind_rows(lapply(profiles, `[[`, "profile"))
  counts <- bind_rows(lapply(profiles, `[[`, "annual"))
  write_table(inv, "01_dataset_inventory")
  write_table(counts, "01_annual_observation_counts")

  observed_years <- counts$year[is.finite(counts$year)]
  if (length(observed_years)) {
    yrseq <- seq.int(min(observed_years), max(observed_years))
    grid <- tidyr::expand_grid(dataset = DATASETS$label, year = yrseq) %>%
      left_join(counts, by = c("dataset", "year")) %>%
      mutate(observations = coalesce(observations, 0L),
             dataset = factor(dataset, levels = rev(DATASETS$label)))
    p_year <- ggplot(grid, aes(year, dataset, fill = log10(observations + 1))) +
      geom_tile(colour = "white", linewidth = 0.45) +
      scale_fill_gradient(low = COL$pale, high = COL$line,
                          name = "log10(n + 1)") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "A. Years with reported observations",
           subtitle = "Blank-coloured tiles indicate zero archived observations in that year",
           x = "Calendar year", y = NULL) + base_theme(10.2)
  } else {
    p_year <- blank_plot("A. Annual data coverage", "No canonical date information available")
  }

  display_inv <- inv %>% mutate(
    dataset = factor(dataset, levels = rev(DATASETS$label)),
    value = coalesce(stations, 0),
    label = ifelse(is.na(stations), "missing", nice_n(stations))
  )
  p_sites <- ggplot(display_inv, aes(x = value, y = dataset, fill = dataset)) +
    geom_col(width = 0.62, show.legend = FALSE) +
    geom_text(aes(label = label), hjust = -0.10, size = 3.25) +
    scale_fill_manual(values = DATA_COLORS, drop = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0.00, 0.27))) +
    labs(title = "B. Sites represented in observations",
         subtitle = "Unique station identities in each canonical dataset",
         x = "Number of distinct stations", y = NULL) +
    base_theme(10.2) + theme(panel.grid.major.y = element_blank())

  stations <- load_stations()
  network_audit <- if (nrow(stations)) stations %>%
    group_by(network) %>%
    summarise(stations_in_metadata = n(), with_coordinates = sum(coordinate_valid),
              coordinate_pct = 100 * mean(coordinate_valid), .groups = "drop") else tibble()
  write_table(network_audit, "01_station_coordinate_coverage")

  world <- ggplot2::map_data("world")
  p_china <- map_panel(stations, world, c(73, 135, 18, 54),
                       "Xiamen", 118.0894, 24.4798, "C. China | national footprint",
                       c("CNEMC surface water", "NMEMC marine", "Fujian weekly"))
  p_fujian <- map_panel(stations, world, c(116, 121.5, 22.6, 28.7),
                        "Xiamen", 118.0894, 24.4798, "D. Fujian | coastal-inland focus",
                        c("CNEMC surface water", "NMEMC marine", "Fujian weekly"))
  p_indonesia <- map_panel(stations, world, c(94, 142, -12, 7),
                           "Jakarta", 106.8456, -6.2088, "E. Indonesia | ONLIMO network", "ONLIMO")

  page <- ((p_year | p_sites) / (p_china | p_fujian | p_indonesia)) +
    plot_layout(heights = c(0.87, 1.32)) +
    plot_annotation(
      title = "01  |  Evidence holdings and monitoring footprint",
      subtitle = "Five canonical products | distinct archive scopes, time bases, and monitoring designs",
      caption = wrap_caption(paste(
        "Site dots = stations with valid coordinates in station_metadata.rds; X = reference city centre, not a monitoring site.",
        "Network colours: CNEMC orange, NMEMC blue, Fujian weekly green, ONLIMO pink.",
        "Tile intensity reflects source-row volume and thus monitoring cadence; a recorded year does not imply continuous coverage."
      )),
      theme = theme(plot.title = element_text(face = "bold", size = 19, colour = COL$title),
                    plot.subtitle = element_text(size = 10.5, colour = COL$muted),
                    plot.caption = element_text(size = 8.7, colour = COL$muted,
                                                hjust = 0, margin = margin(t = 10)))
    )
  list(page = page, panels = list(
    coverage_by_year = p_year, sites_by_dataset = p_sites,
    map_china = p_china, map_fujian = p_fujian, map_indonesia = p_indonesia
  ), summary = inv)
}

# -----------------------------------------------------------------------------
# 04 | Generic figure builders used by source-specific sections
# -----------------------------------------------------------------------------
add_gap_segments <- function(dat, period_col = "period", groups = "parameter",
                             max_gap = 7) {
  if (is.null(dat) || !nrow(dat)) return(dat)
  dat %>% group_by(across(all_of(groups))) %>%
    arrange(.data[[period_col]], .by_group = TRUE) %>%
    mutate(.segment = cumsum(c(TRUE, diff(as.numeric(.data[[period_col]])) > max_gap))) %>%
    ungroup()
}

plot_medians <- function(summary, title, subtitle, x_is_date = FALSE,
                         line_color = COL$line, ncol = 3L) {
  if (!nrow(summary)) return(blank_plot(title, "No non-censored numeric measurements with valid dates and station IDs"))
  x_col <- "period"
  segmented <- add_gap_segments(summary, "period", "parameter",
                                 max_gap = if (x_is_date) 8 else 2)
  p <- ggplot(segmented, aes(x = .data[[x_col]], y = median)) +
    geom_ribbon(aes(ymin = q25, ymax = q75,
                    group = interaction(parameter, .segment)), fill = COL$ribbon,
                alpha = 0.29, na.rm = TRUE) +
    geom_line(data = line_ready(segmented, c("parameter", ".segment"), x_col),
              aes(group = interaction(parameter, .segment)), colour = line_color,
              linewidth = 0.8, na.rm = TRUE) +
    geom_point(size = 1.45, colour = line_color, na.rm = TRUE) +
    facet_wrap(~parameter, scales = "free_y", ncol = ncol) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = "Reported concentration (mg/L)") +
    base_theme(9.7) + theme(legend.position = "none")
  if (x_is_date) {
    p + scale_x_date(date_breaks = period_breaks(summary$period),
                     date_labels = "%b\n%Y")
  } else {
    p + scale_x_continuous(breaks = scales::pretty_breaks(n = 7))
  }
}

plot_station_counts <- function(counts, title, subtitle, x_is_date = FALSE,
                                line_color = COL$line) {
  if (!nrow(counts)) return(blank_plot(title, "No valid station-date combinations"))
  p <- ggplot(counts, aes(x = period, y = n_stations)) +
    geom_col(fill = line_color, width = if (x_is_date) 25 else 0.78, alpha = 0.8) +
    labs(title = title, subtitle = subtitle, x = NULL,
         y = "Distinct monitored stations") + base_theme(10)
  if (x_is_date) p + scale_x_date(date_breaks = period_breaks(counts$period),
                                   date_labels = "%b\n%Y") else
    p + scale_x_continuous(breaks = scales::pretty_breaks(n = 7))
}

plot_class_share <- function(dat, title, subtitle, class_cutoff = 3L,
                             x_is_date = TRUE, color = COL$line) {
  if (!nrow(dat)) return(blank_plot(title, "No usable published water-quality classes"))
  segmented <- add_gap_segments(dat, "period", character(),
                                 max_gap = if (x_is_date) 65 else 2)
  p <- ggplot(segmented, aes(x = period, y = median_share)) +
    geom_ribbon(aes(ymin = q25_share, ymax = q75_share, group = .segment),
                alpha = 0.28, fill = COL$ribbon, na.rm = TRUE) +
    geom_line(data = line_ready(segmented, ".segment", x = "period"),
              aes(group = .segment), colour = color,
              linewidth = 0.90, na.rm = TRUE) +
    geom_point(colour = color, size = 1.85, na.rm = TRUE) +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    labs(title = title, subtitle = subtitle, x = NULL,
         y = paste0("Station-balanced share in class I-", if (class_cutoff == 2L) "II" else "III", " (%)")) +
    base_theme(10)
  if (x_is_date) p + scale_x_date(date_breaks = period_breaks(dat$period),
                                   date_labels = "%b\n%Y") else
    p + scale_x_continuous(breaks = scales::pretty_breaks(n = 7))
}

summarise_class_share <- function(dat, period_col, cutoff = 3L) {
  if (!nrow(dat) || !all(c("station", "score", period_col) %in% names(dat))) return(tibble())
  y <- dat %>% filter(valid_station(station), !is.na(score), !is.na(.data[[period_col]])) %>%
    group_by(station, period = .data[[period_col]]) %>%
    summarise(share = 100 * mean(score <= cutoff), n_samples = n(), .groups = "drop")
  if (!nrow(y)) return(tibble())
  y %>% group_by(period) %>% summarise(
    median_share = median(share), q25_share = q_safe(share, 0.25),
    q75_share = q_safe(share, 0.75), n_stations = n_distinct(station),
    n_samples = sum(n_samples), .groups = "drop"
  )
}

# -----------------------------------------------------------------------------
# 05 | Section 02: CNEMC latest canonical surface-water observations
# -----------------------------------------------------------------------------
build_cnemc <- function() {
  log_msg("Section 02: CNEMC surface water")
  dat <- read_canonical("cnemc")
  if (is.null(dat) || !nrow(dat)) {
    empty <- blank_plot("CNEMC surface-water monitoring", "cnemc_latest_analysis.rds is absent or empty; run script 12")
    return(list(page = section_page(list(empty), "02  |  CNEMC automatic surface-water monitoring",
                                    "Unavailable canonical input", "No outputs were inferred for missing data"),
                panels = list(data_unavailable = empty)))
  }
  dat$date <- get_date(dat, "cnemc")
  dat$station <- get_station(dat, "cnemc")
  area <- clean_text(get_col(dat, c("area_cn", "area", "province")))
  fujian <- !is.na(area) & grepl(FUJIAN_REGEX, area, ignore.case = TRUE)
  local_scope <- identical(CNEMC_SCOPE, "fujian") && any(fujian)
  if (local_scope) dat <- dat[fujian, , drop = FALSE]
  location <- if (local_scope) "Fujian Province" else if (CNEMC_SCOPE == "national") {
    "national CNEMC network (configured scope)"
  } else "national network (Fujian labels not found)"
  dat <- dat %>% filter(!is.na(date), valid_station(station))
  if (!nrow(dat)) {
    empty <- blank_plot("CNEMC", "No dated station observations remained after identity and date checks")
    return(list(page = section_page(list(empty), "02  |  CNEMC automatic surface-water monitoring",
                                    location, "No numeric values were invented"),
                panels = list(data_unavailable = empty)))
  }
  latest <- max(dat$date)
  recent <- dat %>% filter(date >= latest - (CNEMC_RECENT_DAYS - 1L))

  spec <- tibble(
    parameter = c("Dissolved oxygen", "Ammonia-N", "Total phosphorus"),
    value_col = c("dissolved_oxygen_mg_l", "ammonia_nitrogen_mg_l", "total_phosphorus_mg_l"),
    raw_col = c("dissolved_oxygen_mg_l_raw", "ammonia_nitrogen_mg_l_raw", "total_phosphorus_mg_l_raw")
  )
  long <- parameter_long(recent, spec, recent$date, recent$station)
  quality <- parameter_quality(long)
  measure_daily <- station_balanced(long, "date")
  write_table(quality, "02_cnemc_parameter_availability")
  write_table(measure_daily, "02_cnemc_recent_station_balanced_daily")
  p_recent <- plot_medians(
    measure_daily, paste0("A. Recent ", if (local_scope) "Fujian" else "national", " CNEMC chemistry"),
    paste0("Last ", CNEMC_RECENT_DAYS, " days before ", latest,
           " | station-day medians, then cross-station median and IQR"),
    x_is_date = TRUE, line_color = DATA_COLORS[["CNEMC surface water"]]
  )

  # Only classes from the canonical latest-by-observation-key view are used.
  raw_rank <- if ("water_quality_class_rank" %in% names(dat)) {
    numeric_strict(dat$water_quality_class_rank)
  } else if ("water_quality_class_code" %in% names(dat)) {
    numeric_strict(dat$water_quality_class_code)
  } else {
    as.numeric(class_score(get_col(dat, c("water_quality_class_clean", "water_quality_class"))))
  }
  raw_rank[!is.finite(raw_rank) | !raw_rank %in% 1:6] <- NA_real_
  classes <- tibble(
    station = dat$station, date = dat$date, score = raw_rank,
    month = as.Date(format(dat$date, "%Y-%m-01"))
  ) %>% filter(date >= latest - (31L * CNEMC_MONTHS))
  class_month <- summarise_class_share(classes, "month", cutoff = 3L)
  write_table(class_month, "02_cnemc_monthly_class_I_III")
  p_class <- plot_class_share(
    class_month, "B. Published river classes I-III",
    "Monthly within-station share; median and IQR of station-level shares",
    x_is_date = TRUE, color = DATA_COLORS[["CNEMC surface water"]]
  )

  monthly_activity <- dat %>%
    mutate(period = as.Date(format(date, "%Y-%m-01"))) %>%
    filter(date >= latest - (31L * CNEMC_MONTHS)) %>%
    group_by(period) %>%
    summarise(n_stations = n_distinct(station), source_rows = n(), .groups = "drop")
  write_table(monthly_activity, "02_cnemc_monthly_monitoring_activity")
  p_activity <- plot_station_counts(
    monthly_activity, "C. Monitoring network represented",
    "Monthly unique sections (not data-collection frequency)",
    x_is_date = TRUE, line_color = DATA_COLORS[["CNEMC surface water"]]
  )

  scope <- tibble(
    dataset = "CNEMC surface water", location = location, last_observation = latest,
    stations_with_dates = n_distinct(dat$station), dated_rows = nrow(dat),
    latest_version_basis = "cnemc_latest_analysis.rds; versioned rows excluded"
  )
  write_table(scope, "02_cnemc_scope")
  page <- section_page(
    list(p_recent, p_class, p_activity),
    "02  |  CNEMC automatic surface-water monitoring",
    paste0(location, " | latest available observation ", latest, " | canonical latest row version"),
    paste(
      "High-frequency measurements summarised within station-days before across-station aggregation.",
      "River classes I-III are not directly comparable with NMEMC marine classes or ONLIMO IP.",
      "Censored measurement strings omitted from numerical medians; details in CSV."
    ), top_fraction = 1.06
  )
  list(page = page, panels = list(
    recent_parameters = p_recent, monthly_class_share = p_class,
    monthly_station_coverage = p_activity
  ))
}

# -----------------------------------------------------------------------------
# 06 | Section 03: NMEMC coastal/marine monitoring
# -----------------------------------------------------------------------------
build_nmemc <- function() {
  log_msg("Section 03: NMEMC marine")
  dat <- read_canonical("nmemc")
  if (is.null(dat) || !nrow(dat)) {
    empty <- blank_plot("NMEMC marine", "nmemc_marine_analysis.rds is absent or empty; run script 12")
    return(list(page = section_page(list(empty), "03  |  NMEMC coastal and marine water quality",
                                    "Unavailable canonical input", "No values were interpolated"),
                panels = list(data_unavailable = empty)))
  }
  dat$year <- suppressWarnings(as.integer(get_col(dat, "source_year")))
  dat$station <- get_station(dat, "nmemc")
  province <- clean_text(get_col(dat, "province"))
  is_fujian <- !is.na(province) & grepl(FUJIAN_REGEX, province, ignore.case = TRUE)
  use_fujian <- identical(NMEMC_SCOPE, "fujian") && any(is_fujian)
  study <- if (use_fujian) dat[is_fujian, , drop = FALSE] else dat
  study <- study %>% filter(is.finite(year), valid_station(station))
  location <- if (use_fujian) "Fujian coastal stations" else if (NMEMC_SCOPE == "national") {
    "All Chinese coastal stations (configured scope)"
  } else "All Chinese coastal stations (Fujian labels not found)"
  if (!nrow(study)) {
    empty <- blank_plot("NMEMC marine", "No station-year records with usable identities")
    return(list(page = section_page(list(empty), "03  |  NMEMC coastal and marine water quality",
                                    location, "No invented values"),
                panels = list(data_unavailable = empty)))
  }

  score <- class_score(get_col(study, "water_quality_class"))
  classes <- tibble(station = study$station, year = study$year, score = score)
  class_annual <- summarise_class_share(classes, "year", cutoff = 2L)
  write_table(class_annual, "03_nmemc_annual_class_I_II")
  p_class <- plot_class_share(
    class_annual, "A. Reported marine classes I-II",
    "Annual site-balanced percentage; IQR depicts between-site heterogeneity",
    class_cutoff = 2L, x_is_date = FALSE, color = COL$marine
  )

  # source_year is an annual grouping key, NOT a fabricated collection day.
  placeholder_date <- as.Date(sprintf("%04d-01-01", study$year))
  spec <- tibble(
    parameter = c("Dissolved oxygen", "COD", "Inorganic nitrogen", "Reactive phosphate"),
    value_col = c("dissolved_oxygen_mg_l_raw", "cod_mg_l_raw",
                  "inorganic_nitrogen_mg_l_raw", "reactive_phosphate_mg_l_raw"),
    raw_col = c("dissolved_oxygen_mg_l_raw", "cod_mg_l_raw",
                "inorganic_nitrogen_mg_l_raw", "reactive_phosphate_mg_l_raw")
  )
  long <- parameter_long(study, spec, placeholder_date, study$station)
  quality <- parameter_quality(long)
  annual <- station_balanced(long, "year")
  write_table(quality, "03_nmemc_parameter_availability")
  write_table(annual, "03_nmemc_annual_site_balanced_parameters")
  p_params <- plot_medians(
    annual, "B. Coastal chemistry by year",
    "Annual within-site median, then across-site median (IQR shaded)",
    x_is_date = FALSE, line_color = COL$marine, ncol = 2L
  )

  coverage <- dat %>%
    mutate(year = suppressWarnings(as.integer(get_col(dat, "source_year"))),
           station = get_station(dat, "nmemc")) %>%
    filter(is.finite(year), valid_station(station)) %>%
    group_by(year) %>% summarise(all_coastal_sites = n_distinct(station),
                                 .groups = "drop")
  fujian_coverage <- dat[is_fujian, , drop = FALSE] %>%
    mutate(year = suppressWarnings(as.integer(get_col(., "source_year"))),
           station = get_station(., "nmemc")) %>%
    filter(is.finite(year), valid_station(station)) %>%
    group_by(year) %>% summarise(fujian_sites = n_distinct(station), .groups = "drop")
  coverage <- coverage %>% left_join(fujian_coverage, by = "year") %>%
    mutate(fujian_sites = coalesce(fujian_sites, 0L))
  write_table(coverage, "03_nmemc_yearly_site_coverage")
  p_counts <- if (!nrow(coverage)) blank_plot("C. Marine site coverage") else
    ggplot(coverage, aes(x = year)) +
      geom_line(aes(y = all_coastal_sites, colour = "All coastal sites"), linewidth = 0.8) +
      geom_point(aes(y = all_coastal_sites, colour = "All coastal sites"), size = 1.7) +
      geom_line(data = line_ready(coverage, x = "year"),
                aes(y = fujian_sites, colour = "Fujian sites"), linewidth = 0.8) +
      geom_point(aes(y = fujian_sites, colour = "Fujian sites"), size = 1.7) +
      scale_colour_manual(values = c("All coastal sites" = COL$muted,
                                     "Fujian sites" = COL$marine)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
      labs(title = "C. Site availability", subtitle = "Annual unique coastal monitoring sites",
           x = "Source year", y = "Site count", colour = NULL) + base_theme(10)

  write_table(tibble(location = location, rows = nrow(study),
                     stations = n_distinct(study$station),
                     year_min = min(study$year), year_max = max(study$year)),
              "03_nmemc_scope")
  page <- section_page(
    list(p_params, p_class, p_counts),
    "03  |  NMEMC coastal and marine water quality",
    paste0(location, " | ", min(study$year), "-", max(study$year), " | native periodic observations grouped by source year"),
    paste(
      "Sea-water classes and river-water classes follow different assessment contexts.",
      "Annual grouping does not imply equal within-year sampling; non-detect thresholds are not treated as exact values."
    ), top_fraction = 1.09
  )
  list(page = page, panels = list(
    annual_parameters = p_params, annual_marine_class = p_class, annual_site_coverage = p_counts
  ))
}

# -----------------------------------------------------------------------------
# 07 | Section 04: ONLIMO recent daily concentrations and Pollution Index
# -----------------------------------------------------------------------------
build_onlimo_daily <- function() {
  log_msg("Section 04: ONLIMO daily")
  dat <- read_canonical("onlimo_daily")
  if (is.null(dat) || !nrow(dat)) {
    empty <- blank_plot("ONLIMO daily", "onlimo_daily_analysis.rds is absent or empty; run script 12")
    return(list(page = section_page(list(empty), "04  |  ONLIMO recent daily monitoring",
                                    "Unavailable canonical input", "No data were extrapolated"),
                panels = list(data_unavailable = empty)))
  }
  dat$date <- get_date(dat, "onlimo_daily")
  dat$station <- get_station(dat, "onlimo_daily")
  dat$watershed <- canonical_watershed(get_col(dat, "watershed"))
  dat <- dat %>% filter(!is.na(date), valid_station(station))
  if (!nrow(dat)) {
    empty <- blank_plot("ONLIMO daily", "No valid station-date records")
    return(list(page = section_page(list(empty), "04  |  ONLIMO recent daily monitoring",
                                    "No dated records", "No invented values"),
                panels = list(data_unavailable = empty)))
  }
  latest <- max(dat$date)
  recent <- dat %>% filter(date >= latest - (ONLIMO_RECENT_DAYS - 1L))
  focus <- focus_scope(recent)
  focus_recent <- recent %>% filter(watershed %in% focus)

  # The daily collector provides nine measured parameters. Pollution Index and
  # parameter-level indices are derived metrics, not additional measurements.
  param_fields <- c("ph", "do", "tds", "cod", "bod", "tss",
                    "nitrate", "ammonia", "temperature")
  param_names <- c("pH", "DO", "TDS", "COD", "BOD", "TSS",
                   "Nitrate", "Ammonia", "Temperature")
  completeness <- bind_rows(lapply(seq_along(param_fields), function(i) {
    field <- param_fields[[i]]
    z <- numeric_strict(get_col(dat, field))
    invalid_negative <- is.finite(z) & z < 0 & field != "temperature"
    available <- is.finite(z) & !invalid_negative
    tibble(parameter = param_names[[i]], field_present = field %in% names(dat),
           rows = nrow(dat), invalid_negative_rows = sum(invalid_negative),
           measured_rows = sum(available),
           measured_pct = 100 * mean(available),
           stations_measured = n_distinct(dat$station[available]))
  }))
  write_table(completeness, "04_onlimo_measured_parameter_availability")
  p_avail <- ggplot(completeness,
                    aes(x = measured_pct, y = reorder(parameter, measured_pct))) +
    geom_col(fill = DATA_COLORS[["ONLIMO daily"]], width = 0.66) +
    geom_text(aes(label = sprintf("%.0f%%", measured_pct)),
              hjust = -0.10, size = 3.3) +
    scale_x_continuous(limits = c(0, 109), breaks = c(0, 25, 50, 75, 100)) +
    labs(title = "B. Measured parameter availability",
         subtitle = "Percentage of archived station-days with a numeric measurement",
         x = "Observed data availability (%)", y = NULL) +
    base_theme(10) + theme(panel.grid.major.y = element_blank())

  ip <- recent %>% transmute(
    date, station, watershed, pollution_index = numeric_strict(get_col(recent, "pollution_index"))
  ) %>% filter(!is.na(watershed), is.finite(pollution_index), pollution_index >= 0)
  ip_station <- ip %>% group_by(date, station, watershed) %>%
    summarise(station_ip = median(pollution_index), .groups = "drop")
  ip_all <- ip_station %>% group_by(date, watershed) %>%
    summarise(median_ip = median(station_ip), q25 = q_safe(station_ip, 0.25),
              q75 = q_safe(station_ip, 0.75), n_stations = n_distinct(station),
              .groups = "drop")
  write_table(ip_all, "04_onlimo_daily_recent_IP_all_watersheds")
  ip_daily <- ip_all %>% filter(watershed %in% focus)
  write_table(ip_daily, "04_onlimo_daily_recent_IP_plotted_watersheds")
  ip_colors <- watershed_palette(focus)
  ip_plot <- add_gap_segments(ip_daily, "date", "watershed", max_gap = 8)
  p_ip <- if (!nrow(ip_daily)) {
    blank_plot("A. Recent daily Pollution Index", "No numeric IP for the selected watersheds")
  } else {
    ggplot(ip_plot, aes(x = date, y = median_ip, colour = watershed,
                          group = interaction(watershed, .segment))) +
      geom_hline(yintercept = c(1, 5, 10), linetype = "dashed",
                 colour = "#A7B2BD", linewidth = 0.32) +
      geom_line(data = line_ready(ip_plot, c("watershed", ".segment")),
                linewidth = 0.80, na.rm = TRUE) +
      geom_point(size = 1.25, na.rm = TRUE) +
      scale_colour_manual(values = ip_colors, drop = FALSE) +
      scale_x_date(date_breaks = period_breaks(ip_daily$date), date_labels = "%d %b\n%Y") +
      labs(title = "A. Short-window Pollution Index dynamics",
           subtitle = paste0("Station-balanced daily IP; latest archive date ", latest,
                             " | dashed reference levels: 1, 5, 10"),
           x = NULL, y = "Pollution Index (source-reported)", colour = "Watershed") +
      base_theme(10)
  }

  # Two watershed trajectories rather than an unlabelled pooled concentration.
  measured_focus <- head(focus, 2L)
  measured_dat <- focus_recent %>% filter(watershed %in% measured_focus)
  spec <- tibble(
    parameter = c("DO (mg/L)", "COD (mg/L)", "Ammonia (mg/L)"),
    value_col = c("do", "cod", "ammonia"),
    raw_col = c("do", "cod", "ammonia")
  )
  measured_long <- parameter_long(measured_dat, spec, measured_dat$date, measured_dat$station)
  # parameter_long emits a full station-date block for every present field;
  # attach watershed within each parameter block, including zero-row safety.
  measured_long$watershed <- rep(NA_character_, nrow(measured_long))
  if (nrow(measured_long)) {
    for (param in unique(measured_long$parameter)) {
      idx <- which(measured_long$parameter == param)
      if (length(idx) == nrow(measured_dat)) measured_long$watershed[idx] <- measured_dat$watershed
    }
  }
  groups <- split(measured_long, measured_long$watershed, drop = TRUE)
  measured_summary <- bind_rows(lapply(names(groups), function(w) {
    z <- station_balanced(groups[[w]], "date")
    if (nrow(z)) z$watershed <- w
    z
  }))
  write_table(measured_summary, "04_onlimo_daily_recent_measured_parameters")
  measured_plot <- add_gap_segments(measured_summary, "period", c("watershed", "parameter"), 8)
  p_measured <- if (!nrow(measured_summary)) {
    blank_plot("C. Measured chemical parameters", "No numeric DO, COD or ammonia data")
  } else {
    ggplot(measured_plot,
           aes(x = period, y = median, colour = watershed,
               group = interaction(watershed, .segment))) +
      geom_line(data = line_ready(measured_plot, c("watershed", "parameter", ".segment"), "period"),
                linewidth = 0.78, na.rm = TRUE) +
      geom_point(size = 0.95, na.rm = TRUE) +
      facet_wrap(~parameter, scales = "free_y", ncol = 1) +
      scale_colour_manual(values = watershed_palette(measured_summary$watershed)) +
      scale_x_date(date_breaks = period_breaks(measured_summary$period),
                   date_labels = "%d %b") +
      labs(title = "C. Recent measured chemistry",
           subtitle = "Within-station daily medians, then watershed median; up to two watersheds",
           x = NULL, y = "Concentration (mg/L)", colour = "Watershed") +
      base_theme(9.3)
  }

  write_table(tibble(
    last_observation = latest, archived_rows = nrow(dat),
    stations = n_distinct(dat$station), focus_watersheds = paste(focus, collapse = "; "),
    measured_parameters = paste(param_fields, collapse = "; "),
    interpretation = "Recent monitoring only; no extrapolation to historical baselines"
  ), "04_onlimo_daily_scope")
  page <- section_page(
    list(p_ip, p_avail, p_measured),
    "04  |  ONLIMO daily monitoring and parameter availability",
    paste0("Indonesia | ", ONLIMO_RECENT_DAYS, "-day descriptive window to ", latest,
           " | all archived station-days retained for availability analysis"),
    paste(
      "DO/COD/ammonia are measured parameters; IP is a derived index, not a concentration.",
      "Comparisons across different monitoring systems require matched parameters, stations, and sampling periods."
    ), top_fraction = 1.03
  )
  list(page = page, panels = list(
    recent_pollution_index = p_ip, parameter_availability = p_avail,
    recent_measured_chemistry = p_measured
  ))
}

# -----------------------------------------------------------------------------
# 08 | Section 05: ONLIMO historical station-level Pollution Index
# -----------------------------------------------------------------------------
build_onlimo_historical <- function() {
  log_msg("Section 05: ONLIMO historical Pollution Index")
  dat <- read_canonical("onlimo_historical")
  if (is.null(dat) || !nrow(dat)) {
    empty <- blank_plot("ONLIMO historical", "onlimo_historical_analysis.rds is absent or empty; run script 12")
    return(list(page = section_page(list(empty), "05  |  ONLIMO historical Pollution Index",
                                    "Unavailable canonical input", "No baseline was assumed"),
                panels = list(data_unavailable = empty)))
  }
  dat$date <- get_date(dat, "onlimo_historical")
  dat$station <- get_station(dat, "onlimo_historical")
  dat$watershed <- canonical_watershed(get_col(dat, "watershed"))
  dat$ip <- numeric_strict(get_col(dat, "pollution_index"))
  dat <- dat %>% filter(!is.na(date), valid_station(station), !is.na(watershed), is.finite(ip), ip >= 0)
  if (!nrow(dat)) {
    empty <- blank_plot("ONLIMO historical", "No valid dated station/IP records")
    return(list(page = section_page(list(empty), "05  |  ONLIMO historical Pollution Index",
                                    "No numeric IP values", "No values interpolated"),
                panels = list(data_unavailable = empty)))
  }

  dat$month <- as.Date(format(dat$date, "%Y-%m-01"))
  monthly_stations <- dat %>% group_by(month, watershed, station) %>%
    summarise(station_ip = median(ip), days = n_distinct(date), .groups = "drop")
  monthly <- monthly_stations %>% group_by(month, watershed) %>%
    summarise(median_ip = median(station_ip), q25 = q_safe(station_ip, 0.25),
              q75 = q_safe(station_ip, 0.75),
              n_stations = n_distinct(station), station_days = sum(days),
              .groups = "drop")
  write_table(monthly, "05_onlimo_historical_monthly_IP_all_watersheds")
  focus <- focus_scope(dat)
  display <- monthly %>% filter(watershed %in% focus)
  monthly_plot <- add_gap_segments(display, "month", "watershed", max_gap = 65)
  p_month <- ggplot(monthly_plot,
                    aes(x = month, y = median_ip, colour = watershed,
                        group = interaction(watershed, .segment))) +
    geom_hline(yintercept = c(1, 5, 10), linetype = "dashed",
               linewidth = 0.36, colour = "#AAB3BC") +
    geom_line(data = line_ready(monthly_plot, c("watershed", ".segment"), "month"),
              linewidth = 0.94, na.rm = TRUE) +
    geom_point(size = 1.75, na.rm = TRUE) +
    scale_colour_manual(values = watershed_palette(focus)) +
    scale_x_date(date_breaks = period_breaks(display$month),
                 date_labels = "%b\n%Y") +
    labs(title = "A. Monthly watershed Pollution Index",
         subtitle = "Monthly station median, then watershed median; no observations interpolated",
         x = NULL, y = "Pollution Index (IP)", colour = "Watershed") +
    base_theme(10.1)

  status_names <- c("Meets standard", "Light pollution", "Moderate pollution", "Heavy pollution")
  scored <- dat %>% mutate(
    status = case_when(ip <= 1 ~ status_names[[1L]],
                       ip < 5 ~ status_names[[2L]],
                       ip < 10 ~ status_names[[3L]],
                       TRUE ~ status_names[[4L]]),
    status = factor(status, levels = status_names)
  )
  # Each station contributes total weight one, regardless of its archived days.
  station_status <- scored %>% count(watershed, station, status, name = "n_days") %>%
    group_by(watershed, station) %>% mutate(share = n_days / sum(n_days)) %>%
    tidyr::complete(status = factor(status_names, levels = status_names),
                    fill = list(share = 0, n_days = 0)) %>% ungroup()
  status_all <- station_status %>% group_by(watershed, status) %>%
    summarise(station_balanced_share = mean(share),
              n_stations = n_distinct(station), .groups = "drop")
  write_table(status_all, "05_onlimo_historical_station_balanced_IP_categories")
  status_focus <- status_all %>% filter(watershed %in% focus)
  p_status <- ggplot(status_focus,
                     aes(x = watershed, y = station_balanced_share, fill = status)) +
    geom_col(width = 0.69, colour = "white", linewidth = 0.20) +
    coord_flip() +
    scale_fill_manual(values = c("Meets standard" = "#009E73",
                                 "Light pollution" = "#E9C46A",
                                 "Moderate pollution" = "#E88D47",
                                 "Heavy pollution" = "#B34040"), drop = FALSE) +
    scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
    labs(title = "B. Observed IP category composition",
         subtitle = "Station-balanced percentages, across each station's observed dates",
         x = NULL, y = "Share of classified station-days", fill = "Derived IP category") +
    base_theme(9.7) + theme(panel.grid.major.y = element_blank())

  coverage <- monthly %>% select(month, watershed, n_stations, station_days)
  write_table(coverage, "05_onlimo_historical_monthly_station_coverage")
  p_cover <- ggplot(coverage %>% filter(watershed %in% focus),
                    aes(x = month, y = watershed, fill = n_stations)) +
    geom_tile(width = 27, height = 0.79, colour = "white", linewidth = 0.42) +
    scale_fill_gradient(low = COL$pale, high = COL$indonesia,
                        name = "Stations") +
    scale_x_date(date_breaks = period_breaks(coverage$month),
                 date_labels = "%b\n%Y") +
    labs(title = "C. Historical sampling coverage",
         subtitle = "Unique stations represented per watershed and month",
         x = NULL, y = NULL) + base_theme(9.8)

  write_table(tibble(
    first_date = min(dat$date), last_date = max(dat$date),
    observed_stations = n_distinct(dat$station),
    represented_watersheds = n_distinct(dat$watershed),
    focus_watersheds = paste(focus, collapse = "; "),
    caution = "Backfilled date coverage is not evidence of uniformly sampled history"
  ), "05_onlimo_historical_scope")
  page <- section_page(
    list(p_month, p_status, p_cover),
    "05  |  ONLIMO historical Pollution Index",
    paste0("Selected Indonesian watersheds | observed ", min(dat$date), " to ", max(dat$date),
           " | archive is not assumed to be continuous"),
    paste(
      "IP classes: <=1 meets standard, (1,5) light, [5,10) moderate, >=10 heavy.",
      "Plots describe the observed archival period, not a regional historical baseline.",
      "Missing station-months are not interpreted as clean water."
    ), top_fraction = 1.04
  )
  list(page = page, panels = list(
    monthly_pollution_index = p_month, pollution_categories = p_status,
    monthly_station_coverage = p_cover
  ))
}

# -----------------------------------------------------------------------------
# 09 | Section 06: Fujian weekly surface-water archive
# -----------------------------------------------------------------------------
build_fujian <- function() {
  log_msg("Section 06: Fujian weekly surface water")
  dat <- read_canonical("fujian")
  if (is.null(dat) || !nrow(dat)) {
    empty <- blank_plot("Fujian weekly", "fujian_weekly_analysis.rds is absent or empty; run script 12")
    return(list(page = section_page(list(empty), "06  |  Fujian weekly surface water",
                                    "Unavailable canonical input", "No observations extrapolated"),
                panels = list(data_unavailable = empty)))
  }
  dat$year <- get_year(dat, "fujian")
  dat$station <- get_station(dat, "fujian")
  dat <- dat %>% filter(is.finite(year), valid_station(station))
  if (!nrow(dat)) {
    empty <- blank_plot("Fujian weekly", "No valid station/year observations")
    return(list(page = section_page(list(empty), "06  |  Fujian weekly surface water",
                                    "Invalid source dates or IDs", "No invented values"),
                panels = list(data_unavailable = empty)))
  }

  class <- class_score(get_col(dat, c("current_week_water_quality", "water_quality_class")))
  class_summary <- summarise_class_share(
    tibble(station = dat$station, year = dat$year, score = class),
    "year", cutoff = 3L
  )
  write_table(class_summary, "06_fujian_annual_class_I_III")
  p_class <- plot_class_share(
    class_summary, "A. Reported weekly classes I-III",
    "Annual within-station share, then median across stations (IQR shaded)",
    class_cutoff = 3L, x_is_date = FALSE, color = COL$fujian
  )

  spec <- tibble(
    parameter = c("Dissolved oxygen", "CODMn", "Total phosphorus", "Ammonia-N", "Total nitrogen"),
    value_col = c("dissolved_oxygen_mg_l_raw", "permanganate_index_mg_l_raw",
                  "total_phosphorus_mg_l_raw", "ammonia_nitrogen_mg_l_raw",
                  "total_nitrogen_mg_l_raw"),
    raw_col = c("dissolved_oxygen_mg_l_raw", "permanganate_index_mg_l_raw",
                "total_phosphorus_mg_l_raw", "ammonia_nitrogen_mg_l_raw",
                "total_nitrogen_mg_l_raw")
  )
  dates <- as.Date(sprintf("%04d-01-01", dat$year))
  long <- parameter_long(dat, spec, dates, dat$station)
  annual <- station_balanced(long, "year")
  write_table(parameter_quality(long), "06_fujian_parameter_availability")
  write_table(annual, "06_fujian_annual_station_balanced_parameters")
  p_params <- plot_medians(
    annual, "B. Long-term reported river chemistry",
    "Median across station-year medians; between-station IQR shaded",
    x_is_date = FALSE, line_color = COL$fujian, ncol = 3L
  )

  # City-specific station counts only when the canonical coordinate crosswalk
  # provides city labels. Never classify a station as Xiamen from proximity alone.
  city <- clean_text(get_col(dat, "station_city"))
  xiamen <- !is.na(city) & grepl(XIAMEN_REGEX, city, ignore.case = TRUE)
  yearly_coverage <- dat %>% group_by(year) %>%
    summarise(n_stations = n_distinct(station),
              report_rows = n(), .groups = "drop")
  xiamen_year <- tibble(year = dat$year[xiamen], station = dat$station[xiamen]) %>%
    group_by(year) %>% summarise(xiamen_stations = n_distinct(station), .groups = "drop")
  yearly_coverage <- yearly_coverage %>% left_join(xiamen_year, by = "year") %>%
    mutate(xiamen_stations = coalesce(xiamen_stations, 0L))
  write_table(yearly_coverage, "06_fujian_annual_station_and_row_coverage")
  p_counts <- ggplot(yearly_coverage, aes(x = year, y = n_stations)) +
    geom_col(fill = COL$fujian, alpha = 0.83, width = 0.82) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
    labs(title = "C. Observed weekly station network",
         subtitle = "Distinct stations by reporting year; changes affect apparent trends",
         x = "Reporting year", y = "Number of reporting stations") +
    base_theme(10)
  if (any(yearly_coverage$xiamen_stations > 0)) {
    p_counts <- p_counts +
      geom_line(aes(y = xiamen_stations, group = 1),
                colour = "#D55E00", linewidth = 0.92) +
      geom_point(aes(y = xiamen_stations), colour = "#D55E00", size = 1.65) +
      labs(caption = "Orange line: stations with an explicit Xiamen city label (not all stations are city-coded)")
  }

  write_table(tibble(first_year = min(dat$year), last_year = max(dat$year),
                     stations = n_distinct(dat$station), source_rows = nrow(dat),
                     xiamen_city_coded_stations = n_distinct(dat$station[xiamen]),
                     caution = "Changes in station coverage and censoring can influence temporal summaries"),
              "06_fujian_scope")
  page <- section_page(
    list(p_params, p_class, p_counts),
    "06  |  Fujian weekly surface-water quality",
    paste0("Fujian Province | published weekly reports, ", min(dat$year), "-", max(dat$year),
           " | station-balanced annual summaries"),
    paste(
      "CODMn (permanganate index) is not identical to COD; nutrient forms are reported source-native.",
      "Class I-III shares and measured concentrations answer different questions.",
      "Censored numerical fields omitted from medians; changing station coverage must be considered."
    ), top_fraction = 1.09
  )
  list(page = page, panels = list(
    annual_parameters = p_params, annual_river_class = p_class,
    annual_station_coverage = p_counts
  ))
}

# -----------------------------------------------------------------------------
# 10 | Run, export figures, and emit report-automation manifest
# -----------------------------------------------------------------------------
main <- function() {
  log_msg("Start Teliti integrated visualisation | script 17")
  if (!any(file.exists(DATASETS$file))) {
    stop(
      "No canonical analysis RDS files were found under: ", ANALYSIS_DIR,
      "\nRun teliti_reconciliation/script/12_build_analysis_datasets.R first.",
      "\nOr set TELITI_DATA_ROOT / TELITI_ANALYSIS_DIR.", call. = FALSE
    )
  }
  results <- list(
    `01` = build_overview(),
    `02` = build_cnemc(),
    `03` = build_nmemc(),
    `04` = build_onlimo_daily(),
    `05` = build_onlimo_historical(),
    `06` = build_fujian()
  )

  filenames <- c(
    `01` = "section_01_evidence_coverage_and_maps",
    `02` = "section_02_cnemc_surface_water",
    `03` = "section_03_nmemc_marine",
    `04` = "section_04_onlimo_daily",
    `05` = "section_05_onlimo_historical",
    `06` = "section_06_fujian_weekly"
  )

  for (id in names(results)) {
    log_msg("Exporting section ", id, " as PNG and PDF")
    save_pair(results[[id]]$page, filenames[[id]], section = id, kind = "section_page")
    if (SAVE_PANELS) {
      for (panel_name in names(results[[id]]$panels)) {
        save_pair(
          results[[id]]$panels[[panel_name]],
          paste0("panel_", id, "_", panel_name), folder = PANEL_DIR,
          width = PANEL_WIDTH, height = PANEL_HEIGHT,
          section = id, kind = "individual_panel"
        )
      }
    }
  }

  if (SAVE_COMBINED_PDF) {
    combined_path <- file.path(OUTPUT_DIR, "teliti_water_quality_six_section_report.pdf")
    log_msg("Exporting six-page PDF: ", combined_path)
    grDevices::pdf(combined_path, width = REPORT_WIDTH,
                   height = REPORT_HEIGHT, onefile = TRUE, useDingbats = FALSE)
    tryCatch(
      for (id in names(results)) print(results[[id]]$page),
      finally = grDevices::dev.off()
    )
  }

  figure_manifest <- dplyr::bind_rows(FIGURES) %>%
    mutate(generated_at = RUN_AT, script = "17_plot_integrated_water_quality_report.R")
  write_table(figure_manifest, "17_figure_manifest")
  readr::write_csv(figure_manifest,
                   file.path(OUTPUT_DIR, "figure_manifest.csv"), na = "")

  input_audit <- DATASETS %>%
    transmute(section_key = key, dataset = label, source_file = file,
              file_exists = file.exists(file),
              size_mb = ifelse(file_exists, round(file.info(file)$size / 1024^2, 2), NA_real_))
  input_audit <- bind_rows(input_audit,
    tibble(section_key = "station_metadata", dataset = "Station metadata",
           source_file = STATION_FILE, file_exists = file.exists(STATION_FILE),
           size_mb = if (file.exists(STATION_FILE)) round(file.info(STATION_FILE)$size / 1024^2, 2) else NA_real_)
  )
  write_table(input_audit, "17_input_file_audit")
  write_table(tibble(
    script = "17_plot_integrated_water_quality_report.R",
    executed_at = RUN_AT,
    root_dir = normalizePath(PROJECT_DIR, winslash = "/", mustWork = FALSE),
    analysis_dir = normalizePath(ANALYSIS_DIR, winslash = "/", mustWork = FALSE),
    output_dir = normalizePath(OUTPUT_DIR, winslash = "/", mustWork = TRUE),
    dpi = OUTPUT_DPI,
    report_pages = length(results),
    standalone_figures = nrow(figure_manifest),
    six_page_pdf = if (SAVE_COMBINED_PDF) file.path(OUTPUT_DIR,
      "teliti_water_quality_six_section_report.pdf") else NA_character_,
    note = paste(
      "Descriptive station-balanced summaries; no raw data modified.",
      "Published water-quality classes, marine classes, and IP are not cross-system equivalents."
    )
  ), "17_run_metadata")

  log_msg("FINISHED | ", length(results), " report sections, ",
          nrow(figure_manifest), " PNG/PDF figure pairs")
  log_msg("All exports: ", normalizePath(OUTPUT_DIR, winslash = "/", mustWork = TRUE))
  invisible(list(sections = results, manifest = figure_manifest))
}

# The script is designed to work both with Rscript and with source() in RStudio.
TELITI_REPORT_17 <- main()
