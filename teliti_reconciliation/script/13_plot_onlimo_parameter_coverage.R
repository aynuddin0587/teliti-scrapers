# ============================================================
# 13_plot_onlimo_parameter_coverage.R
#
# ONLIMO daily water-quality parameter inventory and monitoring
# visualization.
#
# Scientific purpose:
#   The ONLIMO daily archive is temporally shorter than the
#   historical Pollution Index archive. This script therefore
#   emphasizes what measured parameters are available, how
#   complete their coverage is, where they are represented, and
#   what recent temporal structure is visible without presenting
#   the record as a long-term trend dataset.
#
# Main outputs:
#   onlimo_parameter_availability_overview.png / .pdf
#   panel_onlimo_parameter_inventory.png
#   panel_onlimo_parameter_timeline.png
#   panel_onlimo_parameter_recent_trends.png
#   panel_onlimo_parameter_watershed_coverage.png
#
# Tabular outputs:
#   onlimo_parameter_inventory.csv
#   onlimo_parameter_summary.csv
#   onlimo_parameter_daily_trends.csv
#   onlimo_parameter_watershed_coverage.csv
# ============================================================

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------
PROJECT_DIR <- "D:/# R Project/penelitian"
ANALYSIS_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation", "analysis")
OUTPUT_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation", "output", "onlimo_parameters")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

ONLIMO_CANONICAL <- file.path(ANALYSIS_DIR, "onlimo_daily_analysis.rds")
ONLIMO_FALLBACK <- file.path(PROJECT_DIR, "onlimo", "data", "onlimo_daily_parameters_archive.csv")

OUTPUT_WIDTH <- 15
OUTPUT_HEIGHT <- 11
OUTPUT_DPI <- 320
SAVE_PDF <- TRUE
SAVE_PANELS <- TRUE

# The recent-trend panel is explicitly descriptive/current monitoring.
# If the archive is shorter than this window, the whole archive is used.
TREND_WINDOW_DAYS <- 120L

# Restrict the watershed heatmap to the most represented watersheds so that
# the presentation figure remains readable. The CSV retains every watershed.
MAX_WATERSHEDS_IN_FIGURE <- 10L

# Presentation palette: chosen to remain distinguishable in common forms of
# colour-vision deficiency and on a light background.
COLORS <- list(
  measured = "#2A6FBB",
  station = "#D55E00",
  trend = "#0072B2",
  ribbon = "#56B4E9",
  grid = "#E5E7EB",
  text = "#1F2937",
  muted = "#6B7280",
  heat_low = "#F3F4F6",
  heat_high = "#0072B2"
)

# Raw daily mean parameters actually measured/reported by ONLIMO.
PARAMETER_META <- data.frame(
  parameter = c(
    "ph", "do", "tds", "cod", "bod", "tss",
    "nitrate", "ammonia", "temperature"
  ),
  label = c(
    "pH", "Dissolved oxygen", "Total dissolved solids",
    "Chemical oxygen demand", "Biochemical oxygen demand",
    "Total suspended solids", "Nitrate", "Ammonia",
    "Water temperature"
  ),
  short_label = c(
    "pH", "DO", "TDS", "COD", "BOD", "TSS",
    "Nitrate", "Ammonia", "Temperature"
  ),
  unit = c(
    "pH units", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L",
    "mg/L", "mg/L", "deg C"
  ),
  category = c(
    "General", "Oxygen", "Solids", "Organic pollution",
    "Organic pollution", "Solids", "Nutrients", "Nutrients",
    "General"
  ),
  stringsAsFactors = FALSE
)

# These are derived/interpretive fields available in the daily archive.
# They are inventoried but deliberately excluded from the measured-parameter
# figure so that measured concentrations are not visually mixed with indices.
DERIVED_META <- data.frame(
  parameter = c(
    "index_ph", "index_do", "index_tds", "index_cod", "index_bod",
    "index_tss", "index_nitrate", "index_ammonia",
    "mean_parameter_index", "maximum_parameter_index",
    "critical_parameter", "pollution_index", "pollution_status"
  ),
  label = c(
    "pH parameter index", "DO parameter index", "TDS parameter index",
    "COD parameter index", "BOD parameter index", "TSS parameter index",
    "Nitrate parameter index", "Ammonia parameter index",
    "Mean parameter index", "Maximum parameter index",
    "Critical parameter", "Pollution Index", "Pollution status"
  ),
  parameter_type = c(rep("derived_parameter_index", 10),
                     "derived_status", "derived_pollution_index", "derived_status"),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 2. Packages
# -----------------------------------------------------------------------------
required_packages <- c("dplyr", "tidyr", "readr", "ggplot2", "patchwork", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall with: install.packages(c(",
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
})

# -----------------------------------------------------------------------------
# 3. Helpers
# -----------------------------------------------------------------------------
log_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
}

read_onlimo_daily <- function() {
  if (file.exists(ONLIMO_CANONICAL)) {
    x <- readRDS(ONLIMO_CANONICAL)
    attr(x, "source_file") <- ONLIMO_CANONICAL
    return(x)
  }

  if (file.exists(ONLIMO_FALLBACK)) {
    warning(
      "Canonical onlimo_daily_analysis.rds was not found. Using the operational ",
      "ONLIMO daily archive as a fallback. Re-run script 12 when possible.",
      call. = FALSE
    )
    x <- readr::read_csv(
      ONLIMO_FALLBACK,
      show_col_types = FALSE,
      progress = FALSE,
      col_types = cols(date = col_date(), .default = col_guess())
    )
    attr(x, "source_file") <- ONLIMO_FALLBACK
    return(x)
  }

  stop(
    "No ONLIMO daily analysis dataset found. Expected:\n",
    ONLIMO_CANONICAL, "\n",
    "or fallback:\n", ONLIMO_FALLBACK,
    call. = FALSE
  )
}

safe_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(readr::parse_number(as.character(x), na = c("", "NA", "NaN", "-", "--")))
}

safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  suppressWarnings(as.Date(as.character(x)))
}

median_or_na <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  stats::median(x)
}

quantile_or_na <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, names = FALSE, type = 7))
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

line_ready <- function(data, group_cols) {
  if (is.null(data) || nrow(data) == 0L) return(data)
  data %>%
    group_by(across(all_of(group_cols))) %>%
    filter(dplyr::n_distinct(date) >= 2L) %>%
    ungroup()
}

base_theme <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, colour = COLORS$text),
      plot.subtitle = element_text(size = base_size - 1, colour = COLORS$muted),
      plot.caption = element_text(size = base_size - 2, colour = COLORS$muted),
      axis.title = element_text(face = "bold", colour = COLORS$text),
      axis.text = element_text(colour = COLORS$text),
      panel.grid.major = element_line(colour = COLORS$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", colour = COLORS$text),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

# -----------------------------------------------------------------------------
# 4. Read and normalize data
# -----------------------------------------------------------------------------
log_msg("Reading ONLIMO daily data ...")
dat <- read_onlimo_daily()
source_file <- attr(dat, "source_file")

required_id <- c("station_id", "date")
missing_id <- setdiff(required_id, names(dat))
if (length(missing_id) > 0L) {
  stop("ONLIMO daily dataset lacks required column(s): ", paste(missing_id, collapse = ", "), call. = FALSE)
}

available_measured <- intersect(PARAMETER_META$parameter, names(dat))
missing_measured <- setdiff(PARAMETER_META$parameter, names(dat))
if (length(available_measured) == 0L) {
  stop("None of the expected ONLIMO measured parameter columns were found.", call. = FALSE)
}
if (length(missing_measured) > 0L) {
  warning("Expected measured parameter column(s) absent: ", paste(missing_measured, collapse = ", "), call. = FALSE)
}

meta_measured <- PARAMETER_META %>% filter(parameter %in% available_measured)

# Normalize date and measured numeric columns. Keep the original object intact
# except for the analytical copy below.
dat_clean <- dat %>%
  mutate(
    date = safe_date(date),
    station_id = as.character(station_id),
    station_name = if ("station_name" %in% names(dat)) as.character(station_name) else station_id,
    watershed = if ("watershed" %in% names(dat)) as.character(watershed) else NA_character_
  )

for (nm in available_measured) {
  dat_clean[[nm]] <- safe_numeric(dat_clean[[nm]])
}

# The canonical builder should already be one station-date record. This guard
# makes the visualization deterministic if a fallback operational archive has
# duplicate keys.
dat_clean <- dat_clean %>%
  filter(!is.na(station_id), !is.na(date)) %>%
  arrange(station_id, date) %>%
  group_by(station_id, date) %>%
  slice_tail(n = 1L) %>%
  ungroup()

n_rows_total <- nrow(dat_clean)
n_stations_total <- n_distinct(dat_clean$station_id)
archive_start <- min_date_or_na(dat_clean$date)
archive_end <- max_date_or_na(dat_clean$date)
archive_days <- if (!is.na(archive_start) && !is.na(archive_end)) as.integer(archive_end - archive_start) + 1L else NA_integer_

log_msg(
  "Normalized archive: ", scales::comma(n_rows_total), " station-date rows; ",
  scales::comma(n_stations_total), " stations; ",
  as.character(archive_start), " to ", as.character(archive_end)
)

# -----------------------------------------------------------------------------
# 5. Parameter inventories
# -----------------------------------------------------------------------------
measured_long <- dat_clean %>%
  select(station_id, station_name, watershed, date, all_of(available_measured)) %>%
  pivot_longer(
    cols = all_of(available_measured),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  left_join(meta_measured, by = "parameter")

parameter_summary <- measured_long %>%
  group_by(parameter, label, short_label, unit, category) %>%
  summarise(
    total_station_date_rows = n(),
    valid_observations = sum(is.finite(value)),
    completeness_pct = 100 * valid_observations / total_station_date_rows,
    stations_with_data = n_distinct(station_id[is.finite(value)]),
    station_coverage_pct = 100 * stations_with_data / n_stations_total,
    watersheds_with_data = n_distinct(watershed[is.finite(value) & !is.na(watershed) & watershed != ""]),
    dates_with_data = n_distinct(date[is.finite(value)]),
    first_date = min_date_or_na(date[is.finite(value)]),
    last_date = max_date_or_na(date[is.finite(value)]),
    median = median_or_na(value),
    q25 = quantile_or_na(value, 0.25),
    q75 = quantile_or_na(value, 0.75),
    minimum = if (any(is.finite(value))) min(value[is.finite(value)]) else NA_real_,
    maximum = if (any(is.finite(value))) max(value[is.finite(value)]) else NA_real_,
    .groups = "drop"
  ) %>%
  arrange(match(parameter, PARAMETER_META$parameter))

# Full inventory includes derived fields to document what is at disposal.
all_inventory_meta <- bind_rows(
  PARAMETER_META %>%
    transmute(parameter, label, parameter_type = "measured_parameter", unit, category),
  DERIVED_META %>%
    mutate(unit = NA_character_, category = "Derived") %>%
    select(parameter, label, parameter_type, unit, category)
)

parameter_inventory <- all_inventory_meta %>%
  rowwise() %>%
  mutate(
    column_present = parameter %in% names(dat_clean),
    nonmissing_n = if (column_present) sum(!is.na(dat_clean[[parameter]])) else 0L,
    nonmissing_pct = if (column_present && n_rows_total > 0L) 100 * nonmissing_n / n_rows_total else 0,
    stations_with_data = if (column_present) n_distinct(dat_clean$station_id[!is.na(dat_clean[[parameter]])]) else 0L,
    first_date = if (column_present) min_date_or_na(dat_clean$date[!is.na(dat_clean[[parameter]])]) else as.Date(NA),
    last_date = if (column_present) max_date_or_na(dat_clean$date[!is.na(dat_clean[[parameter]])]) else as.Date(NA)
  ) %>%
  ungroup()

readr::write_csv(parameter_inventory, file.path(OUTPUT_DIR, "onlimo_parameter_inventory.csv"), na = "")
readr::write_csv(parameter_summary, file.path(OUTPUT_DIR, "onlimo_parameter_summary.csv"), na = "")

# -----------------------------------------------------------------------------
# 6. Daily station-balanced trends
# -----------------------------------------------------------------------------
# First summarize any repeated measurements within station-day (defensive),
# then summarize the distribution across stations for each day. This prevents
# stations with more records from receiving extra weight.
station_day <- measured_long %>%
  filter(is.finite(value)) %>%
  group_by(station_id, date, parameter, label, short_label, unit, category) %>%
  summarise(value = median(value, na.rm = TRUE), .groups = "drop")

daily_trends <- station_day %>%
  group_by(date, parameter, label, short_label, unit, category) %>%
  summarise(
    median = median_or_na(value),
    q25 = quantile_or_na(value, 0.25),
    q75 = quantile_or_na(value, 0.75),
    station_n = n_distinct(station_id),
    .groups = "drop"
  ) %>%
  mutate(facet_label = paste0(short_label, " (", unit, ")"))

readr::write_csv(daily_trends, file.path(OUTPUT_DIR, "onlimo_parameter_daily_trends.csv"), na = "")

trend_start <- if (!is.na(archive_end)) max(archive_start, archive_end - TREND_WINDOW_DAYS + 1L, na.rm = TRUE) else archive_start
recent_trends <- daily_trends %>% filter(is.na(trend_start) | date >= trend_start)
recent_lines <- line_ready(recent_trends, c("parameter"))

# -----------------------------------------------------------------------------
# 7. Watershed coverage
# -----------------------------------------------------------------------------
watershed_base <- dat_clean %>%
  filter(!is.na(watershed), watershed != "") %>%
  count(watershed, name = "station_date_rows") %>%
  arrange(desc(station_date_rows))

top_watersheds <- head(watershed_base$watershed, MAX_WATERSHEDS_IN_FIGURE)

watershed_coverage <- measured_long %>%
  filter(!is.na(watershed), watershed != "") %>%
  group_by(watershed, parameter, short_label) %>%
  summarise(
    station_date_rows = n(),
    valid_observations = sum(is.finite(value)),
    coverage_pct = 100 * valid_observations / station_date_rows,
    stations_with_data = n_distinct(station_id[is.finite(value)]),
    .groups = "drop"
  )

readr::write_csv(
  watershed_coverage,
  file.path(OUTPUT_DIR, "onlimo_parameter_watershed_coverage.csv"),
  na = ""
)

watershed_plot_data <- watershed_coverage %>%
  filter(watershed %in% top_watersheds) %>%
  mutate(
    watershed = factor(watershed, levels = rev(top_watersheds)),
    short_label = factor(short_label, levels = PARAMETER_META$short_label)
  )

# -----------------------------------------------------------------------------
# 8. Plot A: parameter inventory
# -----------------------------------------------------------------------------
inv_plot <- parameter_summary %>%
  mutate(label = factor(label, levels = rev(PARAMETER_META$label)))

p_inventory <- ggplot(inv_plot, aes(y = label)) +
  geom_col(
    aes(x = completeness_pct),
    fill = COLORS$measured,
    alpha = 0.82,
    width = 0.65
  ) +
  geom_point(
    aes(x = station_coverage_pct),
    shape = 21,
    fill = COLORS$station,
    colour = "white",
    stroke = 0.6,
    size = 3.4
  ) +
  geom_text(
    aes(
      x = pmin(99, completeness_pct + 2),
      label = paste0(scales::comma(valid_observations), " rows")
    ),
    hjust = 0,
    size = 3.0,
    colour = COLORS$text
  ) +
  scale_x_continuous(
    limits = c(0, 115),
    breaks = seq(0, 100, 20),
    labels = function(x) ifelse(x <= 100, paste0(x, "%"), ""),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "A. What measured parameters are available?",
    subtitle = "Bars = non-missing station-date coverage; points = share of archived stations with at least one value.",
    x = "Coverage",
    y = NULL,
    caption = paste0("Archive contains ", scales::comma(n_rows_total), " unique station-date rows across ",
                     scales::comma(n_stations_total), " stations.")
  ) +
  base_theme() +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# 9. Plot B: temporal coverage
# -----------------------------------------------------------------------------
time_plot <- parameter_summary %>%
  mutate(label = factor(label, levels = rev(PARAMETER_META$label)))

p_timeline <- ggplot(time_plot, aes(y = label)) +
  geom_segment(
    aes(x = first_date, xend = last_date, yend = label),
    linewidth = 2.4,
    colour = COLORS$measured,
    alpha = 0.75,
    lineend = "round"
  ) +
  geom_point(aes(x = first_date), shape = 21, fill = "white", colour = COLORS$measured, size = 2.5, stroke = 0.8) +
  geom_point(aes(x = last_date), shape = 21, fill = COLORS$measured, colour = "white", size = 2.8, stroke = 0.5) +
  labs(
    title = "B. When are the parameters represented?",
    subtitle = "First-to-last valid date for each measured parameter; this describes archive coverage, not continuity between endpoints.",
    x = NULL,
    y = NULL
  ) +
  scale_x_date(date_labels = "%d %b\n%Y", date_breaks = "2 weeks", expand = expansion(mult = c(0.02, 0.05))) +
  base_theme()

# -----------------------------------------------------------------------------
# 10. Plot C: recent station-balanced trends
# -----------------------------------------------------------------------------
p_recent <- ggplot(recent_trends, aes(x = date, y = median, group = 1)) +
  geom_ribbon(
    aes(ymin = q25, ymax = q75),
    fill = COLORS$ribbon,
    alpha = 0.22,
    colour = NA
  ) +
  geom_line(
    data = recent_lines,
    colour = COLORS$trend,
    linewidth = 0.75,
    na.rm = TRUE
  ) +
  geom_point(
    colour = COLORS$trend,
    size = 1.25,
    alpha = 0.8,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ facet_label,
    scales = "free_y",
    ncol = 3,
    labeller = label_wrap_gen(width = 24)
  ) +
  labs(
    title = "C. Recent parameter dynamics",
    subtitle = "Daily median and interquartile range across stations; free y-scales retain native units and avoid a synthetic composite index.",
    x = NULL,
    y = "Daily station-balanced value",
    caption = paste0("Shown from ", as.character(trend_start), " to ", as.character(archive_end), ". Descriptive monitoring only; not a long-term trend test.")
  ) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
  base_theme(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(size = 9.5, face = "bold")
  )

# -----------------------------------------------------------------------------
# 11. Plot D: watershed coverage heatmap
# -----------------------------------------------------------------------------
p_watershed <- ggplot(
  watershed_plot_data,
  aes(x = short_label, y = watershed, fill = coverage_pct)
) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(
    aes(
      label = ifelse(coverage_pct >= 1, paste0(round(coverage_pct), "%"), ""),
      colour = coverage_pct >= 60
    ),
    size = 2.8
  ) +
  scale_colour_manual(
    values = c(`FALSE` = COLORS$text, `TRUE` = "white"),
    guide = "none"
  ) +
  scale_fill_gradient(
    low = COLORS$heat_low,
    high = COLORS$heat_high,
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "D. Where is each parameter represented?",
    subtitle = paste0("Station-date completeness for the ", length(top_watersheds), " most represented watersheds in the daily archive."),
    x = NULL,
    y = NULL,
    fill = "Coverage"
  ) +
  base_theme(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.grid = element_blank()
  )

# -----------------------------------------------------------------------------
# 12. Dashboard and exports
# -----------------------------------------------------------------------------
summary_sentence <- paste0(
  "Measured parameters: ", nrow(parameter_summary),
  " | stations: ", scales::comma(n_stations_total),
  " | station-date rows: ", scales::comma(n_rows_total),
  " | archive span: ", as.character(archive_start), " to ", as.character(archive_end)
)

p_dashboard <- (p_inventory | p_timeline) / (p_recent | p_watershed) +
  plot_layout(heights = c(0.9, 1.35)) +
  plot_annotation(
    title = "ONLIMO daily water-quality parameter coverage",
    subtitle = paste0(
      summary_sentence,
      "\nUse this archive to describe parameter availability and recent water-quality processes; use the longer historical Pollution Index archive for long-term context."
    ),
    caption = paste0(
      "Source: ", source_file,
      ". Availability indicates observed non-missing values and does not by itself establish measurement validity; scientific range/outlier QC should precede formal modeling."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, colour = COLORS$text),
      plot.subtitle = element_text(size = 11, colour = COLORS$muted),
      plot.caption = element_text(size = 9, colour = COLORS$muted)
    )
  )

main_png <- file.path(OUTPUT_DIR, "onlimo_parameter_availability_overview.png")
ggsave(
  main_png,
  p_dashboard,
  width = OUTPUT_WIDTH,
  height = OUTPUT_HEIGHT,
  dpi = OUTPUT_DPI,
  bg = "white"
)

if (isTRUE(SAVE_PDF)) {
  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
  ggsave(
    file.path(OUTPUT_DIR, "onlimo_parameter_availability_overview.pdf"),
    p_dashboard,
    width = OUTPUT_WIDTH,
    height = OUTPUT_HEIGHT,
    device = pdf_device,
    bg = "white"
  )
}

if (isTRUE(SAVE_PANELS)) {
  ggsave(file.path(OUTPUT_DIR, "panel_onlimo_parameter_inventory.png"), p_inventory,
         width = 8.2, height = 6.2, dpi = OUTPUT_DPI, bg = "white")
  ggsave(file.path(OUTPUT_DIR, "panel_onlimo_parameter_timeline.png"), p_timeline,
         width = 8.2, height = 6.2, dpi = OUTPUT_DPI, bg = "white")
  ggsave(file.path(OUTPUT_DIR, "panel_onlimo_parameter_recent_trends.png"), p_recent,
         width = 11.5, height = 9.0, dpi = OUTPUT_DPI, bg = "white")
  ggsave(file.path(OUTPUT_DIR, "panel_onlimo_parameter_watershed_coverage.png"), p_watershed,
         width = 10.5, height = 7.0, dpi = OUTPUT_DPI, bg = "white")
}

log_msg("ONLIMO parameter visualization complete.")
log_msg("Main PNG: ", main_png)
log_msg("Measured parameter columns found: ", paste(parameter_summary$short_label, collapse = ", "))
log_msg(
  "Archive span: ", as.character(archive_start), " to ", as.character(archive_end),
  if (!is.na(archive_days)) paste0(" (", archive_days, " calendar days)") else ""
)
log_msg("Highest station-date completeness: ",
        parameter_summary$short_label[[which.max(parameter_summary$completeness_pct)]], " = ",
        round(max(parameter_summary$completeness_pct, na.rm = TRUE), 1), "%")
log_msg("Lowest station-date completeness: ",
        parameter_summary$short_label[[which.min(parameter_summary$completeness_pct)]], " = ",
        round(min(parameter_summary$completeness_pct, na.rm = TRUE), 1), "%")

invisible(list(
  parameter_summary = parameter_summary,
  daily_trends = daily_trends,
  watershed_coverage = watershed_coverage,
  parameter_inventory = parameter_inventory
))