# ============================================================
# Teliti static collection-progress dashboard
#
# Purpose:
#   Produce a static presentation-ready figure summarizing
#   what data have been collected so far, how far each archive
#   extends in time, and where each collector stands operationally.
#
# Outputs:
#   - teliti_collection_dashboard.png
#   - teliti_collection_dashboard.pdf
#   - panel_coverage_timeline.png
#   - panel_archive_volume.png
#   - panel_status_matrix.png
#   - panel_next_actions.png
#   - collector_monitoring_summary.csv
#
# Suggested use:
#   source("teliti_reconciliation/script/09_plot_collection_progress.R")
# or
#   Rscript "D:/# R Project/penelitian/teliti_reconciliation/script/09_plot_collection_progress.R"
# ============================================================

options(stringsAsFactors = FALSE)

# -----------------------------
# 1. User configuration
# -----------------------------
PROJECT_DIR <- "D:/# R Project/penelitian"
BACKUP_DIR  <- "D:/# R Project/teliti-data-backup"
REGISTRY_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation", "registry")
OUTPUT_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation", "output", "collection_monitor")

# Figure settings ------------------------------------------------------------
FIGURE_TITLE <- "Data-collection progress"
FIGURE_SUBTITLE <- paste(
  "Static monitoring dashboard for collection coverage, archive growth,",
  "and validation maturity"
)
FIGURE_CAPTION <- paste(
  "Source: local archive files, GitHub backup structure, and collector validation registry.",
  "Generated automatically by 09_plot_collection_progress.R"
)

OUTPUT_PREFIX <- "teliti_collection_dashboard"
OUTPUT_WIDTH  <- 15
OUTPUT_HEIGHT <- 11
OUTPUT_DPI    <- 320
SAVE_PDF      <- TRUE
SAVE_PANELS   <- TRUE
USE_LOG10_VOLUME <- FALSE

# Colors --------------------------------------------------------------------
COLORS <- list(
  background = "white",
  text = "#1A1A1A",
  grid = "#D9D9D9",
  muted = "#6F6F6F",
  title = "#163A5F",
  timeline_fill = c(
    nmemc_marine = "#3B82F6",
    fujian_weekly_surfacewater = "#10B981",
    onlimo_historical_pollution_index = "#F59E0B",
    onlimo_daily = "#8B5CF6",
    cnemc_surfacewater = "#EF4444"
  ),
  status = c(
    PASS = "#1B9E77",
    PASS_INITIAL = "#66A61E",
    PASS_PARTIAL_CATCHUP = "#E6AB02",
    PASS_WITH_SOURCE_REVISION_CONTEXT = "#7570B3",
    REVIEW = "#D95F02",
    FAIL = "#D73027",
    UNKNOWN = "#BDBDBD"
  ),
  windows = c(
    enabled = "#D95F02",
    disabled = "#1B9E77",
    not_applicable = "#7570B3",
    unknown = "#BDBDBD"
  ),
  role = c(
    github_primary = "#1B9E77",
    parallel_validation = "#4C78A8",
    github_catchup = "#F2A104",
    parallel_targeted_delta_validation = "#E15759",
    unknown = "#BDBDBD"
  )
)

# Collector order on plots ---------------------------------------------------
COLLECTOR_ORDER <- c(
  "fujian_weekly_surfacewater",
  "nmemc_marine",
  "onlimo_historical_pollution_index",
  "onlimo_daily",
  "cnemc_surfacewater"
)

# Optional short labels for presentation ------------------------------------
COLLECTOR_LABELS <- c(
  nmemc_marine = "NMEMC marine",
  fujian_weekly_surfacewater = "Fujian weekly",
  onlimo_historical_pollution_index = "ONLIMO historical IP",
  onlimo_daily = "ONLIMO daily",
  cnemc_surfacewater = "CNEMC surface water"
)

# -----------------------------
# 2. Package checks
# -----------------------------
required_packages <- c(
  "dplyr", "readr", "tibble", "ggplot2", "patchwork",
  "scales", "stringr", "tidyr", "lubridate"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them first with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(stringr)
  library(tidyr)
  library(lubridate)
})

# -----------------------------
# 3. Small helpers
# -----------------------------
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE, ...), error = function(e) NULL)
}

safe_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

safe_date <- function(x) {
  out <- suppressWarnings(as.Date(x))
  if (all(is.na(out))) return(as.Date(NA))
  out
}

safe_datetime <- function(x, tz = "Asia/Shanghai") {
  out <- suppressWarnings(as.POSIXct(x, tz = tz))
  if (all(is.na(out))) return(as.POSIXct(NA))
  out
}

fmt_n <- function(x) {
  ifelse(is.na(x), "NA", scales::comma(as.numeric(x)))
}

latest_mtime <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0L) return(as.POSIXct(NA))
  mt <- file.info(paths)$mtime
  if (all(is.na(mt))) return(as.POSIXct(NA))
  max(mt, na.rm = TRUE)
}

norm_status <- function(x) {
  x <- as.character(x)
  out <- ifelse(grepl("^PASS_WITH", x), "PASS_WITH_SOURCE_REVISION_CONTEXT",
         ifelse(grepl("^PASS_PARTIAL", x), "PASS_PARTIAL_CATCHUP",
         ifelse(grepl("^PASS_INITIAL", x), "PASS_INITIAL",
         ifelse(grepl("^PASS$", x), "PASS",
         ifelse(grepl("^REVIEW", x), "REVIEW",
         ifelse(grepl("^FAIL", x), "FAIL", "UNKNOWN"))))))
  out
}

collector_label_lookup <- function(id, fallback = id) {
  unname(COLLECTOR_LABELS[id] %||% fallback)
}

base_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", colour = COLORS$title, size = 16),
      plot.subtitle = element_text(size = 10, colour = COLORS$muted),
      plot.caption = element_text(size = 9, colour = COLORS$muted),
      axis.title = element_text(face = "bold", colour = COLORS$text),
      axis.text = element_text(colour = COLORS$text),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = COLORS$grid, linewidth = 0.25),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
}

save_panel <- function(plot_obj, filename, width = 8, height = 4.8) {
  ggsave(
    filename = file.path(OUTPUT_DIR, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = OUTPUT_DPI,
    bg = COLORS$background
  )
}

# -----------------------------
# 4. Read registry
# -----------------------------
collector_registry_path <- file.path(REGISTRY_DIR, "collector_registry.csv")
validation_log_path <- file.path(REGISTRY_DIR, "validation_log.csv")

registry <- safe_read_csv(collector_registry_path)
validation_log <- safe_read_csv(validation_log_path)

if (is.null(registry)) {
  stop("Could not read collector registry: ", collector_registry_path, call. = FALSE)
}

# -----------------------------
# 5. Collector-specific inspectors
# -----------------------------
inspect_fujian <- function() {
  source_dir <- file.path(PROJECT_DIR, "fujian_surfacewater", "data", "source")
  processed_master <- file.path(PROJECT_DIR, "fujian_surfacewater", "data", "processed", "fujian_weekly_surfacewater_master.csv.gz")

  annual_files <- list.files(source_dir, pattern = "^fujian_weekly_[0-9]{4}\\.rds$", full.names = TRUE)
  years <- suppressWarnings(as.integer(sub("^.*fujian_weekly_([0-9]{4})\\.rds$", "\\1", annual_files)))
  dat <- safe_read_csv(processed_master)

  start_date <- as.Date(NA)
  end_date <- as.Date(NA)
  rows <- NA_real_
  stations <- NA_real_
  latest_update <- latest_mtime(c(annual_files, processed_master))

  if (!is.null(dat) && nrow(dat) > 0L) {
    rows <- nrow(dat)
    stations <- dplyr::n_distinct(dat$station_name)
    date_candidates <- c("report_period_end", "report_period_start")
    for (nm in date_candidates) {
      if (nm %in% names(dat)) {
        dx <- safe_date(dat[[nm]])
        if (any(!is.na(dx))) {
          if (is.na(start_date)) start_date <- min(dx, na.rm = TRUE)
          end_date <- max(dx, na.rm = TRUE)
          break
        }
      }
    }
  }

  if (is.na(start_date) && length(years) > 0L) start_date <- as.Date(sprintf("%d-01-01", min(years, na.rm = TRUE)))
  if (is.na(end_date) && length(years) > 0L) end_date <- as.Date(sprintf("%d-12-31", max(years, na.rm = TRUE)))

  tibble(
    collector_id = "fujian_weekly_surfacewater",
    start_date = start_date,
    end_date = end_date,
    row_count = rows,
    station_count = stations,
    file_count = length(annual_files),
    run_count = NA_real_,
    latest_update = latest_update,
    detail_label = paste0(fmt_n(rows), " rows | ", fmt_n(length(annual_files)), " annual files")
  )
}

inspect_nmemc_marine <- function() {
  processed_manifest <- file.path(PROJECT_DIR, "nmemc", "data", "processed", "nmemc_water_manifest.csv")
  master_path <- file.path(PROJECT_DIR, "nmemc", "data", "processed", "nmemc_water_master.csv.gz")
  raw_dir <- file.path(PROJECT_DIR, "nmemc", "data", "raw")

  manifest <- safe_read_csv(processed_manifest)
  master <- safe_read_csv(master_path)
  raw_files <- list.files(raw_dir, pattern = "^water[0-9]{4}\\.json$", full.names = TRUE)

  years <- numeric(0)
  if (!is.null(manifest) && "year" %in% names(manifest)) {
    years <- suppressWarnings(as.integer(manifest$year))
  } else if (length(raw_files) > 0L) {
    years <- suppressWarnings(as.integer(sub("^.*water([0-9]{4})\\.json$", "\\1", raw_files)))
  }

  rows <- if (!is.null(master)) nrow(master) else NA_real_
  stations <- if (!is.null(master) && "site_code" %in% names(master)) dplyr::n_distinct(master$site_code) else NA_real_
  latest_update <- latest_mtime(c(processed_manifest, master_path, raw_files))

  start_date <- if (length(years) > 0L) as.Date(sprintf("%d-01-01", min(years, na.rm = TRUE))) else as.Date(NA)
  end_date <- if (length(years) > 0L) as.Date(sprintf("%d-12-31", max(years, na.rm = TRUE))) else as.Date(NA)

  tibble(
    collector_id = "nmemc_marine",
    start_date = start_date,
    end_date = end_date,
    row_count = rows,
    station_count = stations,
    file_count = length(raw_files),
    run_count = NA_real_,
    latest_update = latest_update,
    detail_label = paste0(fmt_n(rows), " rows | ", fmt_n(length(raw_files)), " annual files")
  )
}

inspect_onlimo_historical <- function() {
  data_path <- file.path(PROJECT_DIR, "onlimo", "data", "onlimo_pollution_index_historical.csv")
  ledger_path <- file.path(PROJECT_DIR, "onlimo", "data", "onlimo_pollution_index_request_ledger.csv")
  dat <- safe_read_csv(data_path, col_types = cols(.default = col_guess(), date = col_date(), retrieved_at = col_character()))
  ledger <- safe_read_csv(ledger_path)

  rows <- if (!is.null(dat)) nrow(dat) else NA_real_
  stations <- if (!is.null(dat) && "station_id" %in% names(dat)) dplyr::n_distinct(dat$station_id) else NA_real_
  start_date <- if (!is.null(dat) && "date" %in% names(dat)) min(as.Date(dat$date), na.rm = TRUE) else as.Date(NA)
  end_date <- if (!is.null(dat) && "date" %in% names(dat)) max(as.Date(dat$date), na.rm = TRUE) else as.Date(NA)
  completed_blocks <- if (!is.null(ledger) && all(c("request_status") %in% names(ledger))) sum(ledger$request_status %in% c("ok_data", "ok_empty"), na.rm = TRUE) else NA_real_
  latest_update <- latest_mtime(c(data_path, ledger_path))

  tibble(
    collector_id = "onlimo_historical_pollution_index",
    start_date = start_date,
    end_date = end_date,
    row_count = rows,
    station_count = stations,
    file_count = 2,
    run_count = completed_blocks,
    latest_update = latest_update,
    detail_label = paste0(fmt_n(rows), " rows | ", fmt_n(stations), " stations")
  )
}

inspect_onlimo_daily <- function() {
  data_path <- file.path(PROJECT_DIR, "onlimo", "data", "onlimo_daily_parameters_archive.csv")
  catalog_path <- file.path(PROJECT_DIR, "onlimo", "data", "onlimo_station_catalog.csv")
  dat <- safe_read_csv(data_path, col_types = cols(.default = col_guess(), date = col_date(), retrieved_at = col_character()))
  catalog <- safe_read_csv(catalog_path)

  rows <- if (!is.null(dat)) nrow(dat) else NA_real_
  stations <- if (!is.null(dat) && "station_id" %in% names(dat)) dplyr::n_distinct(dat$station_id) else NA_real_
  start_date <- if (!is.null(dat) && "date" %in% names(dat) && nrow(dat) > 0L) min(as.Date(dat$date), na.rm = TRUE) else as.Date(NA)
  end_date <- if (!is.null(dat) && "date" %in% names(dat) && nrow(dat) > 0L) max(as.Date(dat$date), na.rm = TRUE) else as.Date(NA)
  catalog_n <- if (!is.null(catalog) && "station_id" %in% names(catalog)) dplyr::n_distinct(catalog$station_id) else NA_real_
  latest_update <- latest_mtime(c(data_path, catalog_path))

  tibble(
    collector_id = "onlimo_daily",
    start_date = start_date,
    end_date = end_date,
    row_count = rows,
    station_count = stations,
    file_count = 2,
    run_count = catalog_n,
    latest_update = latest_update,
    detail_label = paste0(fmt_n(rows), " rows | ", fmt_n(stations), " stations")
  )
}

inspect_cnemc <- function() {
  processed_dir <- file.path(PROJECT_DIR, "nmemc", "data", "surfacewater", "processed")
  master_rds <- file.path(processed_dir, "nmemc_surfacewater_observations.rds")
  run_manifest <- file.path(processed_dir, "nmemc_surfacewater_run_manifest.csv")
  current_csv <- file.path(processed_dir, "nmemc_surfacewater_current.csv.gz")

  master <- safe_read_rds(master_rds)
  manifest <- safe_read_csv(run_manifest)
  current <- safe_read_csv(current_csv)

  rows <- if (!is.null(master)) nrow(master) else NA_real_
  run_count <- if (!is.null(manifest)) nrow(manifest) else NA_real_
  stations <- if (!is.null(current) && "monitoring_section" %in% names(current)) dplyr::n_distinct(current$monitoring_section) else NA_real_
  latest_update <- latest_mtime(c(master_rds, run_manifest, current_csv))

  start_date <- as.Date(NA)
  end_date <- as.Date(NA)
  if (!is.null(master) && "observation_datetime" %in% names(master)) {
    dt <- safe_datetime(master$observation_datetime)
    if (any(!is.na(dt))) {
      start_date <- as.Date(min(dt, na.rm = TRUE))
      end_date <- as.Date(max(dt, na.rm = TRUE))
    }
  }
  if (is.na(start_date) && !is.null(manifest) && "collected_at" %in% names(manifest)) {
    dt <- safe_datetime(manifest$collected_at)
    if (any(!is.na(dt))) {
      start_date <- as.Date(min(dt, na.rm = TRUE))
      end_date <- as.Date(max(dt, na.rm = TRUE))
    }
  }

  tibble(
    collector_id = "cnemc_surfacewater",
    start_date = start_date,
    end_date = end_date,
    row_count = rows,
    station_count = stations,
    file_count = NA_real_,
    run_count = run_count,
    latest_update = latest_update,
    detail_label = paste0(fmt_n(rows), " row versions | ", fmt_n(run_count), " runs")
  )
}

inspect_all_collectors <- function() {
  bind_rows(
    inspect_fujian(),
    inspect_nmemc_marine(),
    inspect_onlimo_historical(),
    inspect_onlimo_daily(),
    inspect_cnemc()
  )
}

# -----------------------------
# 6. Build monitoring tables
# -----------------------------
monitor_tbl <- inspect_all_collectors() %>%
  left_join(registry, by = "collector_id") %>%
  mutate(
    collector_label = ifelse(!is.na(collector_label), collector_label, collector_id),
    collector_short = unname(ifelse(collector_id %in% names(COLLECTOR_LABELS), COLLECTOR_LABELS[collector_id], collector_label)),
    collector_short = factor(collector_short, levels = rev(collector_label_lookup(COLLECTOR_ORDER, COLLECTOR_ORDER))),
    collector_order = match(collector_id, COLLECTOR_ORDER),
    validation_status_group = norm_status(latest_validation_status),
    windows_task_state = ifelse(is.na(windows_task_state), "unknown", windows_task_state),
    operational_role = ifelse(is.na(operational_role), "unknown", operational_role),
    latest_update_date = as.Date(latest_update),
    coverage_days = as.numeric(end_date - start_date) + 1,
    years_span = ifelse(!is.na(start_date) & !is.na(end_date),
                        paste0(format(start_date, "%Y-%m-%d"), " – ", format(end_date, "%Y-%m-%d")),
                        "Not available"),
    metric_summary_short = str_wrap(validation_metric_summary %||% "", width = 58),
    next_action_short = str_wrap(next_action %||% "", width = 55)
  ) %>%
  arrange(collector_order)

readr::write_csv(monitor_tbl, file.path(OUTPUT_DIR, "collector_monitoring_summary.csv"), na = "")

# -----------------------------
# 7. Plot data preparation
# -----------------------------
# Coverage timeline
coverage_tbl <- monitor_tbl %>%
  filter(!is.na(start_date), !is.na(end_date)) %>%
  mutate(
    collector_short = factor(collector_short, levels = rev(unique(as.character(collector_short)))),
    color_key = collector_id
  )

# Archive size bars
volume_tbl <- monitor_tbl %>%
  mutate(
    metric_value = ifelse(!is.na(row_count), row_count, file_count),
    metric_type = ifelse(!is.na(row_count), "rows/keys", "files"),
    collector_short = factor(collector_short, levels = rev(as.character(collector_short))),
    label = paste0(detail_label, ifelse(!is.na(latest_update_date), paste0("\nupdated ", latest_update_date), ""))
  )

# Status matrix
status_tbl <- monitor_tbl %>%
  transmute(
    collector_short,
    `Validation` = latest_validation_status,
    `Windows` = windows_task_state,
    `Role` = operational_role
  ) %>%
  pivot_longer(cols = c(`Validation`, `Windows`, `Role`), names_to = "dimension", values_to = "value") %>%
  mutate(
    collector_short = factor(collector_short, levels = rev(collector_label_lookup(COLLECTOR_ORDER, COLLECTOR_ORDER))),
    fill_group = case_when(
      dimension == "Validation" ~ norm_status(value),
      dimension == "Windows" ~ ifelse(value %in% names(COLORS$windows), value, "unknown"),
      dimension == "Role" ~ ifelse(value %in% names(COLORS$role), value, "unknown"),
      TRUE ~ "unknown"
    ),
    fill_color = case_when(
      dimension == "Validation" ~ COLORS$status[fill_group],
      dimension == "Windows" ~ COLORS$windows[fill_group],
      dimension == "Role" ~ COLORS$role[fill_group],
      TRUE ~ COLORS$windows[["unknown"]]
    ),
    display_value = case_when(
      dimension == "Validation" ~ str_replace_all(value, "_", "\n"),
      dimension == "Windows" ~ str_replace_all(value, "_", "\n"),
      dimension == "Role" ~ str_replace_all(value, "_", "\n"),
      TRUE ~ value
    )
  )

# Next-action panel
notes_tbl <- monitor_tbl %>%
  transmute(
    collector_short = collector_label_lookup(collector_id, collector_label),
    note = next_action_short,
    idx = row_number()
  )

# -----------------------------
# 8. Build plots
# -----------------------------
# Panel A: Coverage timeline -------------------------------------------------
p_coverage <- ggplot(coverage_tbl, aes(x = start_date, xend = end_date, y = collector_short, yend = collector_short)) +
  geom_segment(aes(colour = collector_id), linewidth = 7, lineend = "round") +
  geom_point(aes(x = end_date, colour = collector_id), size = 3) +
  geom_text(
    aes(
      x = end_date,
      y = collector_short,
      label = paste0("  ", detail_label)
    ),
    hjust = 0,
    size = 3.3,
    colour = COLORS$text,
    family = "sans"
  ) +
  scale_colour_manual(values = COLORS$timeline_fill, guide = "none") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = expansion(mult = c(0.02, 0.20))) +
  labs(
    title = "A. Temporal coverage of collected archives",
    subtitle = "Historical backbone, daily archives, and near-real-time monitoring occupy different temporal niches.",
    x = NULL,
    y = NULL
  ) +
  base_theme()

# Panel B: Archive volume ----------------------------------------------------
p_volume <- ggplot(volume_tbl, aes(x = metric_value, y = collector_short, fill = validation_status_group)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = label), hjust = -0.02, size = 3.2, colour = COLORS$text, lineheight = 0.95) +
  scale_fill_manual(values = COLORS$status, guide = "none") +
  labs(
    title = "B. Archive volume currently retained",
    subtitle = if (USE_LOG10_VOLUME) {
      "Bar lengths are shown on a log10 scale to keep large and small archives readable."
    } else {
      "Bar lengths are shown on a natural scale; labels indicate rows/keys and other useful counts."
    },
    x = if (USE_LOG10_VOLUME) "Rows / keys retained (log10 scale)" else "Rows / keys retained",
    y = NULL
  ) +
  base_theme()
if (USE_LOG10_VOLUME) {
  p_volume <- p_volume +
    scale_x_continuous(trans = "log10", labels = label_number(big.mark = ","), expand = expansion(mult = c(0, 0.22)))
} else {
  p_volume <- p_volume +
    scale_x_continuous(labels = label_number(big.mark = ","), expand = expansion(mult = c(0, 0.22)))
}

# Panel C: Status matrix -----------------------------------------------------
p_status <- ggplot(status_tbl, aes(x = dimension, y = collector_short)) +
  geom_tile(aes(fill = fill_color), colour = "white", linewidth = 0.8, width = 0.95, height = 0.85) +
  geom_text(aes(label = display_value), size = 3.1, colour = "white", fontface = "bold", lineheight = 0.9) +
  scale_fill_identity() +
  labs(
    title = "C. Operational validation status",
    subtitle = "Validation result, current Windows task state, and collector operating role.",
    x = NULL,
    y = NULL
  ) +
  base_theme() +
  theme(panel.grid = element_blank())

# Panel D: Next actions ------------------------------------------------------
max_lines <- max(str_count(notes_tbl$note, "\n") + 1, na.rm = TRUE)
row_spacing <- 1.25
n_notes <- nrow(notes_tbl)
y_positions <- rev(seq(1, n_notes * row_spacing, by = row_spacing))
notes_tbl$y <- y_positions

p_notes <- ggplot(notes_tbl, aes(x = 0, y = y)) +
  geom_text(aes(label = collector_short), hjust = 0, vjust = 1, fontface = "bold", size = 4.0, colour = COLORS$title) +
  geom_text(aes(x = 0.02, y = y - 0.38, label = note), hjust = 0, vjust = 1, size = 3.25, lineheight = 1.0, colour = COLORS$text) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, max(y_positions) + 0.8), clip = "off") +
  labs(
    title = "D. Immediate monitoring priorities",
    subtitle = "What to watch next before the next collector-retirement decision.",
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", colour = COLORS$title, size = 16),
    plot.subtitle = element_text(size = 10, colour = COLORS$muted),
    plot.margin = margin(10, 10, 10, 10)
  )

# Compose dashboard ----------------------------------------------------------
header_text <- paste0(
  "Updated: ", format(Sys.time(), "%Y-%m-%d %H:%M"),
  " | Registry validations: ", if (!is.null(validation_log)) nrow(validation_log) else 0,
  " | Collectors tracked: ", nrow(registry)
)

p_dashboard <- (p_coverage / p_volume) | (p_status / p_notes)

p_dashboard <- p_dashboard +
  plot_annotation(
    title = FIGURE_TITLE,
    subtitle = paste(FIGURE_SUBTITLE, header_text, sep = "\n"),
    caption = FIGURE_CAPTION,
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, colour = COLORS$title),
      plot.subtitle = element_text(size = 11, colour = COLORS$muted),
      plot.caption = element_text(size = 9, colour = COLORS$muted)
    )
  )

# -----------------------------
# 9. Save outputs
# -----------------------------
ggsave(
  filename = file.path(OUTPUT_DIR, paste0(OUTPUT_PREFIX, ".png")),
  plot = p_dashboard,
  width = OUTPUT_WIDTH,
  height = OUTPUT_HEIGHT,
  dpi = OUTPUT_DPI,
  bg = COLORS$background
)

if (isTRUE(SAVE_PDF)) {
  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(OUTPUT_PREFIX, ".pdf")),
    plot = p_dashboard,
    width = OUTPUT_WIDTH,
    height = OUTPUT_HEIGHT,
    bg = COLORS$background,
    device = pdf_device
  )
}

if (isTRUE(SAVE_PANELS)) {
  save_panel(p_coverage, "panel_coverage_timeline.png", width = 10, height = 4.8)
  save_panel(p_volume, "panel_archive_volume.png", width = 10, height = 4.8)
  save_panel(p_status, "panel_status_matrix.png", width = 8, height = 4.8)
  save_panel(p_notes, "panel_next_actions.png", width = 9, height = 5.5)
}

message("Dashboard written to: ", OUTPUT_DIR)
message("Main PNG: ", file.path(OUTPUT_DIR, paste0(OUTPUT_PREFIX, ".png")))
if (isTRUE(SAVE_PDF)) {
  message("Main PDF: ", file.path(OUTPUT_DIR, paste0(OUTPUT_PREFIX, ".pdf")))
}

invisible(monitor_tbl)