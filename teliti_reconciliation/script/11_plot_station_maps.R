# ============================================================
# 11_plot_station_maps.R
#
# Station-coordinate maps from the canonical analysis layer.
# Run 12_build_analysis_datasets.R first.
#
# Overview layout:
#   title + station-coordinate summary subtitle
#   row 1 = China
#   row 2 = Fujian
#   row 3 = Indonesia
# ============================================================

options(stringsAsFactors = FALSE)

# ----------------------------------------------------------------------------
# 1. Configuration
# ----------------------------------------------------------------------------
PROJECT_DIR <- "D:/# R Project/penelitian"
ANALYSIS_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation", "analysis")
OUTPUT_DIR <- file.path(PROJECT_DIR, "teliti_reconciliation", "output", "station_maps")
STATION_METADATA <- file.path(ANALYSIS_DIR, "station_metadata.rds")

OUTPUT_PREFIX <- "station_maps_overview"
OUTPUT_WIDTH <- 13.5
OUTPUT_HEIGHT <- 15.5
OUTPUT_DPI <- 320
SAVE_PDF <- TRUE
SAVE_PANELS <- TRUE

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

BBOX_CHINA <- c(xmin = 73, xmax = 135, ymin = 18, ymax = 54)
BBOX_FUJIAN <- c(xmin = 116, xmax = 121.5, ymin = 22.6, ymax = 28.7)
BBOX_INDONESIA <- c(xmin = 94, xmax = 142, ymin = -12, ymax = 7)

CITY_LABELS <- data.frame(
  city = c("Xiamen", "Jakarta"),
  longitude = c(118.0894, 106.8456),
  latitude = c(24.4798, -6.2088),
  stringsAsFactors = FALSE
)

# Network identity is intentionally encoded with BOTH shape and fill.
# Colors are Okabe-Ito inspired and remain distinct in common color-vision
# deficiencies. CNEMC is smaller/lighter because it can be much denser.
NETWORK_COLORS <- c(
  "Fujian weekly" = "#009E73",
  "NMEMC marine" = "#0072B2",
  "CNEMC surface water" = "#D55E00",
  "ONLIMO" = "#CC79A7"
)

NETWORK_SHAPES <- c(
  "Fujian weekly" = 24,
  "NMEMC marine" = 22,
  "CNEMC surface water" = 21,
  "ONLIMO" = 21
)

NETWORK_SIZES <- c(
  "Fujian weekly" = 3.4,
  "NMEMC marine" = 3.0,
  "CNEMC surface water" = 1.75,
  "ONLIMO" = 2.55
)

NETWORK_ALPHA <- c(
  "Fujian weekly" = 0.95,
  "NMEMC marine" = 0.90,
  "CNEMC surface water" = 0.52,
  "ONLIMO" = 0.78
)

LAND_FILL <- "#F3F1EA"
COAST_COLOR <- "#7A7A7A"
GRID_COLOR <- "#DEDEDE"
TEXT_COLOR <- "#202020"
MUTED_COLOR <- "#666666"
TITLE_COLOR <- "#17365D"
POINT_BORDER <- "#202020"

# ----------------------------------------------------------------------------
# 2. Packages
# ----------------------------------------------------------------------------
required_packages <- c("dplyr", "ggplot2", "patchwork", "maps", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

# ----------------------------------------------------------------------------
# 3. Read canonical station metadata
# ----------------------------------------------------------------------------
if (!file.exists(STATION_METADATA)) {
  stop(
    "Canonical station metadata not found: ", STATION_METADATA,
    "\nRun teliti_reconciliation/script/12_build_analysis_datasets.R first.",
    call. = FALSE
  )
}

stations <- readRDS(STATION_METADATA) %>%
  tibble::as_tibble()

required <- c("network", "station_key", "station_name", "longitude", "latitude")
missing <- setdiff(required, names(stations))
if (length(missing) > 0L) {
  stop(
    "station_metadata.rds is missing required field(s): ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

if (!"in_historical_ip" %in% names(stations)) {
  stations$in_historical_ip <- FALSE
}

stations <- stations %>%
  mutate(
    network = factor(as.character(network), levels = names(NETWORK_COLORS)),
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude = suppressWarnings(as.numeric(latitude)),
    in_historical_ip = as.logical(in_historical_ip),
    coordinate_valid = (
      is.finite(longitude) & is.finite(latitude) &
        longitude >= -180 & longitude <= 180 &
        latitude >= -90 & latitude <= 90
    )
  )

mapped <- stations %>% filter(coordinate_valid)

if (nrow(mapped) == 0L) {
  stop("Canonical station metadata contains no valid station coordinates.", call. = FALSE)
}

# ----------------------------------------------------------------------------
# 4. Basemap
# ----------------------------------------------------------------------------
# IMPORTANT: keep complete polygon groups. Do not filter polygon vertices to a
# bounding box before geom_polygon(), because doing so can connect surviving
# vertices across the crop and create diagonal coastline/land artifacts.
world <- ggplot2::map_data("world")

base_map_theme <- function() {
  theme_minimal(base_size = 11.5) +
    theme(
      plot.title = element_text(face = "bold", size = 15, colour = TITLE_COLOR),
      plot.subtitle = element_text(size = 10.2, colour = MUTED_COLOR),
      axis.title = element_blank(),
      axis.text = element_text(size = 9, colour = MUTED_COLOR),
      panel.grid.major = element_line(colour = GRID_COLOR, linewidth = 0.25),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      plot.margin = margin(8, 10, 8, 10)
    )
}

add_land <- function(p) {
  p +
    geom_polygon(
      data = world,
      aes(x = long, y = lat, group = group),
      inherit.aes = FALSE,
      fill = LAND_FILL,
      colour = COAST_COLOR,
      linewidth = 0.28
    )
}

add_network_points <- function(p, dat, show_legend = TRUE) {
  if (nrow(dat) == 0L) return(p)

  p +
    geom_point(
      data = dat,
      aes(
        x = longitude,
        y = latitude,
        fill = network,
        shape = network,
        size = network,
        alpha = network
      ),
      colour = POINT_BORDER,
      stroke = 0.45
    ) +
    scale_fill_manual(values = NETWORK_COLORS, drop = FALSE) +
    scale_shape_manual(values = NETWORK_SHAPES, drop = FALSE) +
    scale_size_manual(values = NETWORK_SIZES, guide = "none", drop = FALSE) +
    scale_alpha_manual(values = NETWORK_ALPHA, guide = "none", drop = FALSE) +
    guides(
      fill = guide_legend(
        override.aes = list(
          size = 3.5,
          alpha = 0.95,
          colour = POINT_BORDER
        )
      ),
      shape = "none"
    ) +
    theme(legend.position = if (show_legend) "bottom" else "none")
}

add_city_marker <- function(p, city_name) {
  city <- CITY_LABELS[CITY_LABELS$city == city_name, , drop = FALSE]
  if (nrow(city) == 0L) return(p)

  p +
    geom_point(
      data = city,
      aes(x = longitude, y = latitude),
      inherit.aes = FALSE,
      shape = 4,
      size = 3.2,
      stroke = 1.0,
      colour = TITLE_COLOR
    ) +
    geom_text(
      data = city,
      aes(x = longitude, y = latitude, label = city),
      inherit.aes = FALSE,
      hjust = -0.12,
      vjust = -0.45,
      size = 3.5,
      fontface = "bold",
      colour = TITLE_COLOR
    )
}

# ----------------------------------------------------------------------------
# 5. Panel subsets
# ----------------------------------------------------------------------------
china_points <- mapped %>%
  filter(
    longitude >= BBOX_CHINA[["xmin"]], longitude <= BBOX_CHINA[["xmax"]],
    latitude >= BBOX_CHINA[["ymin"]], latitude <= BBOX_CHINA[["ymax"]],
    as.character(network) %in% c(
      "Fujian weekly", "NMEMC marine", "CNEMC surface water"
    )
  )

fujian_points <- mapped %>%
  filter(
    longitude >= BBOX_FUJIAN[["xmin"]], longitude <= BBOX_FUJIAN[["xmax"]],
    latitude >= BBOX_FUJIAN[["ymin"]], latitude <= BBOX_FUJIAN[["ymax"]],
    as.character(network) %in% c(
      "Fujian weekly", "NMEMC marine", "CNEMC surface water"
    )
  )

indonesia_points <- mapped %>%
  filter(
    as.character(network) == "ONLIMO",
    longitude >= BBOX_INDONESIA[["xmin"]], longitude <= BBOX_INDONESIA[["xmax"]],
    latitude >= BBOX_INDONESIA[["ymin"]], latitude <= BBOX_INDONESIA[["ymax"]]
  )

historical_onlimo <- indonesia_points %>% filter(in_historical_ip %in% TRUE)

# ----------------------------------------------------------------------------
# 6. Overview subtitle
# ----------------------------------------------------------------------------
count_network <- function(name) {
  sum(as.character(mapped$network) == name, na.rm = TRUE)
}

summary_subtitle <- paste0(
  "Stations with recovered coordinates — ",
  "Fujian weekly: ", comma(count_network("Fujian weekly")),
  " | NMEMC marine: ", comma(count_network("NMEMC marine")),
  " | CNEMC: ", comma(count_network("CNEMC surface water")),
  " | ONLIMO: ", comma(count_network("ONLIMO"))
)

# ----------------------------------------------------------------------------
# 7. Map panels
# ----------------------------------------------------------------------------
p_china <- ggplot()
p_china <- add_land(p_china)
p_china <- add_network_points(p_china, china_points, show_legend = TRUE)
p_china <- add_city_marker(p_china, "Xiamen")
p_china <- p_china +
  coord_quickmap(
    xlim = c(BBOX_CHINA[["xmin"]], BBOX_CHINA[["xmax"]]),
    ylim = c(BBOX_CHINA[["ymin"]], BBOX_CHINA[["ymax"]]),
    expand = FALSE,
    clip = "on"
  ) +
  labs(
    title = "China",
    subtitle = paste0(
      "National and regional monitoring stations with recovered coordinates (n = ",
      comma(nrow(china_points)), ")."
    )
  ) +
  base_map_theme()

p_fujian <- ggplot()
p_fujian <- add_land(p_fujian)
p_fujian <- add_network_points(p_fujian, fujian_points, show_legend = FALSE)
p_fujian <- add_city_marker(p_fujian, "Xiamen")
p_fujian <- p_fujian +
  coord_quickmap(
    xlim = c(BBOX_FUJIAN[["xmin"]], BBOX_FUJIAN[["xmax"]]),
    ylim = c(BBOX_FUJIAN[["ymin"]], BBOX_FUJIAN[["ymax"]]),
    expand = FALSE,
    clip = "on"
  ) +
  labs(
    title = "Fujian",
    subtitle = paste0(
      "Detailed inland-to-coastal station coverage around the Xiamen case-study region (n = ",
      comma(nrow(fujian_points)), ")."
    )
  ) +
  base_map_theme() +
  theme(legend.position = "none")

p_indonesia <- ggplot()
p_indonesia <- add_land(p_indonesia)
p_indonesia <- add_network_points(p_indonesia, indonesia_points, show_legend = FALSE)
if (nrow(historical_onlimo) > 0L) {
  p_indonesia <- p_indonesia +
    geom_point(
      data = historical_onlimo,
      aes(x = longitude, y = latitude),
      inherit.aes = FALSE,
      shape = 21,
      fill = NA,
      colour = "black",
      stroke = 0.9,
      size = 3.4
    )
}
p_indonesia <- add_city_marker(p_indonesia, "Jakarta")
p_indonesia <- p_indonesia +
  coord_quickmap(
    xlim = c(BBOX_INDONESIA[["xmin"]], BBOX_INDONESIA[["xmax"]]),
    ylim = c(BBOX_INDONESIA[["ymin"]], BBOX_INDONESIA[["ymax"]]),
    expand = FALSE,
    clip = "on"
  ) +
  labs(
    title = "Indonesia",
    subtitle = paste0(
      "ONLIMO stations with coordinates (n = ", comma(nrow(indonesia_points)),
      "); black outlines identify stations represented in the historical Pollution Index archive (n = ",
      comma(nrow(historical_onlimo)), ")."
    )
  ) +
  base_map_theme() +
  theme(legend.position = "none")

# ----------------------------------------------------------------------------
# 8. Dashboard
# ----------------------------------------------------------------------------
p_dashboard <- p_china / p_fujian / p_indonesia +
  plot_layout(heights = c(1.0, 1.05, 1.0), guides = "collect") +
  plot_annotation(
    title = "Station-coordinate coverage",
    subtitle = summary_subtitle,
    caption = paste(
      "Coordinates are taken from the canonical station metadata generated by",
      "12_build_analysis_datasets.R. Missing coordinates are not imputed."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, colour = TITLE_COLOR),
      plot.subtitle = element_text(size = 11, colour = MUTED_COLOR),
      plot.caption = element_text(size = 9, colour = MUTED_COLOR),
      plot.margin = margin(10, 12, 8, 12)
    )
  ) &
  theme(legend.position = "bottom")

# ----------------------------------------------------------------------------
# 9. Save
# ----------------------------------------------------------------------------
ggsave(
  file.path(OUTPUT_DIR, paste0(OUTPUT_PREFIX, ".png")),
  p_dashboard,
  width = OUTPUT_WIDTH,
  height = OUTPUT_HEIGHT,
  dpi = OUTPUT_DPI,
  bg = "white"
)

if (isTRUE(SAVE_PDF)) {
  pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
  ggsave(
    file.path(OUTPUT_DIR, paste0(OUTPUT_PREFIX, ".pdf")),
    p_dashboard,
    width = OUTPUT_WIDTH,
    height = OUTPUT_HEIGHT,
    device = pdf_device,
    bg = "white"
  )
}

if (isTRUE(SAVE_PANELS)) {
  ggsave(file.path(OUTPUT_DIR, "panel_map_china.png"), p_china, width = 11.5, height = 5.5, dpi = OUTPUT_DPI, bg = "white")
  ggsave(file.path(OUTPUT_DIR, "panel_map_fujian.png"), p_fujian, width = 9.5, height = 6.0, dpi = OUTPUT_DPI, bg = "white")
  ggsave(file.path(OUTPUT_DIR, "panel_map_indonesia.png"), p_indonesia, width = 11.5, height = 5.5, dpi = OUTPUT_DPI, bg = "white")
}

# Lightweight diagnostic tables for presentation QA --------------------------
coordinate_summary <- stations %>%
  mutate(has_coordinate = coordinate_valid) %>%
  group_by(network) %>%
  summarise(
    stations_total = n(),
    stations_with_coordinates = sum(has_coordinate, na.rm = TRUE),
    coordinate_completeness_pct = 100 * mean(has_coordinate, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  coordinate_summary,
  file.path(OUTPUT_DIR, "station_coordinate_summary.csv"),
  na = ""
)

readr::write_csv(
  stations,
  file.path(OUTPUT_DIR, "station_coordinate_inventory.csv"),
  na = ""
)

message("Station map complete.")
message("Canonical metadata: ", STATION_METADATA)
message("Main PNG: ", file.path(OUTPUT_DIR, paste0(OUTPUT_PREFIX, ".png")))
message("Mapped stations: ", nrow(mapped), " / ", nrow(stations))
message("China panel: ", nrow(china_points))
message("Fujian panel: ", nrow(fujian_points))
message("Indonesia panel: ", nrow(indonesia_points))
message("Historical ONLIMO subset: ", nrow(historical_onlimo))