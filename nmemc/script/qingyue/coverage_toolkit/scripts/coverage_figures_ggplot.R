#!/usr/bin/env Rscript
# Optional publication figures from qingyue_coverage.py compiled coverage tables.
# Read only: does not call Huawei, touch SQLite, or write to source archives.
# Usage: Rscript coverage_figures_ggplot.R "D:/.../processed/qingyue_coverage"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop("Usage: Rscript coverage_figures_ggplot.R COVERAGE_OUTPUT_DIRECTORY")
out <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
required <- c("readr", "dplyr", "tidyr", "ggplot2", "scales")
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing) > 0) stop("Install required packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2); library(scales)
})
read_table <- function(filename) read_csv(file.path(out, filename), show_col_types = FALSE)
blue <- "#18758C"
amber <- "#DDA044"
mist <- "#DDE5E8"
slate <- "#495F70"
report_theme <- function() theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#173750", size = 14),
        plot.subtitle = element_text(color = slate),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom", plot.margin = margin(12, 15, 12, 12))
write_plot <- function(p, file, w = 13, h = 7) {
  ggsave(file.path(out, paste0(file, ".png")), p, width = w, height = h,
         dpi = 250, bg = "white", limitsize = FALSE)
  ggsave(file.path(out, paste0(file, ".pdf")), p, width = w, height = h,
         device = "pdf", bg = "white", limitsize = FALSE)
}
# A) Stage accounting: resolved status != retrieved observation.
stg <- read_table("01_acquisition_by_stage.csv")
cols <- intersect(c("ok", "no_record", "skipped_noon_missing", "pending",
                    "retryable_error", "transient_exhausted", "fatal_error",
                    "application_error", "response_mismatch"), names(stg))
stg_long <- stg |>
  pivot_longer(cols = all_of(cols), names_to = "outcome", values_to = "n") |>
  mutate(stage = factor(stage, levels = rev(c("1", "2", "3", "4", "deferred-2020", "extension"))),
         outcome = factor(outcome, levels = cols))
p_stage <- ggplot(stg_long, aes(x = n, y = stage, fill = outcome)) +
  geom_col(position = "fill", width = .68) +
  scale_x_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = c(ok = blue, no_record = amber,
                               skipped_noon_missing = "#9AADB4", pending = mist,
                               retryable_error = "#BE6C72", transient_exhausted = "#BE6C72",
                               fatal_error = "#BE6C72", application_error = "#BE6C72",
                               response_mismatch = "#BE6C72"), drop = TRUE) +
  labs(title = "Historical monitoring acquisition outcomes", subtitle = "Share of original planned API requests; 100% resolved does not mean 100% observed",
       x = "Share of planned requests", y = NULL, fill = "Executor status") + report_theme()
write_plot(p_stage, "R_stage_outcomes", 12, 5)
# B) Spatial-temporal monthly-noon coverage; all months including pending and no_record.
noon <- read_table("03_noon_coverage_by_site_month.csv") |>
  mutate(month = factor(month, levels = sort(unique(month))),
         site = factor(site, levels = rev(sort(unique(site)))))
p_month <- ggplot(noon, aes(x = month, y = site, fill = strict_core_rate)) +
  geom_tile(width = .96, height = .9) +
  scale_fill_gradient(low = "#F5F0C6", high = "#0E5173", limits = c(0, 1),
                      labels = percent_format(), na.value = "#E6ECF0") +
  scale_x_discrete(breaks = function(x) x[grepl("-01$|-07$", x)]) +
  labs(title = "Spatial–temporal coverage of strict-valid noon snapshots",
       subtitle = "Denominator: planned station-month requests within each study-site group",
       x = "Month", y = "Study-site code", fill = "Seven-core\nvalidity") +
  theme_minimal(base_size = 10) + theme(plot.title = element_text(face = "bold", color = "#173750"),
    axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1), panel.grid = element_blank(),
    legend.position = "right")
write_plot(p_month, "R_site_month_heatmap", 15, 6)
# C) Parameter coverage among retrieved records: not the same as plan completeness.
param <- read_table("06_parameter_coverage_by_year.csv") |>
  mutate(year = factor(year),
         parameter = factor(parameter, levels = rev(c("water_temp", "ph", "dissolvedoxygen", "codmn",
              "nh3-n", "tp", "tn", "conductivity", "turbidity", "chlorophyll", "algal_density"))),
         component = recode(component, monthly_noon_backbone = "Monthly noon",
                            semiannual_full_day_extra = "Five extra daily hours"))
p_par <- ggplot(param, aes(year, parameter, fill = numeric_rate)) +
  geom_tile(color = "white", linewidth = .24) +
  geom_text(aes(label = ifelse(is.na(numeric_rate), "", round(numeric_rate))), size = 2.5) +
  facet_grid(~ component) +
  scale_fill_gradient(low = "#F5EFF4", high = "#006C68", limits = c(0, 100),
                      labels = function(x) paste0(x, "%"), na.value = "#E6ECF0") +
  labs(title = "Measurement availability among successfully retrieved observations",
       subtitle = "Nonnegative numeric value; pH additionally limited to 0–14; QC status remains a separate column",
       x = "Scheduled year", y = NULL, fill = "Numeric\navailability") +
  theme_minimal(base_size = 10) + theme(plot.title = element_text(face = "bold", color = "#173750"),
  panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
write_plot(p_par, "R_parameter_coverage", 13, 6.2)
# D) Six-point completeness, following the plan's Jan/Jul dates only.
diurnal <- read_table("10_sixpoint_coverage_by_site_season.csv") |>
  group_by(year, season) |>
  summarise(planned = sum(planned), all_retrieved = sum(all_retrieved),
            strict_six = sum(strict_six), .groups = "drop") |>
  mutate(period = paste(year, season)) |>
  pivot_longer(c(all_retrieved, strict_six), names_to = "metric", values_to = "count") |>
  mutate(share = ifelse(planned > 0, count / planned, NA_real_),
         metric = recode(metric, all_retrieved = "All six retrieved", strict_six = "Six strictly valid"),
         period = factor(period, levels = unique(period)))
p_d <- ggplot(diurnal, aes(period, share, fill = metric)) +
  geom_col(position = position_dodge2(width = .8, preserve = "single"), width = .76) +
  scale_y_continuous(limits = c(0, 1.05), labels = percent_format()) +
  scale_fill_manual(values = c("All six retrieved" = "#7AB0C5", "Six strictly valid" = "#188676")) +
  labs(title = "Completeness of six-point January–July profiles",
       subtitle = "Each planned station-day requires noon plus five additional monitoring hours",
       x = "Scheduled period", y = "Complete station-days / planned station-days", fill = NULL) +
  report_theme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
write_plot(p_d, "R_sixpoint_completeness", 13, 5.2)
cat("PASS: ggplot2 figures saved in", out, "\n")
