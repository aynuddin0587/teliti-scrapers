#!/usr/bin/env Rscript
# Qingyue Phase C: fixed-noon versus six-point monitoring representativeness
# Rebuilds the analysis from ORIGINAL approved observations, manifest and
# immutable 50 station-year roster. READ ONLY: no Huawei API / no SQLite edits.
#
# Requires: base R, ggplot2, sandwich, lmtest.
# Install once: install.packages(c("ggplot2", "sandwich", "lmtest"))
#
# Usage (PowerShell):
# Rscript phaseC_diurnal_representativeness.R observations.csv.gz manifest.csv.gz phaseC_selected_station_years.csv output_folder [phaseC_followup_day_results.csv]
#
# Outputs correspond in definition to the Python-validated CSV/figures supplied.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4L) {
  stop("Usage: Rscript phaseC_diurnal_representativeness.R <observations.csv.gz> <manifest.csv.gz> <roster.csv> <output_folder> [followup_day_results.csv]", call. = FALSE)
}
for (pkg in c("ggplot2", "sandwich", "lmtest")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Missing R package '%s'. Run install.packages('%s')", pkg, pkg), call. = FALSE)
  }
}
observations_path <- args[[1]]
manifest_path <- args[[2]]
roster_path <- args[[3]]
out_dir <- args[[4]]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_data <- function(path) {
  # read.csv directly supports gzipped files by filename.
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
           fileEncoding = "UTF-8", na.strings = c("", "NA", "NaN"))
}
obs <- read_data(observations_path)
manifest <- read_data(manifest_path)
roster <- read_data(roster_path)
names(roster)[1] <- sub("^\\ufeff", "", names(roster)[1])
stopifnot(nrow(roster) == 50L, length(unique(roster$station_identity_id)) == 37L)
stopifnot(nrow(manifest) == 39626L, !anyDuplicated(manifest$request_id),
          !anyDuplicated(obs$request_id))
stopifnot(sum(manifest$status == "ok") == nrow(obs))
stopifnot(setequal(manifest$request_id[manifest$status == "ok"], obs$request_id))

hours <- c(0L, 4L, 8L, 12L, 16L, 20L)
core <- c("water_temp", "ph", "dissolvedoxygen", "codmn", "nh3-n", "tp", "tn")
stopifnot(all(core %in% names(obs)))

# Generate the 50 station-year x 2 months x 6 timepoints design.
expected <- do.call(rbind, lapply(seq_len(nrow(roster)), function(i) {
  r <- roster[i, ]
  do.call(rbind, lapply(c(1L, 7L), function(m) {
    day <- sprintf("%04d-%02d-15", as.integer(r$year), m)
    data.frame(
      phase = r$phase, site = r$study_site_code,
      station_identity_id = r$station_identity_id,
      station = r$station_name, year = as.integer(r$year), month = m,
      season = if (m == 1L) "January" else "July",
      date = day, hour = hours,
      target_datetime = sprintf("%s %02d:00:00", day, hours),
      stringsAsFactors = FALSE
    )
  }))
}))
stopifnot(nrow(expected) == 600L)
key <- function(id, datetime) paste(id, datetime, sep = "||")
expected_key <- key(expected$station_identity_id, expected$target_datetime)
man_key <- key(manifest$station_identity_id, manifest$target_datetime)
obs_key <- key(obs$station_identity_id, obs$target_datetime)
stopifnot(!anyDuplicated(man_key), !anyDuplicated(obs_key))
mi <- match(expected_key, man_key)
oi <- match(expected_key, obs_key)
stopifnot(!anyNA(mi))
expected$api_status <- manifest$status[mi]
expected$request_id <- manifest$request_id[mi]
stopifnot(all(expected$api_status %in% c("ok", "no_record")))
stopifnot(identical(expected$api_status == "ok", !is.na(oi)))
stopifnot(all(expected$request_id[!is.na(oi)] == obs$request_id[oi[!is.na(oi)]]))

expected$section_status <- obs$section_status[oi]
expected$monitor_time <- obs$monitor_time[oi]
for (col in core) {
  x <- suppressWarnings(as.numeric(obs[[col]][oi]))
  x[is.finite(x) & x < 0] <- NA_real_  # source negative sentinels, not environmental measurements
  expected[[col]] <- x
}
expected$normal <- !is.na(expected$section_status) & expected$section_status == "正常"
expected$time_matches <- !is.na(expected$monitor_time) &
  expected$monitor_time == expected$target_datetime
expected$core_present <- stats::complete.cases(expected[, core, drop = FALSE])
expected$strict <- expected$api_status == "ok" & expected$normal &
  expected$time_matches & expected$core_present
expected$qc_reason <- ifelse(expected$api_status == "no_record", "NO_RECORD",
  ifelse(!expected$normal, "NON_NORMAL_STATUS",
  ifelse(!expected$time_matches, "TIMESTAMP_MISMATCH",
  ifelse(!expected$core_present, "INCOMPLETE_SEVEN_CORE", "STRICT_PASS"))))
write.csv(expected, file.path(out_dir, "R_phaseC_hourly_QC.csv"), row.names = FALSE, na = "")

# Each day requires valid measurements at all six four-hourly timestamps.
make_day <- function(g) {
  g <- g[order(g$hour), , drop = FALSE]
  stopifnot(nrow(g) == 6L, identical(as.integer(g$hour), hours))
  r <- g[which(g$hour == 12L), , drop = FALSE]
  all_strict <- all(g$strict)
  do <- g$dissolvedoxygen
  noon <- r$dissolvedoxygen[[1]]
  mean6 <- if (all_strict) mean(do) else NA_real_
  dmin <- if (all_strict) min(do) else NA_real_
  dmax <- if (all_strict) max(do) else NA_real_
  data.frame(
    phase = r$phase, site = r$site, station = r$station,
    station_identity_id = r$station_identity_id, year = r$year,
    month = r$month, season = r$season, date = r$date,
    extra_ok = sum(g$api_status[g$hour != 12L] == "ok"),
    extra_strict = sum(g$strict[g$hour != 12L]),
    noon_valid = r$strict, strict_sixpoint = all_strict,
    do_noon = if (all_strict) noon else NA_real_,
    do_six_mean = mean6, do_min = dmin, do_max = dmax,
    do_range = if (all_strict) dmax - dmin else NA_real_,
    signed_noon_mean_diff = if (all_strict) noon - mean6 else NA_real_,
    abs_noon_mean_diff = if (all_strict) abs(noon - mean6) else NA_real_,
    noon_minus_sampled_min = if (all_strict) noon - dmin else NA_real_,
    temp_range = if (all_strict) diff(range(g$water_temp)) else NA_real_,
    ph_range = if (all_strict) diff(range(g$ph)) else NA_real_,
    do_min_hour = if (all_strict) g$hour[which.min(do)] else NA_integer_,
    do_max_hour = if (all_strict) g$hour[which.max(do)] else NA_integer_,
    flag_large_do_range_gt5 = isTRUE(all_strict && (dmax - dmin) > 5),
    flag_high_do_gt20 = isTRUE(all_strict && dmax > 20),
    flag_ph_range_gt2 = isTRUE(all_strict && diff(range(g$ph)) > 2),
    stringsAsFactors = FALSE
  )
}
keys <- paste(expected$station_identity_id, expected$date, sep = "||")
days <- do.call(rbind, lapply(split(expected, keys), make_day))
rownames(days) <- NULL
stopifnot(nrow(days) == 100L, sum(days$strict_sixpoint) == 80L)
write.csv(days, file.path(out_dir, "R_phaseC_station_days.csv"), row.names = FALSE, na = "")

# Optional comparison with the uploaded independent follow-up day-level QC.
if (length(args) >= 5L && file.exists(args[[5]])) {
  provided <- read_data(args[[5]])
  follow <- days[days$phase == "followup_400", ]
  idx <- match(paste(follow$station_identity_id, follow$date),
               paste(provided$station_identity_id, provided$date))
  stopifnot(length(idx) == 80L, !anyNA(idx),
            identical(as.logical(follow$strict_sixpoint),
                      as.logical(provided$strict_sixpoint[idx])),
            all(follow$extra_ok == provided$extra_ok[idx]),
            all(follow$extra_strict == provided$extra_strict[idx]))
  good <- follow$strict_sixpoint
  stopifnot(all(abs(follow$do_range[good] - provided$DO_range[idx][good]) < 1e-7))
}

valid <- days[days$strict_sixpoint, , drop = FALSE]
jan <- valid[valid$month == 1L, , drop = FALSE]
jul <- valid[valid$month == 7L, , drop = FALSE]
stopifnot(nrow(jan) == 38L, nrow(jul) == 42L)
vars <- c("do_range", "abs_noon_mean_diff", "signed_noon_mean_diff", "temp_range")
left <- jan[, c("station_identity_id", "year", "site", "station", vars)]
right <- jul[, c("station_identity_id", "year", vars)]
names(left)[match(vars,names(left))] <- paste0("jan_", vars)
names(right)[match(vars,names(right))] <- paste0("jul_", vars)
pairs <- merge(left, right, by = c("station_identity_id", "year"))
pairs$delta_range <- pairs$jul_do_range - pairs$jan_do_range
pairs$delta_abs_noon_mean <- pairs$jul_abs_noon_mean_diff - pairs$jan_abs_noon_mean_diff
pairs$delta_temp_range <- pairs$jul_temp_range - pairs$jan_temp_range
stopifnot(nrow(pairs) == 34L, length(unique(pairs$station_identity_id)) == 29L)
write.csv(pairs, file.path(out_dir, "R_phaseC_matched_season_pairs.csv"), row.names = FALSE, na = "")

summarize_part <- function(d) {
  c(n_days = nrow(d), n_stations = length(unique(d$station_identity_id)),
    median_do_range = median(d$do_range), median_abs_noon_mean = median(d$abs_noon_mean_diff),
    median_signed_noon_mean = median(d$signed_noon_mean_diff),
    median_noon_minus_min = median(d$noon_minus_sampled_min))
}
seasons <- rbind(ALL = summarize_part(valid), January = summarize_part(jan), July = summarize_part(jul))
write.csv(seasons, file.path(out_dir, "R_phaseC_summary_by_season.csv"))
site_months <- do.call(rbind, lapply(split(valid, list(valid$site, valid$season), drop = TRUE), function(d) {
  data.frame(site = d$site[[1]], season = d$season[[1]], t(summarize_part(d)))
}))
write.csv(site_months, file.path(out_dir, "R_phaseC_summary_by_site_season.csv"), row.names = FALSE)

set.seed(20261012)
# Inference is deliberately clustered by STATION (a station may contribute multiple years).
cluster_boot_median <- function(d, field, n_boot = 4000L) {
  station_groups <- split(d[[field]], d$station_identity_id)
  labels <- names(station_groups)
  draws <- replicate(n_boot, {
    picked <- sample(labels, length(labels), replace = TRUE)
    median(unlist(station_groups[picked], use.names = FALSE))
  })
  stats::quantile(draws, c(0.025, 0.975), names = FALSE)
}
ci <- do.call(rbind, lapply(c("do_range","abs_noon_mean_diff","signed_noon_mean_diff","noon_minus_sampled_min"), function(v) {
  q <- cluster_boot_median(valid, v)
  data.frame(outcome = v, median = median(valid[[v]]), ci_low = q[1], ci_high = q[2], n_days = nrow(valid))
}))
write.csv(ci, file.path(out_dir, "R_phaseC_cluster_bootstrap_median_CI.csv"), row.names = FALSE)

# Sign-flip test and bootstrap CI use each unique station's MEAN paired contrast.
# This avoids treating multiple years at the same site as independent stations.
station_pair_test <- function(d, contrast, n_boot = 5000L, n_perm = 20000L) {
  v <- tapply(d[[contrast]], d$station_identity_id, mean)
  m <- length(v)
  boot <- replicate(n_boot, mean(sample(v, m, replace = TRUE)))
  flips <- replicate(n_perm, mean(v * sample(c(-1, 1), m, replace = TRUE)))
  c(unique_stations = m, mean_station_contrast = mean(v),
    ci_low = unname(quantile(boot, .025)), ci_high = unname(quantile(boot, .975)),
    two_sided_signflip_p = (1 + sum(abs(flips) >= abs(mean(v)))) / (n_perm + 1))
}
tests <- do.call(rbind, lapply(c("delta_range","delta_abs_noon_mean","delta_temp_range"), function(v) {
  data.frame(outcome = v, t(station_pair_test(pairs, v)))
}))
write.csv(tests, file.path(out_dir, "R_phaseC_station_cluster_paired_tests.csv"), row.names = FALSE)
signed <- data.frame(outcome = "signed_noon_minus_sixpoint_mean",
                     t(station_pair_test(valid, "signed_noon_mean_diff")))
write.csv(signed, file.path(out_dir, "R_phaseC_station_cluster_signed_bias.csv"), row.names = FALSE)

# Exploratory comparison across provisional study-area groups; unequal samples
# and repeated stations are handled using station-clustered sandwich covariance.
fit_site_model <- function(variable) {
  f <- as.formula(paste0("log1p(", variable, ") ~ factor(month) + factor(site) + factor(year)"))
  fit <- stats::lm(f, data = valid)
  v <- sandwich::vcovCL(fit, cluster = valid$station_identity_id, type = "HC1")
  ctable <- lmtest::coeftest(fit, vcov. = v)
  terms <- grep("^factor\\(site\\)", names(stats::coef(fit)))
  b <- stats::coef(fit)[terms]
  S <- v[terms, terms, drop = FALSE]
  site_wald <- as.numeric(t(b) %*% solve(S, b))
  data.frame(outcome = variable, n_days = nrow(valid), n_station_clusters = length(unique(valid$station_identity_id)),
    july_log_coefficient = ctable["factor(month)7", "Estimate"],
    july_p = ctable["factor(month)7", "Pr(>|t|)"],
    site_joint_wald = site_wald, site_df = length(b),
    site_joint_p = stats::pchisq(site_wald, df = length(b), lower.tail = FALSE))
}
models <- rbind(fit_site_model("do_range"), fit_site_model("abs_noon_mean_diff"))
write.csv(models, file.path(out_dir, "R_phaseC_station_clustered_models.csv"), row.names = FALSE)

# Sensitivity analyses: extreme profiles are flagged, never deleted from raw data.
scenario_list <- list(
  "All strict" = valid,
  "Exclude DO range >5" = valid[valid$do_range <= 5, ],
  "Exclude reported DO >20" = valid[valid$do_max <= 20, ],
  "Exclude DO range >5 or pH range >2" = valid[valid$do_range <= 5 & valid$ph_range <= 2, ]
)
sensitivity <- do.call(rbind, lapply(names(scenario_list), function(label) {
  d <- scenario_list[[label]]
  id <- paste(d$station_identity_id, d$year)
  keep <- intersect(unique(id[d$month == 1L]), unique(id[d$month == 7L]))
  p <- pairs[paste(pairs$station_identity_id, pairs$year) %in% keep, ]
  data.frame(scenario = label, valid_days = nrow(d), valid_matched_pairs = nrow(p),
    jan_median_range = median(d$do_range[d$month == 1L]),
    jul_median_range = median(d$do_range[d$month == 7L]),
    median_matched_july_minus_jan = if (nrow(p)) median(p$delta_range) else NA_real_)
}))
write.csv(sensitivity, file.path(out_dir, "R_phaseC_extreme_sensitivity.csv"), row.names = FALSE)

# Journal-style plots, exported at high resolution (DO units are NOT independently verified).
colors <- c(BHB = "#3177A8", LZB = "#D68D28", MJE = "#38915B", XMB = "#9C508C")
base_theme <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 plot.title.position = "plot", legend.position = "bottom")
plot1 <- ggplot2::ggplot(valid, ggplot2::aes(x = do_six_mean, y = do_noon, color = site, shape = season)) +
  ggplot2::geom_point(alpha = .75, size = 2.5) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::coord_equal() +
  ggplot2::labs(title = "Noon DO and six-point mean", x = "Six-point mean DO (source units)",
                y = "Noon DO (source units)", color = "Study area", shape = "Season") + base_theme
ggplot2::ggsave(file.path(out_dir, "R_Figure_01_noon_vs_mean.png"), plot1, width = 7.1, height = 5.8, dpi = 320)

plot2 <- ggplot2::ggplot(pairs, ggplot2::aes(color = site)) +
  ggplot2::geom_segment(ggplot2::aes(x = 1, xend = 2, y = jan_do_range, yend = jul_do_range), alpha = .55) +
  ggplot2::geom_point(ggplot2::aes(x = 1, y = jan_do_range), size = 2, alpha = .65) +
  ggplot2::geom_point(ggplot2::aes(x = 2, y = jul_do_range), size = 2, alpha = .65) +
  ggplot2::scale_x_continuous(breaks = c(1,2),labels = c("January","July"),limits = c(.85,2.15)) +
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::labs(title = "Matched January–July DO variation", x = NULL,
                y = "Six-point DO range (source units)", color = "Study area") + base_theme
ggplot2::ggsave(file.path(out_dir, "R_Figure_02_paired_DO_range.png"), plot2, width = 7, height = 5.5, dpi = 320)

plot3 <- ggplot2::ggplot(valid, ggplot2::aes(x = site, y = abs_noon_mean_diff, fill = season)) +
  ggplot2::geom_boxplot(outlier.shape = NA, position = ggplot2::position_dodge(width = .74), alpha = .45) +
  ggplot2::geom_jitter(ggplot2::aes(color = season), width = .17, height = 0, alpha = .6, size = 1.5) +
  ggplot2::labs(title = "Absolute noon–mean DO disagreement", x = "Provisional study area",
                y = "Absolute discrepancy (source units)", fill = "Season", color = "Season") + base_theme
ggplot2::ggsave(file.path(out_dir, "R_Figure_03_area_disagreement.png"), plot3, width = 7.5, height = 5.3, dpi = 320)

cat("PASS: 600 reconstructed timepoints, 500 extra-hour requests, 469 strict extra-hour records, ",
    "80 valid station-days and 34 January–July pairs.\n", sep = "")
cat("Results and figures saved in: ", normalizePath(out_dir), "\n", sep = "")
