#!/usr/bin/env Rscript
# Qingyue Stage 4, four Fujian-labelled study-area groups, 2021-2025.
# Offline and read-only: NO Huawei calls and NO SQLite operations.
# Usage:
# Rscript fujian_stage4_analysis.R observations.csv.gz manifest.csv.gz output_dir [priority_1290_ids.txt]
# Base R analyses always run. ggplot2 is optional for publication figures.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop('Usage: Rscript fujian_stage4_analysis.R <observations.csv.gz> <manifest.csv.gz> <output_dir> [priority_1290_ids.txt]', call. = FALSE)
}
obs_path <- args[[1]]
manifest_path <- args[[2]]
out_dir <- args[[3]]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
set.seed(20261012)

read_export <- function(path) {
  read.csv(path, check.names = FALSE, fileEncoding = 'UTF-8',
           stringsAsFactors = FALSE, na.strings = c('', 'NA', 'NaN'))
}
write_output <- function(x, filename) {
  write.csv(x, file.path(out_dir, filename), row.names = FALSE, na = '', fileEncoding = 'UTF-8')
}
key <- function(id, datetime) paste(id, datetime, sep = '||')
core <- c('water_temp', 'ph', 'dissolvedoxygen', 'codmn', 'nh3-n', 'tp', 'tn')
sites <- c('XMB', 'MJE', 'QZB', 'DSB')
hours <- c(0L, 4L, 8L, 12L, 16L, 20L)
normal_status <- '\u6b63\u5e38'

man <- read_export(manifest_path)
obs <- read_export(obs_path)
stopifnot(nrow(man) == 39626L, !anyDuplicated(man$request_id),
          !anyDuplicated(obs$request_id),
          setequal(man$request_id[man$status == 'ok'], obs$request_id),
          all(core %in% names(obs)))
stopifnot(!anyDuplicated(key(man$station_identity_id, man$target_datetime)),
          !anyDuplicated(key(obs$station_identity_id, obs$target_datetime)))

selection <- man$plan_component == 'semiannual_full_day_extra' &
             man$study_site_code %in% sites &
             substr(man$target_datetime, 1L, 4L) %in% as.character(2021:2025)
selected <- man[selection, , drop = FALSE]
stopifnot(nrow(selected) == 1850L)
selected$date <- substr(selected$target_datetime, 1L, 10L)
selected$year <- as.integer(substr(selected$target_datetime, 1L, 4L))
selected$month <- as.integer(substr(selected$target_datetime, 6L, 7L))
selected$hour <- as.integer(substr(selected$target_datetime, 12L, 13L))
stopifnot(setequal(unique(selected$hour), c(0L,4L,8L,16L,20L)))
expected_five <- table(key(selected$station_identity_id, selected$date))
stopifnot(all(expected_five == 5L))

id_cols <- c('study_site_code','study_site_name','station_identity_id','date','year','month')
base_days <- unique(selected[, id_cols, drop = FALSE])
stopifnot(nrow(base_days) == 370L)
grid <- base_days[rep(seq_len(nrow(base_days)), each = 6L), , drop = FALSE]
row.names(grid) <- NULL
grid$hour <- rep(hours, times = nrow(base_days))
grid$target_datetime <- paste0(grid$date, ' ', sprintf('%02d:00:00',grid$hour))
grid$season <- ifelse(grid$month == 1L, 'January', 'July')

ix_m <- match(key(grid$station_identity_id, grid$target_datetime),
              key(man$station_identity_id, man$target_datetime))
stopifnot(!anyNA(ix_m))
grid$request_id <- man$request_id[ix_m]
grid$status <- man$status[ix_m]
grid$plan_component <- man$plan_component[ix_m]
stopifnot(all(grid$plan_component[grid$hour == 12L] == 'monthly_noon_backbone'),
          all(grid$plan_component[grid$hour != 12L] == 'semiannual_full_day_extra'),
          all(grid$status %in% c('ok','no_record','pending')))

ix_o <- match(grid$request_id, obs$request_id)
stopifnot(identical(!is.na(ix_o), grid$status == 'ok'))
for (nm in c('monitor_time','section_status','cnemc_station_name',
             'qingyue_province','qingyue_city','river','basin','longitude','latitude')) {
  grid[[nm]] <- obs[[nm]][ix_o]
}
for (nm in core) {
  grid[[nm]] <- suppressWarnings(as.numeric(obs[[nm]][ix_o]))
}
core_matrix <- as.matrix(grid[,core,drop=FALSE])
grid$seven_core_present <- rowSums(!is.finite(core_matrix)) == 0L
grid$seven_core_nonnegative <- rowSums(core_matrix < 0, na.rm=TRUE) == 0L
grid$seven_core_valid <- grid$seven_core_present & grid$seven_core_nonnegative
grid$normal <- !is.na(grid$section_status) & grid$section_status == normal_status
grid$timestamp_matches <- !is.na(grid$monitor_time) & grid$monitor_time == grid$target_datetime
grid$strict <- grid$status == 'ok' & grid$normal & grid$timestamp_matches & grid$seven_core_valid
grid$do_only_strict <- grid$status == 'ok' & grid$normal & grid$timestamp_matches &
                       is.finite(grid$dissolvedoxygen) & grid$dissolvedoxygen >= 0
grid$quality_reason <- ifelse(grid$status == 'pending', 'PENDING',
  ifelse(grid$status == 'no_record', 'NO_RECORD',
  ifelse(!grid$normal, 'NON_NORMAL_STATUS',
  ifelse(!grid$timestamp_matches, 'TIMESTAMP_MISMATCH',
  ifelse(!grid$seven_core_valid, 'INCOMPLETE_OR_NEGATIVE_CORE', 'STRICT_PASS')))))
write_output(grid, 'R_fujian_expected_hourly_QC.csv')

make_day <- function(g) {
  g <- g[order(g$hour), , drop = FALSE]
  stopifnot(nrow(g) == 6L, identical(as.integer(g$hour), hours))
  noon <- g[g$hour == 12L, , drop = FALSE]
  extra <- g[g$hour != 12L, , drop = FALSE]
  valid <- all(g$strict)
  do_valid <- all(g$do_only_strict)
  names_at_station <- g$cnemc_station_name[!is.na(g$cnemc_station_name)]
  station_name <- if (length(names_at_station)) names_at_station[1L] else g$station_identity_id[1L]
  first_non_na <- function(z) {
    x <- z[!is.na(z)]
    if (length(x)) x[1L] else NA
  }
  d <- data.frame(
    site = g$study_site_code[1L], station_identity_id = g$station_identity_id[1L],
    station = station_name, qingyue_province = first_non_na(g$qingyue_province),
    qingyue_city = first_non_na(g$qingyue_city), river = first_non_na(g$river),
    longitude = first_non_na(g$longitude), latitude = first_non_na(g$latitude),
    date = g$date[1L], year = as.integer(g$year[1L]), month = as.integer(g$month[1L]),
    season = g$season[1L], noon_status = noon$status[1L], noon_strict = noon$strict[1L],
    extras_ok = sum(extra$status == 'ok'), extras_no_record = sum(extra$status == 'no_record'),
    extras_pending = sum(extra$status == 'pending'), extras_strict = sum(extra$strict),
    all_extra_ok = all(extra$status == 'ok'), strict_sixpoint = valid,
    do_only_sixpoint = do_valid,
    invalid_hours = paste(sprintf('%02d:%s', g$hour[!g$strict],g$quality_reason[!g$strict]),collapse=';'),
    do_noon = NA_real_, do_sixmean = NA_real_, do_min = NA_real_, do_max = NA_real_,
    do_range = NA_real_, signed_noon_sixmean = NA_real_, abs_noon_sixmean = NA_real_,
    noon_minus_sampled_min = NA_real_, temp_range = NA_real_, ph_range = NA_real_,
    do_min_hour = NA_integer_, do_max_hour = NA_integer_,
    do_only_range = NA_real_, do_only_signed_noon_sixmean = NA_real_,
    flag_do_range_gt5 = FALSE, flag_do_range_gt10 = FALSE,
    flag_max_do_gt20 = FALSE, flag_ph_range_gt2 = FALSE,
    stringsAsFactors = FALSE
  )
  if (valid) {
    do <- g$dissolvedoxygen
    d$do_noon <- noon$dissolvedoxygen[1L]
    d$do_sixmean <- mean(do)
    d$do_min <- min(do)
    d$do_max <- max(do)
    d$do_range <- diff(range(do))
    d$signed_noon_sixmean <- d$do_noon - d$do_sixmean
    d$abs_noon_sixmean <- abs(d$signed_noon_sixmean)
    d$noon_minus_sampled_min <- d$do_noon - d$do_min
    d$temp_range <- diff(range(g$water_temp))
    d$ph_range <- diff(range(g$ph))
    d$do_min_hour <- as.integer(g$hour[which.min(do)])
    d$do_max_hour <- as.integer(g$hour[which.max(do)])
    d$flag_do_range_gt5 <- d$do_range > 5
    d$flag_do_range_gt10 <- d$do_range > 10
    d$flag_max_do_gt20 <- d$do_max > 20
    d$flag_ph_range_gt2 <- d$ph_range > 2
  }
  if (do_valid) {
    d$do_only_range <- diff(range(g$dissolvedoxygen))
    d$do_only_signed_noon_sixmean <- noon$dissolvedoxygen[1L] - mean(g$dissolvedoxygen)
  }
  d
}
keys <- key(grid$station_identity_id, grid$date)
days <- do.call(rbind, lapply(split(grid, keys), make_day))
row.names(days) <- NULL
days <- days[order(days$site,days$station_identity_id,days$year,days$month), , drop=FALSE]
stopifnot(nrow(days) == 370L)
write_output(days, 'R_fujian_station_days.csv')

valid <- days[days$strict_sixpoint, , drop=FALSE]
jan <- valid[valid$month == 1L, , drop=FALSE]
jul <- valid[valid$month == 7L, , drop=FALSE]
pairs <- merge(jan, jul, by=c('site','station_identity_id','year'), suffixes=c('_jan','_jul'))
pairs$delta_do_range <- pairs$do_range_jul - pairs$do_range_jan
pairs$delta_abs_noon_sixmean <- pairs$abs_noon_sixmean_jul - pairs$abs_noon_sixmean_jan
pairs$delta_temp_range <- pairs$temp_range_jul - pairs$temp_range_jan
write_output(pairs, 'R_fujian_valid_january_july_pairs.csv')

summarise_valid <- function(g) {
  data.frame(n_days=nrow(g), n_stations=length(unique(g$station_identity_id)),
    median_do_range=median(g$do_range),
    median_abs_noon_mean=median(g$abs_noon_sixmean),
    median_signed_noon_mean=median(g$signed_noon_sixmean),
    median_noon_minus_min=median(g$noon_minus_sampled_min),
    share_noon_not_min=mean(g$noon_minus_sampled_min > 1e-8))
}
summary_season <- do.call(rbind,lapply(list(All=valid,January=jan,July=jul),function(g) summarise_valid(g)))
summary_season$season <- row.names(summary_season)
summary_season <- summary_season[,c('season',setdiff(names(summary_season),'season'))]
write_output(summary_season, 'R_fujian_summary_by_season.csv')
by_site_season <- do.call(rbind,lapply(split(valid, paste(valid$site,valid$season,sep='|')),function(g) {
  cbind(site=g$site[1L],season=g$season[1L],summarise_valid(g))
}))
write_output(by_site_season,'R_fujian_summary_by_site_season.csv')

coverage <- do.call(rbind,lapply(split(days,days$site),function(g) {
  data.frame(site=g$site[1L],planned_station_days=nrow(g),
    station_identities=length(unique(g$station_identity_id)),
    noon_ok=sum(g$noon_status=='ok'),noon_strict=sum(g$noon_strict),
    all_extra_ok=sum(g$all_extra_ok),valid_sixpoint_days=sum(g$strict_sixpoint),
    extra_ok=sum(g$extras_ok),extra_no_record=sum(g$extras_no_record),
    extra_pending=sum(g$extras_pending),extra_strict=sum(g$extras_strict))
}))
write_output(coverage,'R_fujian_coverage_by_site.csv')
remaining <- days[days$extras_pending>0,,drop=FALSE]
pending_counts <- do.call(rbind,lapply(split(remaining,paste(remaining$site,remaining$noon_status,remaining$noon_strict,sep='|')),function(g) {
  data.frame(site=g$site[1L],noon_status=g$noon_status[1L],noon_strict=g$noon_strict[1L],
    n_station_days=nrow(g),pending_extra_requests=sum(g$extras_pending))
}))
write_output(pending_counts,'R_fujian_remaining_requests_by_noon_dependency.csv')

by_site_year_season <- do.call(rbind,lapply(split(days,paste(days$site,days$year,days$season,sep='|')),function(g) {
  data.frame(site=g$site[1L],year=g$year[1L],season=g$season[1L],
    n_planned_days=nrow(g),noon_ok=sum(g$noon_status=='ok'),
    noon_strict=sum(g$noon_strict),all_extra_ok=sum(g$all_extra_ok),
    n_strict_profiles=sum(g$strict_sixpoint),total_extra_ok=sum(g$extras_ok),
    total_extra_strict=sum(g$extras_strict))
}))
write_output(by_site_year_season,'R_fujian_profile_coverage_by_site_year_season.csv')

qc_counts <- as.data.frame(table(grid$study_site_code,grid$season,grid$hour,grid$quality_reason))
names(qc_counts) <- c('site','season','hour','quality_reason','n')
qc_counts <- qc_counts[qc_counts$n>0L,,drop=FALSE]
write_output(qc_counts,'R_fujian_quality_reasons.csv')
flags <- valid[valid$flag_do_range_gt5 | valid$flag_max_do_gt20 | valid$flag_ph_range_gt2,,drop=FALSE]
write_output(flags,'R_fujian_extreme_profiles_for_review.csv')
min_time <- as.data.frame(table(valid$season,valid$do_min_hour))
names(min_time) <- c('season','do_min_hour','n_profiles')
write_output(min_time[min_time$n_profiles>0,,drop=FALSE],'R_fujian_DO_minimum_by_sample_hour.csv')

# Station-level uncertainty rather than treating every station-year as independent.
station_pair_test <- function(d, value, n_perm=20000L, n_boot=5000L) {
  x <- tapply(d[[value]],d$station_identity_id,mean)
  n <- length(x)
  if (!n) return(data.frame(n_pairs=0L,n_stations=0L))
  obs_diff <- mean(x)
  signed <- replicate(n_perm,mean(x*sample(c(-1,1),n,replace=TRUE)))
  boot <- replicate(n_boot,mean(sample(x,n,replace=TRUE)))
  data.frame(n_pairs=nrow(d),n_stations=n,
    mean_station_contrast=obs_diff,
    ci_low=unname(quantile(boot,.025)),ci_high=unname(quantile(boot,.975)),
    station_signflip_p=(1+sum(abs(signed)>=abs(obs_diff)))/(n_perm+1),
    median_paired_contrast=median(d[[value]]),
    n_positive_pairs=sum(d[[value]]>0))
}
paired_tests <- do.call(rbind,lapply(c('delta_do_range','delta_abs_noon_sixmean','delta_temp_range'), function(v) {
  cbind(outcome=v,station_pair_test(pairs,v))
}))
write_output(paired_tests,'R_fujian_paired_station_cluster_tests.csv')
bias_scenarios <- list('All strict'=valid,
  '2021-2024 only'=valid[valid$year<=2024,,drop=FALSE],
  'Exclude DO range >5'=valid[valid$do_range<=5,,drop=FALSE],
  'Exclude large DO and pH swings'=valid[valid$do_range<=5 & valid$ph_range<=2,,drop=FALSE])
bias_results <- do.call(rbind,lapply(names(bias_scenarios),function(label) {
  cbind(sample=label,station_pair_test(bias_scenarios[[label]],'signed_noon_sixmean'))
}))
write_output(bias_results,'R_fujian_signed_bias_sensitivity.csv')
trim <- pairs[pairs$do_range_jan<=5 & pairs$do_range_jul<=5,,drop=FALSE]
trim_tests <- do.call(rbind,lapply(c('delta_do_range','delta_abs_noon_sixmean'),function(v) {
  cbind(outcome=v,station_pair_test(trim,v))
}))
write_output(trim_tests,'R_fujian_paired_trimmed_station_cluster_tests.csv')

# Sensitivity test: valid dissolved oxygen only, even if a nutrient variable is unavailable.
do_valid <- days[days$do_only_sixpoint,,drop=FALSE]
dj <- do_valid[do_valid$month == 1L,,drop=FALSE]
du <- do_valid[do_valid$month == 7L,,drop=FALSE]
dopairs <- merge(dj,du,by=c('site','station_identity_id','year'),suffixes=c('_jan','_jul'))
dopairs$delta_do_only_range <- dopairs$do_only_range_jul-dopairs$do_only_range_jan
write_output(dopairs,'R_fujian_DO_only_pairs.csv')
write_output(station_pair_test(dopairs,'delta_do_only_range'),'R_fujian_DO_only_station_test.csv')

roster_cols <- c('study_site_code','station_identity_id','cnemc_station_name',
  'qingyue_province','qingyue_city','river','basin','longitude','latitude')
roster <- obs[obs$study_site_code %in% sites,roster_cols,drop=FALSE]
roster <- roster[!duplicated(roster$station_identity_id),,drop=FALSE]
roster$watershed_connection_verified <- FALSE
roster$geography_review_note <- 'Study-area label provisional; delineate upstream watershed and outlet'
roster$geography_review_note[roster$study_site_code=='MJE' & roster$river=='\u6556\u6c5f'] <-
  'Aojiang is separately named in source river field; verify assigned outlet'
roster$geography_review_note[roster$study_site_code=='QZB' & roster$river=='\u6728\u5170\u6eaa'] <-
  'Mulanxi is separately named in source river field; verify assigned outlet'
write_output(roster,'R_fujian_station_geography_review.csv')

# For provenance, report the original 1290 allowlisted calls separately.
if (length(args) >= 4L && file.exists(args[[4]])) {
  ids <- trimws(readLines(args[[4]],encoding='UTF-8',warn=FALSE))
  ids <- ids[nzchar(ids) & !startsWith(ids,'#')]
  stopifnot(length(ids)==1290L,!anyDuplicated(ids))
  priority <- man[man$request_id %in% ids,,drop=FALSE]
  stopifnot(nrow(priority)==1290L,all(priority$status %in% c('ok','no_record')))
  priority$in_Fujian_four_sites <- priority$study_site_code %in% sites
  priority_summary <- do.call(rbind,lapply(split(priority,priority$study_site_code),function(g) {
    data.frame(site=g$study_site_code[1L],site_name=g$study_site_name[1L],
      requested=nrow(g),ok=sum(g$status=='ok'),no_record=sum(g$status=='no_record'),
      in_Fujian_four_sites=all(g$in_Fujian_four_sites))
  }))
  write_output(priority_summary,'R_original_priority_1290_status.csv')
}

# Optional publication-ready plots; numeric results are available without ggplot2.
if (requireNamespace('ggplot2',quietly=TRUE)) {
  gg <- asNamespace('ggplot2')
  coverage_plot <- do.call(rbind,lapply(split(days,paste(days$site,days$season,sep='|')),function(g) {
    data.frame(site=g$site[1L],season=g$season[1L],valid_days=sum(g$strict_sixpoint))
  }))
  plot1 <- ggplot2::ggplot(coverage_plot,ggplot2::aes(x=site,y=valid_days,fill=season)) +
    ggplot2::geom_col(position=ggplot2::position_dodge(width=.75),width=.7) +
    ggplot2::geom_text(ggplot2::aes(label=valid_days),position=ggplot2::position_dodge(width=.75),vjust=-.35,size=3) +
    ggplot2::labs(x='Study-area group',y='Strict six-point station-days',fill='Date',
      title='Fujian-labelled monitoring network: strict profile coverage') +
    ggplot2::theme_minimal(base_size=12) + ggplot2::theme(legend.position='top')
  ggplot2::ggsave(file.path(out_dir,'R_figure_01_profile_coverage.png'),plot1,width=8.3,height=5,dpi=220)

  plot2 <- ggplot2::ggplot(pairs,ggplot2::aes(x=do_range_jan,y=do_range_jul,color=site)) +
    ggplot2::geom_abline(slope=1,intercept=0,linetype='dashed',color='grey50') +
    ggplot2::geom_point(alpha=.7,size=2) + ggplot2::coord_equal() +
    ggplot2::labs(x='January six-point DO range',y='July six-point DO range',color='Study area',
      title='Within-station-year January-July DO comparison') +
    ggplot2::theme_minimal(base_size=12)
  ggplot2::ggsave(file.path(out_dir,'R_figure_02_paired_DO_range.png'),plot2,width=7.4,height=6,dpi=220)

  plot3 <- ggplot2::ggplot(valid,ggplot2::aes(x=do_sixmean,y=do_noon,color=site)) +
    ggplot2::geom_abline(slope=1,intercept=0,linetype='dashed',color='grey50') +
    ggplot2::geom_point(alpha=.65,size=2) +
    ggplot2::labs(x='Six-point arithmetic mean DO',y='Noon dissolved oxygen',color='Study area',
      title='Noon versus six-point dissolved oxygen') +
    ggplot2::theme_minimal(base_size=12)
  ggplot2::ggsave(file.path(out_dir,'R_figure_03_noon_vs_sixpoint.png'),plot3,width=7.4,height=6,dpi=220)

  heat <- aggregate(strict ~ year + season + hour, data=grid, FUN=mean)
  heat$period <- factor(paste0(heat$year,'-',substr(heat$season,1,3)),
    levels=rev(as.vector(rbind(paste0(2021:2025,'-Jan'),paste0(2021:2025,'-Jul')))))
  heat$hour_label <- factor(sprintf('%02d:00',heat$hour),levels=sprintf('%02d:00',hours))
  heat$label <- paste0(round(100*heat$strict),'%')
  plot4 <- ggplot2::ggplot(heat,ggplot2::aes(x=hour_label,y=period,fill=strict)) +
    ggplot2::geom_tile(color='white',linewidth=.7) +
    ggplot2::geom_text(ggplot2::aes(label=label),size=3.5) +
    ggplot2::scale_fill_viridis_c(limits=c(0,1)) +
    ggplot2::labs(x='Sampled hour',y='Year and selected month',fill='Strictly valid',
      title='Hourly strict-measurement validity, 2021-2025') +
    ggplot2::theme_minimal(base_size=12)
  ggplot2::ggsave(file.path(out_dir,'R_figure_04_hourly_quality_heatmap.png'),plot4,width=8,height=5.8,dpi=220)
} else {
  message('ggplot2 not installed: numerical CSVs created; install.packages("ggplot2") to generate figures.')
}

message(sprintf('PASS: %d original plan rows; %d Fujian-labelled stations; %d expected station-days; %d strict six-point profiles; %d valid Jan-Jul pairs (%d unique stations).',
  nrow(man),length(unique(days$station_identity_id)),nrow(days),nrow(valid),nrow(pairs),
  length(unique(pairs$station_identity_id))))
message(sprintf('Jan 2025 strict profiles: %d of %d; all output is read-only.',
  sum(days$year == 2025L & days$month == 1L & days$strict_sixpoint),
  sum(days$year == 2025L & days$month == 1L)))
