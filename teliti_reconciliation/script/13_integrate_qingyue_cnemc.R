# 13_integrate_qingyue_cnemc.R
# Non-destructive reconciliation of Qingyue/Huawei observations and CNEMC scraper.
# Produces DERIVED files only. Never sends API requests or updates SQLite ledgers.
# Requires readr, dplyr, tibble; runs standalone or as an add-on to builder 12.

qy_message <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | Huawei-CNEMC | ", ...)

qy_check_packages <- function() {
  pkgs <- c("readr", "dplyr", "tibble")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "), call. = FALSE)
}

qy_norm <- function(x) {
  z <- trimws(as.character(x))
  z[is.na(x) | z == ""] <- NA_character_
  z <- gsub(intToUtf8(12288), "", z, fixed = TRUE)
  z <- gsub("[[:space:]]+", "", z, perl = TRUE)
  tolower(z)
}

qy_key <- function(area, name) {
  a <- qy_norm(area)
  b <- qy_norm(name)
  ifelse(!is.na(a) & !is.na(b), paste(a, b, sep = "|"), NA_character_)
}

qy_or <- function(x, y) {
  x <- as.character(x); y <- as.character(y)
  empty <- is.na(x) | !nzchar(trimws(x))
  x[empty] <- y[empty]
  x
}

qy_column <- function(x, col, default = NA_character_) {
  if (col %in% names(x)) return(as.character(x[[col]]))
  rep(default, nrow(x))
}

qy_read_csv <- function(path) {
  if (!file.exists(path)) stop("Required file not found: ", path, call. = FALSE)
  suppressMessages(readr::read_csv(
    path, col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE, progress = FALSE, name_repair = "minimal"
  ))
}

qy_datetime_str <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai"))
  }
  z <- as.character(x)
  z <- gsub("T", " ", z, fixed = TRUE)
  z <- sub("Z$", "", z)
  z <- trimws(z)
  z[is.na(x) | !grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}(:\\d{2})?$", z)] <- NA_character_
  short <- !is.na(z) & nchar(z) == 16L
  z[short] <- paste0(z[short], ":00")
  z
}

qy_posix <- function(x) {
  as.POSIXct(qy_datetime_str(x), format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai")
}

qy_write_rds <- function(x, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "qy_", tmpdir = dirname(dest), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, compress = "gzip", version = 3)
  # Derived-only output; we refuse to modify source and archived files.
  if (file.exists(dest)) unlink(dest)
  if (!file.rename(tmp, dest)) stop("Could not write derived RDS: ", dest, call. = FALSE)
  invisible(dest)
}

qy_write_csv <- function(x, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "qy_", tmpdir = dirname(dest), fileext = if (grepl("\\.gz$", dest)) ".csv.gz" else ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(x, tmp, na = "")
  if (file.exists(dest)) unlink(dest)
  if (!file.rename(tmp, dest)) stop("Could not write derived CSV: ", dest, call. = FALSE)
  invisible(dest)
}

qy_find_archives <- function(hist_root) {
  known <- c(
    historical_main = hist_root,
    probe_noon_52 = file.path(hist_root, "probes", "fixed2_2020older_v1"),
    probe_nonnoon_180 = file.path(hist_root, "probes", "fixed2_nonnoon_2015_2020_v1")
  )
  probe_root <- file.path(hist_root, "probes")
  other <- if (dir.exists(probe_root)) list.dirs(probe_root, recursive = FALSE, full.names = TRUE) else character()
  other <- setdiff(other, unname(known))
  # paste0("prefix", integer(0)) can still produce one name in base R.
  # Avoid assigning a nonzero number of names to zero additional archives.
  additional <- if (length(other) > 0L) {
    stats::setNames(other, sprintf("probe_other_%d", seq_along(other)))
  } else {
    character(0)
  }
  roots <- c(known, additional)
  main_manifest <- file.path(roots[["historical_main"]], "processed", "qingyue_historical_manifest.csv.gz")
  if (!file.exists(main_manifest)) stop("Original Huawei manifest is missing: ", main_manifest, call. = FALSE)
  # The two completed probe ledgers are expected. Do not silently omit them.
  for (id in c("probe_noon_52", "probe_nonnoon_180")) {
    expected <- file.path(roots[[id]], "processed", "qingyue_historical_manifest.csv.gz")
    if (!file.exists(expected)) stop("Completed probe export is missing: ", expected, call. = FALSE)
  }
  has_manifest <- file.exists(file.path(roots, "processed", "qingyue_historical_manifest.csv.gz"))
  roots[has_manifest]
}

qy_find_plan <- function(root, archive_id, manifest_ids) {
  candidates <- if (archive_id == "historical_main") {
    file.path(root, "plan", "qingyue_request_plan_recommended.csv")
  } else if (archive_id == "probe_noon_52") {
    file.path(root, "plan", "qingyue_fixed2_probe_extra52_PLAN.csv")
  } else if (archive_id == "probe_nonnoon_180") {
    file.path(root, "plan", "qingyue_fixed2_nonnoon_2015_2020_PLAN180.csv")
  } else {
    list.files(file.path(root, "plan"), pattern = "\\.csv$", full.names = TRUE)
  }
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) stop("No plan CSV found for ", archive_id, call. = FALSE)
  matches <- list()
  for (path in candidates) {
    first <- suppressMessages(tryCatch(
      readr::read_csv(path, n_max = 0, show_col_types = FALSE), error = function(e) NULL
    ))
    if (is.null(first) || !all(c("request_id", "station_identity_id", "target_datetime") %in% names(first))) next
    plan <- qy_read_csv(path)
    if (nrow(plan) == length(manifest_ids) && setequal(plan$request_id, manifest_ids)) {
      matches[[length(matches) + 1L]] <- list(path = path, plan = plan)
    }
  }
  if (length(matches) != 1L) {
    stop("Expected exactly one plan matching the manifest in ", archive_id,
         "; found ", length(matches), call. = FALSE)
  }
  matches[[1L]]
}

qy_load_archive <- function(root, archive_id) {
  processed <- file.path(root, "processed")
  manifest_path <- file.path(processed, "qingyue_historical_manifest.csv.gz")
  observations_path <- file.path(processed, "qingyue_historical_observations.csv.gz")
  manifest <- qy_read_csv(manifest_path)
  observations <- qy_read_csv(observations_path)
  if (!all(c("request_id", "status", "attempts") %in% names(manifest))) {
    stop("Manifest missing required fields: ", archive_id, call. = FALSE)
  }
  if (anyDuplicated(manifest$request_id)) stop("Duplicate manifest request IDs in ", archive_id, call. = FALSE)
  if (anyDuplicated(observations$request_id)) stop("Duplicate observation request IDs in ", archive_id, call. = FALSE)

  plan_obj <- qy_find_plan(root, archive_id, manifest$request_id)
  plan <- plan_obj$plan
  if (anyDuplicated(plan$request_id)) stop("Duplicate plan request IDs in ", archive_id, call. = FALSE)
  required_plan <- c("station_identity_id", "target_datetime", "cnemc_area", "cnemc_station_name", "plan_component")
  if (!all(required_plan %in% names(plan))) stop("Plan is missing station metadata in ", archive_id, call. = FALSE)

  # One request-ledger row per APPROVED call, including no_record, skipped and pending.
  plan_extra <- setdiff(names(plan), names(manifest))
  ledger <- dplyr::left_join(manifest, plan[, c("request_id", plan_extra), drop = FALSE], by = "request_id")
  ledger$analysis_huawei_archive <- archive_id
  ledger$analysis_huawei_manifest <- normalizePath(manifest_path, winslash = "/", mustWork = TRUE)
  ledger$analysis_huawei_plan <- normalizePath(plan_obj$path, winslash = "/", mustWork = TRUE)
  if (anyDuplicated(ledger$request_id)) stop("Ledger join duplicated request IDs in ", archive_id, call. = FALSE)

  ok_ids <- ledger$request_id[ledger$status == "ok"]
  if (!setequal(ok_ids, observations$request_id)) {
    stop("OK request IDs do not match exported observation IDs in ", archive_id,
         ". Run the local executor export again.", call. = FALSE)
  }
  # Attach fields that may be missing in older observation exports.
  extra <- setdiff(names(plan), names(observations))
  observations <- dplyr::left_join(observations, plan[, c("request_id", extra), drop = FALSE], by = "request_id")
  observations$analysis_huawei_archive <- archive_id
  observations$analysis_huawei_manifest <- normalizePath(manifest_path, winslash = "/", mustWork = TRUE)
  if (anyDuplicated(observations$request_id)) stop("Observation join duplicated IDs in ", archive_id, call. = FALSE)
  if (nrow(observations)) {
    expected <- qy_datetime_str(observations$target_datetime)
    actual <- qy_datetime_str(observations$monitor_time)
    if (any(is.na(expected) | is.na(actual) | expected != actual)) {
      stop("Huawei returned observation times inconsistent with approved plan in ", archive_id, call. = FALSE)
    }
    planned_names <- qy_column(observations, "staname_param")
    returned_names <- qy_column(observations, "section")
    if (any(!is.na(planned_names) & planned_names != returned_names)) {
      stop("Huawei section mismatch in ", archive_id, call. = FALSE)
    }
  }
  qy_message(archive_id, ": plan=", nrow(plan), ", requests=", nrow(ledger),
             ", ok=", nrow(observations), ", attempts=", sum(as.integer(ledger$attempts), na.rm = TRUE))
  list(ledger = ledger, observations = observations,
       provenance = tibble::tibble(archive = archive_id, plan = plan_obj$path,
         manifest = manifest_path, observations = observations_path,
         planned = nrow(plan), ok = nrow(observations),
         api_attempts = sum(as.integer(ledger$attempts), na.rm = TRUE),
         plan_md5 = unname(tools::md5sum(plan_obj$path)),
         manifest_md5 = unname(tools::md5sum(manifest_path)),
         observations_md5 = unname(tools::md5sum(observations_path))))
}

qy_scraper_station_lookup <- function(scraper) {
  required <- c("area_cn", "monitoring_section_cn", "observation_datetime")
  if (!all(required %in% names(scraper))) {
    stop("cnemc_latest_analysis.rds must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  if (!"analysis_station_key" %in% names(scraper)) {
    scraper$analysis_station_key <- qy_key(scraper$area_cn, scraper$monitoring_section_cn)
  }
  station_key <- as.character(scraper$analysis_station_key)
  area_key <- qy_norm(scraper$area_cn)
  name_key <- qy_norm(scraper$monitoring_section_cn)
  timestamp <- qy_datetime_str(scraper$observation_datetime)
  valid <- !is.na(station_key) & nzchar(station_key) & !is.na(name_key) & !is.na(timestamp)
  if (!any(valid)) stop("Scraper has no usable station/timestamp pairs", call. = FALSE)
  lookup <- tibble::tibble(
    analysis_station_key = station_key[valid],
    area_key = area_key[valid], name_key = name_key[valid],
    area_section_key = qy_key(scraper$area_cn[valid], scraper$monitoring_section_cn[valid]),
    observation_timestamp = timestamp[valid]
  ) |>
    dplyr::group_by(.data$analysis_station_key, .data$area_key, .data$name_key, .data$area_section_key) |>
    dplyr::summarise(scraper_first_time = min(.data$observation_timestamp),
                     scraper_last_time = max(.data$observation_timestamp),
                     scraper_observations_n = dplyr::n(), .groups = "drop")
  lookup
}

qy_match_stations <- function(ledger, scraper_lookup, scraper, overrides_path) {
  info <- ledger |>
    dplyr::select(dplyr::any_of(c("station_identity_id", "cnemc_area", "cnemc_station_name",
                                   "qingyue_station_name", "qingyue_province", "study_site_code"))) |>
    dplyr::distinct(.data$station_identity_id, .keep_all = TRUE)
  info$area_key <- qy_norm(qy_column(info, "cnemc_area"))
  info$name_key <- qy_norm(qy_column(info, "cnemc_station_name"))
  info$area_section_key <- qy_key(qy_column(info, "cnemc_area"), qy_column(info, "cnemc_station_name"))
  info$analysis_station_key <- NA_character_
  info$match_rule <- "unmatched"

  # If builder enrichment exposes the same audited Qingyue identity ID, use it.
  identity_fields <- intersect(c(
    "reference_station_identity_id", "reference_qingyue_station_identity_id",
    "qingyue_station_identity_id"
  ), names(scraper))
  if (length(identity_fields)) {
    id_values <- qy_column(scraper, identity_fields[[1]])
    id_key <- tibble::tibble(
      station_identity_id = id_values,
      analysis_station_key = as.character(scraper$analysis_station_key)
    ) |>
      dplyr::filter(!is.na(.data$station_identity_id),
                    nzchar(.data$station_identity_id),
                    !is.na(.data$analysis_station_key)) |>
      dplyr::group_by(.data$station_identity_id) |>
      dplyr::summarise(n_keys = dplyr::n_distinct(.data$analysis_station_key),
                       target_key = dplyr::first(.data$analysis_station_key), .groups = "drop") |>
      dplyr::filter(.data$n_keys == 1L)
    idx <- match(info$station_identity_id, id_key$station_identity_id)
    valid <- !is.na(idx)
    info$analysis_station_key[valid] <- id_key$target_key[idx[valid]]
    info$match_rule[valid] <- "audited_reference_identity"
  }

  # Exact CNEMC source province + section match; never collapse ambiguous keys.
  exact <- scraper_lookup |>
    dplyr::filter(!is.na(.data$area_section_key)) |>
    dplyr::group_by(.data$area_section_key) |>
    dplyr::summarise(n_keys = dplyr::n_distinct(.data$analysis_station_key),
                     target_key = dplyr::first(.data$analysis_station_key), .groups = "drop") |>
    dplyr::filter(.data$n_keys == 1L)
  pos <- match(info$area_section_key, exact$area_section_key)
  valid <- is.na(info$analysis_station_key) & !is.na(pos) & !is.na(info$area_section_key)
  info$analysis_station_key[valid] <- exact$target_key[pos[valid]]
  info$match_rule[valid] <- "exact_area_section"

  # Station name alone is safe only if unique across BOTH source catalogues.
  api_name_n <- info |>
    dplyr::filter(!is.na(.data$name_key)) |>
    dplyr::count(.data$name_key, name = "api_id_count")
  scraper_name_n <- scraper_lookup |>
    dplyr::filter(!is.na(.data$name_key)) |>
    dplyr::group_by(.data$name_key) |>
    dplyr::summarise(scraper_key_count = dplyr::n_distinct(.data$analysis_station_key),
                     target_key = dplyr::first(.data$analysis_station_key), .groups = "drop")
  alt <- dplyr::inner_join(api_name_n, scraper_name_n, by = "name_key") |>
    dplyr::filter(.data$api_id_count == 1L, .data$scraper_key_count == 1L)
  pos <- match(info$name_key, alt$name_key)
  valid <- is.na(info$analysis_station_key) & !is.na(pos)
  info$analysis_station_key[valid] <- alt$target_key[pos[valid]]
  info$match_rule[valid] <- "unique_station_name"

  # Optional user-audited station overrides (never generated automatically).
  if (file.exists(overrides_path)) {
    ov <- qy_read_csv(overrides_path)
    if (!all(c("station_identity_id", "analysis_station_key") %in% names(ov))) {
      stop("Station overrides need station_identity_id and analysis_station_key", call. = FALSE)
    }
    if (anyDuplicated(ov$station_identity_id)) stop("Duplicate station override ID", call. = FALSE)
    unknown <- setdiff(ov$analysis_station_key, scraper_lookup$analysis_station_key)
    if (length(unknown)) stop("Overrides refer to unknown scraper station keys", call. = FALSE)
    ix <- match(info$station_identity_id, ov$station_identity_id)
    valid <- !is.na(ix)
    info$analysis_station_key[valid] <- ov$analysis_station_key[ix[valid]]
    info$match_rule[valid] <- "audited_override"
  }

  scraper_window <- scraper_lookup |>
    dplyr::group_by(.data$analysis_station_key) |>
    dplyr::summarise(scraper_first_time = min(.data$scraper_first_time),
                     scraper_last_time = max(.data$scraper_last_time), .groups = "drop")
  match_idx <- match(info$analysis_station_key, scraper_window$analysis_station_key)
  info$scraper_first_time <- scraper_window$scraper_first_time[match_idx]
  info$scraper_last_time <- scraper_window$scraper_last_time[match_idx]
  info$matched_to_scraper <- !is.na(info$analysis_station_key)
  info
}

qy_numeric <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x), na = c("", "-", "--", "—", "NA", "null", "NULL")))
}

qy_qualifier <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  out[grepl("^(<=|≤)", x) & !is.na(x)] <- "<="
  out[grepl("^(>=|≥)", x) & !is.na(x)] <- ">="
  out[grepl("^<", x) & is.na(out) & !is.na(x)] <- "<"
  out[grepl("^>", x) & is.na(out) & !is.na(x)] <- ">"
  out
}

qy_harmonize_observations <- function(observations, station_match, scraper) {
  lookup <- station_match |>
    dplyr::select(.data$station_identity_id, .data$analysis_station_key, .data$match_rule,
                  .data$scraper_first_time)
  dat <- dplyr::left_join(observations, lookup, by = "station_identity_id")
  if (nrow(dat) != nrow(observations)) stop("Station mapping duplicated API observations", call. = FALSE)
  dat$matched_to_scraper <- !is.na(dat$analysis_station_key)
  dat$analysis_station_key[!dat$matched_to_scraper] <- paste0(
    "huawei_unmatched|", dat$station_identity_id[!dat$matched_to_scraper]
  )
  timestamp <- qy_datetime_str(dat$target_datetime)
  cls <- qy_column(dat, "qulity")
  class_nums <- c("Ⅰ" = "1", "Ⅱ" = "2", "Ⅲ" = "3", "Ⅳ" = "4", "Ⅴ" = "5", "劣Ⅴ" = "6")
  class_code <- unname(class_nums[cls])

  out <- tibble::tibble(
    analysis_source_basis = "qingyue_huawei_api_archived",
    analysis_version_rule = "one_exact_station_time_result_per_request",
    analysis_archive_source = dat$analysis_huawei_archive,
    analysis_archive_file = dat$analysis_huawei_manifest,
    analysis_station_key = dat$analysis_station_key,
    analysis_integrated_key = paste(dat$analysis_station_key, timestamp, sep = "|"),
    analysis_integrated_source_id = paste0("HUAWEI:", dat$analysis_huawei_archive, ":", dat$request_id),
    analysis_huawei_request_id = dat$request_id,
    analysis_huawei_station_identity_id = dat$station_identity_id,
    analysis_huawei_match_rule = dat$match_rule,
    analysis_huawei_response_path = qy_column(dat, "response_path"),
    analysis_huawei_response_sha256 = qy_column(dat, "response_sha256"),
    analysis_huawei_plan_component = qy_column(dat, "plan_component"),
    area = qy_column(dat, "cnemc_area"),
    river_basin = qy_or(qy_column(dat, "cnemc_river_basin"), qy_column(dat, "basin")),
    monitoring_section = qy_column(dat, "cnemc_station_name"),
    observation_datetime = qy_posix(timestamp),
    observation_year_inferred = as.integer(substr(timestamp, 1L, 4L)),
    monitoring_time_raw = NA_character_, # scraper-specific MM-DD published text
    water_quality_class = cls,
    water_quality_class_published = cls,
    water_quality_class_clean = cls,
    water_quality_class_code = class_code,
    water_quality_class_rank = suppressWarnings(as.integer(class_code)),
    water_temperature_c_raw = qy_column(dat, "water_temp"),
    ph_raw = qy_column(dat, "ph"),
    dissolved_oxygen_mg_l_raw = qy_column(dat, "dissolvedoxygen"),
    conductivity_raw = qy_column(dat, "conductivity"),
    turbidity_ntu_raw = qy_column(dat, "turbidity"),
    permanganate_index_mg_l_raw = qy_column(dat, "codmn"),
    ammonia_nitrogen_mg_l_raw = qy_column(dat, "nh3-n"),
    total_phosphorus_mg_l_raw = qy_column(dat, "tp"),
    total_nitrogen_mg_l_raw = qy_column(dat, "tn"),
    chlorophyll_a_raw = qy_column(dat, "chlorophyll"),
    algal_density_raw = qy_column(dat, "algal_density"),
    toc_mg_l_raw = NA_character_
  )
  numeric_map <- c(
    water_temperature_c_raw = "water_temperature_c", ph_raw = "ph",
    dissolved_oxygen_mg_l_raw = "dissolved_oxygen_mg_l",
    conductivity_raw = "conductivity_us_cm", turbidity_ntu_raw = "turbidity_ntu",
    permanganate_index_mg_l_raw = "permanganate_index_mg_l",
    ammonia_nitrogen_mg_l_raw = "ammonia_nitrogen_mg_l",
    total_phosphorus_mg_l_raw = "total_phosphorus_mg_l",
    total_nitrogen_mg_l_raw = "total_nitrogen_mg_l",
    chlorophyll_a_raw = "chlorophyll_a", algal_density_raw = "algal_density_cells_l",
    toc_mg_l_raw = "toc_mg_l"
  )
  for (src in names(numeric_map)) {
    dst <- numeric_map[[src]]
    out[[dst]] <- qy_numeric(out[[src]])
    out[[paste0(dst, "_qualifier")]] <- qy_qualifier(out[[src]])
  }
  out$area_cn <- out$area
  out$river_basin_cn <- out$river_basin
  out$monitoring_section_cn <- out$monitoring_section

  # Attach station-level reference metadata already audited by builder 12.
  # Never overwrite source-native observations; only populate API-derived rows.
  reference_cols <- intersect(c(
    "station_longitude", "station_latitude", "station_city", "station_coordinate_source",
    "station_coordinate_source_url", "coordinate_source", "reference_city",
    "reference_valley", "reference_province", "reference_station_identity_id",
    "reference_qingyue_station_identity_id", "qingyue_station_identity_id"
  ), names(scraper))
  if (length(reference_cols)) {
    scraper_key <- as.character(scraper$analysis_station_key)
    for (nm in reference_cols) {
      vals <- scraper[[nm]]
      usable <- !is.na(scraper_key) & !is.na(vals)
      if (is.character(vals)) usable <- usable & nzchar(trimws(vals))
      if (!any(usable)) next
      k <- scraper_key[usable]
      v <- vals[usable]
      # Preserve the first nonmissing audited reference per canonical station.
      keep <- !duplicated(k)
      out[[nm]] <- v[keep][match(out$analysis_station_key, k[keep])]
    }
  }
  # API's source-native record remains intact in the separately saved full observation table.
  out
}

qy_cast_for_union <- function(api, scraper) {
  # Convert API columns to the scraper's existing types before bind_rows().
  shared <- intersect(names(api), names(scraper))
  for (nm in shared) {
    ref <- scraper[[nm]]
    val <- api[[nm]]
    if (inherits(ref, "POSIXt")) {
      api[[nm]] <- as.POSIXct(val, tz = "Asia/Shanghai")
    } else if (inherits(ref, "Date")) {
      api[[nm]] <- as.Date(val)
    } else if (is.character(ref) || is.factor(ref)) {
      api[[nm]] <- as.character(val)
    } else if (is.integer(ref)) {
      api[[nm]] <- suppressWarnings(as.integer(val))
    } else if (is.numeric(ref)) {
      api[[nm]] <- suppressWarnings(as.numeric(val))
    } else if (is.logical(ref)) {
      api[[nm]] <- suppressWarnings(as.logical(val))
    }
  }
  api
}

qy_compute_call_readiness <- function(ledger) {
  ledger$target_time_key <- qy_datetime_str(ledger$target_datetime)
  noon <- ledger |>
    dplyr::filter(.data$plan_component == "monthly_noon_backbone",
                  substr(.data$target_time_key, 12L, 19L) == "12:00:00") |>
    dplyr::transmute(station_identity_id = .data$station_identity_id,
                     target_date = substr(.data$target_time_key, 1L, 10L),
                     noon_status = .data$status)
  if (anyDuplicated(paste(noon$station_identity_id, noon$target_date))) {
    stop("Multiple noon backbone requests per station-date across Huawei archives", call. = FALSE)
  }
  noon_key <- paste(noon$station_identity_id, noon$target_date, sep = "|")
  row_noon <- paste(ledger$station_identity_id, substr(ledger$target_time_key, 1L, 10L), sep = "|")
  ledger$noon_status <- noon$noon_status[match(row_noon, noon_key)]
  readiness <- rep("requires_error_review", nrow(ledger))
  readiness[ledger$status == "ok"] <- "already_retrieved"
  readiness[ledger$status == "no_record"] <- "already_tested_no_record"
  readiness[ledger$status == "skipped_noon_missing"] <- "zero_call_skip"
  pending <- !is.na(ledger$status) & ledger$status == "pending"
  extra <- !is.na(ledger$plan_component) & ledger$plan_component == "semiannual_full_day_extra"
  readiness[pending & !extra] <- "pending_approved_direct"
  readiness[pending & extra] <- "blocked_verify_noon"
  readiness[pending & extra & !is.na(ledger$noon_status) & ledger$noon_status == "ok"] <- "pending_approved_noon_verified"
  readiness[pending & extra & !is.na(ledger$noon_status) & ledger$noon_status == "no_record"] <- "blocked_noon_no_record"
  readiness[pending & extra & !is.na(ledger$noon_status) & ledger$noon_status == "pending"] <- "blocked_waiting_for_noon"
  ledger$api_call_readiness <- readiness
  ledger
}

run_qingyue_cnemc_integration <- function(project_dir = "D:/# R Project/penelitian") {
  qy_check_packages()
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  analysis_dir <- file.path(project_dir, "teliti_reconciliation", "analysis")
  historical_root <- file.path(project_dir, "nmemc", "data", "surfacewater", "qingyue", "historical")
  source_scraper <- file.path(analysis_dir, "cnemc_latest_analysis.rds")
  overrides_path <- file.path(project_dir, "teliti_reconciliation", "reference", "cnemc_qingyue_station_overrides.csv")
  if (!file.exists(source_scraper)) stop("Run builder 12 first. Missing: ", source_scraper, call. = FALSE)

  roots <- qy_find_archives(historical_root)
  archives <- lapply(names(roots), function(id) qy_load_archive(roots[[id]], id))
  ledger <- dplyr::bind_rows(lapply(archives, `[[`, "ledger"))
  observations <- dplyr::bind_rows(lapply(archives, `[[`, "observations"))
  provenance <- dplyr::bind_rows(lapply(archives, `[[`, "provenance"))
  if (anyDuplicated(ledger$request_id)) stop("Duplicate Huawei request IDs across archives", call. = FALSE)
  if (anyDuplicated(observations$request_id)) stop("Duplicate Huawei observation request IDs", call. = FALSE)
  if (sum(ledger$status == "ok") != nrow(observations)) stop("Huawei OK/observations total mismatch", call. = FALSE)

  scraper <- readRDS(source_scraper)
  if (!is.data.frame(scraper)) stop("Scraper latest analysis is not a data frame", call. = FALSE)
  scraper_lookup <- qy_scraper_station_lookup(scraper)
  if (!"analysis_station_key" %in% names(scraper)) scraper$analysis_station_key <- qy_key(scraper$area_cn, scraper$monitoring_section_cn)
  station_match <- qy_match_stations(ledger, scraper_lookup, scraper, overrides_path)
  ledger <- dplyr::left_join(ledger,
    station_match |>
      dplyr::select(.data$station_identity_id, .data$analysis_station_key,
                    .data$match_rule, .data$scraper_first_time, .data$scraper_last_time),
    by = "station_identity_id")
  ledger <- qy_compute_call_readiness(ledger)
  ledger$pre_scraper <- !is.na(ledger$scraper_first_time) &
    !is.na(ledger$target_time_key) & ledger$target_time_key < ledger$scraper_first_time

  api_harmonized <- qy_harmonize_observations(observations, station_match, scraper)
  if (nrow(api_harmonized) != nrow(observations)) stop("API normalization changed row count", call. = FALSE)
  scraper_time_key <- qy_datetime_str(scraper$observation_datetime)
  scraper$analysis_integrated_key <- paste(scraper$analysis_station_key, scraper_time_key, sep = "|")
  scraper$analysis_integrated_source_id <- paste0("SCRAPER:", as.character(scraper$observation_key_hash))
  if (!"analysis_huawei_request_id" %in% names(scraper)) scraper$analysis_huawei_request_id <- NA_character_
  if (!"analysis_huawei_station_identity_id" %in% names(scraper)) scraper$analysis_huawei_station_identity_id <- NA_character_
  if (!"analysis_huawei_match_rule" %in% names(scraper)) scraper$analysis_huawei_match_rule <- NA_character_
  if (!"analysis_huawei_response_path" %in% names(scraper)) scraper$analysis_huawei_response_path <- NA_character_
  if (!"analysis_huawei_response_sha256" %in% names(scraper)) scraper$analysis_huawei_response_sha256 <- NA_character_
  if (!"analysis_huawei_plan_component" %in% names(scraper)) scraper$analysis_huawei_plan_component <- NA_character_

  api_harmonized$already_in_scraper <- api_harmonized$analysis_integrated_key %in% scraper$analysis_integrated_key
  api_dups <- api_harmonized |>
    dplyr::count(.data$analysis_integrated_key, name = "n") |>
    dplyr::filter(.data$n > 1L)
  api_ordered <- api_harmonized |>
    dplyr::mutate(.source_priority = match(.data$analysis_archive_source,
      c("historical_main", "probe_noon_52", "probe_nonnoon_180"))) |>
    dplyr::arrange(.data$.source_priority, .data$analysis_huawei_request_id) |>
    dplyr::distinct(.data$analysis_integrated_key, .keep_all = TRUE) |>
    dplyr::select(-dplyr::any_of(".source_priority"))
  add_api <- api_ordered |>
    dplyr::filter(!.data$already_in_scraper) |>
    dplyr::select(-dplyr::any_of("already_in_scraper"))
  add_api <- qy_cast_for_union(add_api, scraper)
  # Remove API-specific wide source fields from the large integrated scraper table;
  # those source fields remain in cnemc_huawei_success_full.rds.
  integrated <- dplyr::bind_rows(scraper, add_api)
  if (nrow(integrated) != nrow(scraper) + nrow(add_api)) {
    stop("Integrated row count mismatch", call. = FALSE)
  }

  # Request status inventory and only already-approved, not-yet-called candidates.
  approved_ready <- c("pending_approved_direct", "pending_approved_noon_verified")
  candidate_cols <- c(
    "request_id", "analysis_huawei_archive", "station_identity_id", "cnemc_area",
    "cnemc_station_name", "study_site_code", "plan_component", "target_datetime",
    "status", "attempts", "analysis_station_key", "scraper_first_time",
    "noon_status", "api_call_readiness", "plan_order", "query_string", "endpoint"
  )
  pre_scraper_inventory <- ledger[which(ledger$pre_scraper), , drop = FALSE]
  candidates <- pre_scraper_inventory |>
    dplyr::filter(.data$api_call_readiness %in% approved_ready) |>
    dplyr::arrange(.data$target_datetime, .data$station_identity_id) |>
    dplyr::select(dplyr::any_of(candidate_cols))
  blocked <- pre_scraper_inventory |>
    dplyr::filter(grepl("^blocked_", .data$api_call_readiness)) |>
    dplyr::arrange(.data$target_datetime, .data$station_identity_id) |>
    dplyr::select(dplyr::any_of(candidate_cols))

  success_times <- api_harmonized |>
    dplyr::mutate(timestamp = qy_datetime_str(.data$observation_datetime)) |>
    dplyr::group_by(.data$analysis_station_key) |>
    dplyr::summarise(huawei_success_n = dplyr::n(),
                     huawei_first_success = min(.data$timestamp),
                     huawei_last_success = max(.data$timestamp), .groups = "drop")
  api_pre_scraper_counts <- api_harmonized |>
    dplyr::transmute(
      station_identity_id = .data$analysis_huawei_station_identity_id,
      observation_timestamp = qy_datetime_str(.data$observation_datetime)
    ) |>
    dplyr::left_join(
      station_match |> dplyr::select(.data$station_identity_id, .data$scraper_first_time),
      by = "station_identity_id"
    ) |>
    dplyr::filter(!is.na(.data$scraper_first_time),
                  .data$observation_timestamp < .data$scraper_first_time) |>
    dplyr::count(.data$station_identity_id, name = "pre_scraper_api_success_n")
  windows <- station_match |>
    dplyr::left_join(success_times, by = "analysis_station_key") |>
    dplyr::left_join(api_pre_scraper_counts, by = "station_identity_id")
  windows$pre_scraper_api_success_n[is.na(windows$pre_scraper_api_success_n) &
                                    !is.na(windows$scraper_first_time)] <- 0L
  # The raw API success table and request ledger remain available for provenance.
  qy_write_rds(observations, file.path(analysis_dir, "cnemc_huawei_success_full.rds"))
  qy_write_rds(api_harmonized, file.path(analysis_dir, "cnemc_huawei_success_harmonized.rds"))
  qy_write_rds(ledger, file.path(analysis_dir, "cnemc_huawei_request_ledger.rds"))
  qy_write_rds(integrated, file.path(analysis_dir, "cnemc_integrated_latest_analysis.rds"))
  qy_write_csv(pre_scraper_inventory, file.path(analysis_dir, "cnemc_pre_scraper_request_inventory.csv.gz"))
  qy_write_csv(candidates, file.path(analysis_dir, "cnemc_pre_scraper_pending_approved_calls.csv"))
  qy_write_csv(blocked, file.path(analysis_dir, "cnemc_pre_scraper_blocked_extra_hours.csv"))
  qy_write_csv(station_match, file.path(analysis_dir, "cnemc_huawei_station_match_audit.csv"))
  qy_write_csv(windows, file.path(analysis_dir, "cnemc_huawei_station_time_windows.csv"))
  qy_write_csv(provenance, file.path(analysis_dir, "cnemc_huawei_input_provenance.csv"))
  qy_write_csv(api_harmonized[api_harmonized$already_in_scraper, , drop = FALSE],
               file.path(analysis_dir, "cnemc_huawei_scraper_overlap.csv"))
  qy_write_csv(api_dups, file.path(analysis_dir, "cnemc_huawei_duplicate_station_times.csv"))
  summary <- tibble::tibble(
    input_scraper_rows = nrow(scraper),
    huawei_plan_request_rows = nrow(ledger),
    huawei_http_attempts_reported = sum(as.integer(ledger$attempts), na.rm = TRUE),
    huawei_ok_observation_rows = nrow(observations),
    huawei_no_record_requests = sum(ledger$status == "no_record"),
    huawei_skipped_zero_call = sum(ledger$status == "skipped_noon_missing"),
    huawei_pending_requests = sum(ledger$status == "pending"),
    huawei_overlap_with_scraper = sum(api_harmonized$already_in_scraper),
    huawei_duplicate_station_times = nrow(api_dups),
    huawei_rows_added_to_analysis = nrow(add_api),
    integrated_analysis_rows = nrow(integrated),
    huawei_station_identities = nrow(station_match),
    huawei_station_identities_matched = sum(station_match$matched_to_scraper),
    pre_scraper_approved_direct_ready = nrow(candidates),
    pre_scraper_blocked_requests = nrow(blocked)
  )
  qy_write_csv(summary, file.path(analysis_dir, "cnemc_huawei_integration_summary.csv"))
  qy_message("Complete. Scraper=", nrow(scraper), "; Huawei OK=", nrow(observations),
             "; Huawei added=", nrow(add_api), "; integrated=", nrow(integrated),
             "; pre-scraper approved pending=", nrow(candidates),
             "; blocked=", nrow(blocked))
  qy_message("Outputs: ", analysis_dir)
  invisible(summary)
}

if (sys.nframe() == 0L) run_qingyue_cnemc_integration()
