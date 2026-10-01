# ============================================================================
# CNEMC station reconciliation helpers
#
# Purpose
#   Use the Qingyue national /staname catalogue as a station-reference layer
#   for the official CNEMC surface-water observation archive.
#
# Principles
#   * CNEMC remains the primary observation source.
#   * Qingyue supplies secondary station metadata (coordinates, city, valley,
#     catalogue time span).
#   * Raw/source labels are preserved; standardized keys are only for matching.
#   * Automatic matching is conservative. Fuzzy similarity is never used to
#     force a match.
#   * The crosswalk is unique by normalized CNEMC province + station name so
#     joining it cannot multiply CNEMC observation rows.
# ============================================================================

required_cnemc_reference_packages <- c(
  "dplyr", "readr", "tibble", "stringr", "digest"
)

missing_cnemc_reference_packages <- required_cnemc_reference_packages[
  !vapply(required_cnemc_reference_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_cnemc_reference_packages) > 0L) {
  stop(
    "Missing required package(s) for CNEMC station reconciliation: ",
    paste(missing_cnemc_reference_packages, collapse = ", "),
    call. = FALSE
  )
}

# -----------------------------------------------------------------------------
# Normalization helpers
# -----------------------------------------------------------------------------

cnemc_clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- enc2utf8(x)
  x <- gsub("[\u00A0\u3000]", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

cnemc_station_key <- function(x, loose = FALSE) {
  x <- cnemc_clean_text(x)
  x <- gsub("\\s+", "", x, perl = TRUE)

  if (isTRUE(loose)) {
    # Remove punctuation/spacing variants only. Do NOT remove semantic words
    # such as 桥, 断面, 水库, 闸, etc.
    x <- gsub("[()（）\\[\\]【】{}<>《》·•・,，.。:：;；'\"“”‘’/\\\\_—–-]", "", x, perl = TRUE)
  }

  x
}

cnemc_province_key <- function(x) {
  x <- cnemc_clean_text(x)
  x <- gsub("\\s+", "", x, perl = TRUE)

  # Match administrative units by their core name. This deliberately handles
  # Qingyue anomalies such as 黑龙江 versus 黑龙江省 without overwriting the raw
  # source value.
  x <- sub("特别行政区$", "", x)
  x <- sub("维吾尔自治区$", "", x)
  x <- sub("壮族自治区$", "", x)
  x <- sub("回族自治区$", "", x)
  x <- sub("自治区$", "", x)
  x <- sub("省$", "", x)
  x <- sub("市$", "", x)
  x
}

cnemc_standardize_province <- function(x) {
  raw <- cnemc_clean_text(x)
  key <- cnemc_province_key(raw)

  map <- c(
    "北京" = "北京市", "天津" = "天津市", "河北" = "河北省",
    "山西" = "山西省", "内蒙古" = "内蒙古自治区", "辽宁" = "辽宁省",
    "吉林" = "吉林省", "黑龙江" = "黑龙江省", "上海" = "上海市",
    "江苏" = "江苏省", "浙江" = "浙江省", "安徽" = "安徽省",
    "福建" = "福建省", "江西" = "江西省", "山东" = "山东省",
    "河南" = "河南省", "湖北" = "湖北省", "湖南" = "湖南省",
    "广东" = "广东省", "广西" = "广西壮族自治区", "海南" = "海南省",
    "重庆" = "重庆市", "四川" = "四川省", "贵州" = "贵州省",
    "云南" = "云南省", "西藏" = "西藏自治区", "陕西" = "陕西省",
    "甘肃" = "甘肃省", "青海" = "青海省", "宁夏" = "宁夏回族自治区",
    "新疆" = "新疆维吾尔自治区", "香港" = "香港特别行政区",
    "澳门" = "澳门特别行政区"
  )

  out <- unname(map[key])
  keep_raw <- is.na(out) & nzchar(raw)
  out[keep_raw] <- raw[keep_raw]
  out[!nzchar(raw)] <- NA_character_
  out
}

cnemc_parse_time <- function(x) {
  x <- cnemc_clean_text(x)
  x[!nzchar(x)] <- NA_character_
  suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai"))
}

cnemc_name_similarity <- function(a, b) {
  a <- cnemc_clean_text(a)
  b <- cnemc_clean_text(b)
  if (!nzchar(a) || !nzchar(b)) return(NA_real_)
  if (identical(a, b)) return(1)

  d <- utils::adist(a, b)[1, 1]
  denom <- max(nchar(a, type = "chars"), nchar(b, type = "chars"), 1L)
  max(0, 1 - as.numeric(d) / denom)
}

cnemc_collapse_unique <- function(x) {
  x <- cnemc_clean_text(x)
  x <- sort(unique(x[nzchar(x)]))
  if (length(x) == 0L) NA_character_ else paste(x, collapse = " | ")
}

cnemc_safe_min_time <- function(x) {
  x <- as.POSIXct(x, tz = "Asia/Shanghai")
  if (all(is.na(x))) as.POSIXct(NA, tz = "Asia/Shanghai") else min(x, na.rm = TRUE)
}

cnemc_safe_max_time <- function(x) {
  x <- as.POSIXct(x, tz = "Asia/Shanghai")
  if (all(is.na(x))) as.POSIXct(NA, tz = "Asia/Shanghai") else max(x, na.rm = TRUE)
}

# -----------------------------------------------------------------------------
# Input standardization
# -----------------------------------------------------------------------------

prepare_cnemc_station_inventory <- function(cnemc) {
  required <- c("area", "monitoring_section")
  missing <- setdiff(required, names(cnemc))
  if (length(missing) > 0L) {
    stop(
      "CNEMC observations are missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"river_basin" %in% names(cnemc)) cnemc$river_basin <- NA_character_
  if (!"observation_datetime" %in% names(cnemc)) {
    cnemc$observation_datetime <- as.POSIXct(NA, tz = "Asia/Shanghai")
  }

  cnemc |>
    dplyr::mutate(
      cnemc_area = cnemc_clean_text(.data$area),
      cnemc_station_name = cnemc_clean_text(.data$monitoring_section),
      cnemc_river_basin_raw = cnemc_clean_text(.data$river_basin),
      cnemc_province_key = cnemc_province_key(.data$area),
      cnemc_station_key = cnemc_station_key(.data$monitoring_section, loose = FALSE),
      cnemc_station_key_loose = cnemc_station_key(.data$monitoring_section, loose = TRUE)
    ) |>
    dplyr::filter(nzchar(.data$cnemc_station_name)) |>
    dplyr::group_by(
      .data$cnemc_province_key,
      .data$cnemc_station_key,
      .data$cnemc_station_key_loose
    ) |>
    dplyr::summarise(
      cnemc_area = cnemc_collapse_unique(.data$cnemc_area),
      cnemc_station_name = cnemc_collapse_unique(.data$cnemc_station_name),
      cnemc_river_basin = cnemc_collapse_unique(.data$cnemc_river_basin_raw),
      cnemc_first_observed = cnemc_safe_min_time(.data$observation_datetime),
      cnemc_last_observed = cnemc_safe_max_time(.data$observation_datetime),
      cnemc_observation_rows = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$cnemc_province_key, .data$cnemc_station_name)
}

prepare_qingyue_catalog <- function(qingyue, reference_retrieved_at = NA_character_) {
  # Accept either the Python-normalized CSV or a source-like table.
  if (!"station_name" %in% names(qingyue) && "staname" %in% names(qingyue)) {
    qingyue$station_name <- qingyue$staname
  }

  required <- c("province", "city", "valley", "station_name", "longitude", "latitude", "first_time", "last_time")
  missing <- setdiff(required, names(qingyue))
  if (length(missing) > 0L) {
    stop(
      "Qingyue catalogue is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out <- qingyue |>
    dplyr::transmute(
      reference_station_name = cnemc_clean_text(.data$station_name),
      reference_province_raw = cnemc_clean_text(.data$province),
      reference_province = cnemc_standardize_province(.data$province),
      reference_city = cnemc_clean_text(.data$city),
      reference_valley = cnemc_clean_text(.data$valley),
      longitude = suppressWarnings(as.numeric(.data$longitude)),
      latitude = suppressWarnings(as.numeric(.data$latitude)),
      reference_first_time = cnemc_clean_text(.data$first_time),
      reference_last_time = cnemc_clean_text(.data$last_time),
      reference_first_datetime = cnemc_parse_time(.data$first_time),
      reference_last_datetime = cnemc_parse_time(.data$last_time),
      reference_request_id = if ("source_request_id" %in% names(qingyue)) {
        cnemc_clean_text(.data$source_request_id)
      } else {
        NA_character_
      }
    ) |>
    dplyr::mutate(
      reference_province_key = cnemc_province_key(.data$reference_province),
      reference_station_key = cnemc_station_key(.data$reference_station_name, loose = FALSE),
      reference_station_key_loose = cnemc_station_key(.data$reference_station_name, loose = TRUE),
      coordinate_complete = !is.na(.data$longitude) & !is.na(.data$latitude),
      reference_retrieved_at = as.character(reference_retrieved_at)
    )

  # Deterministic record id: one ID per published Qingyue catalogue row.
  out$qingyue_record_id <- vapply(
    seq_len(nrow(out)),
    function(i) {
      vals <- unlist(out[i, c(
        "reference_province_raw", "reference_city", "reference_valley",
        "reference_station_name", "longitude", "latitude",
        "reference_first_time", "reference_last_time"
      ), drop = FALSE], use.names = FALSE)
      paste0(
        "QYREC_",
        substr(
          digest::digest(paste(vals, collapse = "\u001F"), algo = "xxhash64", serialize = FALSE),
          1L,
          12L
        )
      )
    },
    character(1)
  )

  # Coordinate-sharing groups are useful alias candidates, but are not assumed
  # to represent the same physical station automatically.
  coord_key <- ifelse(
    out$coordinate_complete,
    paste(format(out$longitude, digits = 15, trim = TRUE), format(out$latitude, digits = 15, trim = TRUE), sep = "|"),
    NA_character_
  )

  coord_counts <- table(coord_key[!is.na(coord_key)])
  repeated_coords <- names(coord_counts[coord_counts > 1L])

  out$station_alias_group <- NA_character_
  hit <- !is.na(coord_key) & coord_key %in% repeated_coords
  out$station_alias_group[hit] <- vapply(
    coord_key[hit],
    function(z) paste0(
      "QYCOORD_",
      substr(digest::digest(z, algo = "xxhash64", serialize = FALSE), 1L, 12L)
    ),
    character(1)
  )

  tibble::as_tibble(out)
}

# -----------------------------------------------------------------------------
# Candidate selection
# -----------------------------------------------------------------------------

cnemc_time_overlap_seconds <- function(cnemc_start, cnemc_end, ref_start, ref_end) {
  if (any(is.na(c(cnemc_start, cnemc_end, ref_start, ref_end)))) return(NA_real_)
  lo <- max(cnemc_start, ref_start)
  hi <- min(cnemc_end, ref_end)
  max(0, as.numeric(difftime(hi, lo, units = "secs")))
}

cnemc_choose_candidate <- function(candidates, cnemc_first, cnemc_last) {
  if (nrow(candidates) == 0L) {
    return(list(status = "none", row = NULL, note = "No candidate"))
  }
  if (nrow(candidates) == 1L) {
    return(list(status = "unique", row = candidates[1, , drop = FALSE], note = NA_character_))
  }

  overlaps <- vapply(
    seq_len(nrow(candidates)),
    function(i) cnemc_time_overlap_seconds(
      cnemc_first,
      cnemc_last,
      candidates$reference_first_datetime[[i]],
      candidates$reference_last_datetime[[i]]
    ),
    numeric(1)
  )

  positive <- which(!is.na(overlaps) & overlaps > 0)
  if (length(positive) == 1L) {
    return(list(
      status = "unique_temporal",
      row = candidates[positive, , drop = FALSE],
      note = "Multiple name candidates existed; a single catalogue interval overlapped the CNEMC observation window."
    ))
  }

  list(
    status = "ambiguous",
    row = NULL,
    note = sprintf("%d reference candidates remain after conservative matching.", nrow(candidates))
  )
}

match_one_cnemc_station <- function(cnemc_row, qingyue) {
  province_key <- cnemc_row$cnemc_province_key[[1]]
  exact_key <- cnemc_row$cnemc_station_key[[1]]
  loose_key <- cnemc_row$cnemc_station_key_loose[[1]]
  first_obs <- cnemc_row$cnemc_first_observed[[1]]
  last_obs <- cnemc_row$cnemc_last_observed[[1]]

  try_candidates <- function(cand, method, allow_temporal = TRUE) {
    if (nrow(cand) == 0L) return(NULL)

    # For national fallback we only accept a genuinely unique name. Province-
    # constrained matches may use a unique temporal overlap to disambiguate
    # historical catalogue rows carrying the same published station name.
    chosen <- if (isTRUE(allow_temporal)) {
      cnemc_choose_candidate(cand, first_obs, last_obs)
    } else if (nrow(cand) == 1L) {
      list(status = "unique", row = cand[1, , drop = FALSE], note = NA_character_)
    } else {
      list(
        status = "ambiguous",
        row = NULL,
        note = sprintf("%d national candidates share this station key.", nrow(cand))
      )
    }

    if (chosen$status %in% c("unique", "unique_temporal")) {
      return(list(
        status = "matched",
        method = method,
        ref = chosen$row,
        note = chosen$note,
        candidate_n = nrow(cand)
      ))
    }

    list(
      status = "ambiguous",
      method = "ambiguous",
      ref = NULL,
      note = paste0(method, ": ", chosen$note),
      candidate_n = nrow(cand)
    )
  }

  # 1. Exact name inside the CNEMC province. If candidates exist here but are
  # ambiguous, stop rather than broadening nationally and risking a wrong join.
  cand <- qingyue |>
    dplyr::filter(
      .data$reference_province_key == province_key,
      .data$reference_station_key == exact_key
    )
  res <- try_candidates(cand, "exact_name_province", allow_temporal = TRUE)
  if (!is.null(res)) return(res)

  # 2. Exact name nationally, but only when globally unique.
  cand <- qingyue |>
    dplyr::filter(.data$reference_station_key == exact_key)
  res <- try_candidates(cand, "exact_name_national_unique", allow_temporal = FALSE)
  if (!is.null(res)) return(res)

  # 3. Punctuation/spacing-normalized name inside province.
  cand <- qingyue |>
    dplyr::filter(
      .data$reference_province_key == province_key,
      .data$reference_station_key_loose == loose_key
    )
  res <- try_candidates(cand, "normalized_name_province", allow_temporal = TRUE)
  if (!is.null(res)) return(res)

  # 4. Normalized name nationally, but only when globally unique.
  cand <- qingyue |>
    dplyr::filter(.data$reference_station_key_loose == loose_key)
  res <- try_candidates(cand, "normalized_name_national_unique", allow_temporal = FALSE)
  if (!is.null(res)) return(res)

  list(
    status = "unmatched",
    method = "unresolved",
    ref = NULL,
    note = "No exact or punctuation-normalized Qingyue station-name match was found.",
    candidate_n = 0L
  )
}

# -----------------------------------------------------------------------------
# Optional manual overrides
# -----------------------------------------------------------------------------

create_cnemc_override_template <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path) || file.exists(path)) return(invisible(FALSE))

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  template <- tibble::tibble(
    cnemc_area = character(),
    cnemc_station_name = character(),
    action = character(),              # match | unmatched
    reference_station_name = character(),
    reference_province = character(),
    reference_first_time = character(),
    review_note = character()
  )

  readr::write_csv(template, path, na = "")
  invisible(TRUE)
}

apply_cnemc_overrides <- function(crosswalk, qingyue, overrides_path = NULL) {
  if (is.null(overrides_path) || is.na(overrides_path) || !nzchar(overrides_path) || !file.exists(overrides_path)) {
    return(crosswalk)
  }

  overrides <- readr::read_csv(
    overrides_path,
    show_col_types = FALSE,
    progress = FALSE,
    na = c("", "NA")
  )

  if (nrow(overrides) == 0L) return(crosswalk)

  required <- c(
    "cnemc_area", "cnemc_station_name", "action",
    "reference_station_name", "reference_province",
    "reference_first_time", "review_note"
  )
  missing <- setdiff(required, names(overrides))
  if (length(missing) > 0L) {
    stop(
      "Override file is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  overrides <- overrides |>
    dplyr::mutate(
      override_province_key = cnemc_province_key(.data$cnemc_area),
      override_station_key = cnemc_station_key(.data$cnemc_station_name),
      action = tolower(cnemc_clean_text(.data$action)),
      reference_station_name = cnemc_clean_text(.data$reference_station_name),
      reference_province = cnemc_clean_text(.data$reference_province),
      reference_first_time = cnemc_clean_text(.data$reference_first_time),
      review_note = cnemc_clean_text(.data$review_note)
    )

  for (i in seq_len(nrow(overrides))) {
    ov <- overrides[i, , drop = FALSE]
    idx <- which(
      crosswalk$cnemc_province_key == ov$override_province_key[[1]] &
        crosswalk$cnemc_station_key == ov$override_station_key[[1]]
    )

    if (length(idx) != 1L) {
      stop(
        "Override key did not resolve to exactly one CNEMC station: ",
        ov$cnemc_area[[1]], " / ", ov$cnemc_station_name[[1]],
        call. = FALSE
      )
    }

    action <- ov$action[[1]]

    if (identical(action, "unmatched")) {
      ref_cols <- c(
        "reference_station_name", "reference_province_raw", "reference_province",
        "reference_city", "reference_valley", "longitude", "latitude",
        "reference_first_time", "reference_last_time", "qingyue_record_id",
        "station_alias_group", "reference_request_id", "reference_retrieved_at"
      )
      for (nm in intersect(ref_cols, names(crosswalk))) crosswalk[[nm]][idx] <- NA
      crosswalk$match_method[idx] <- "manual_unmatched"
      crosswalk$match_status[idx] <- "unmatched"
      crosswalk$match_score[idx] <- NA_real_
      crosswalk$review_required[idx] <- FALSE
      crosswalk$review_note[idx] <- ov$review_note[[1]]
      next
    }

    if (!identical(action, "match")) {
      stop("Unknown override action: ", action, ". Use match or unmatched.", call. = FALSE)
    }

    q <- qingyue |>
      dplyr::filter(
        .data$reference_station_key == cnemc_station_key(ov$reference_station_name[[1]]),
        .data$reference_province_key == cnemc_province_key(ov$reference_province[[1]])
      )

    if (nzchar(ov$reference_first_time[[1]])) {
      q <- q |>
        dplyr::filter(.data$reference_first_time == ov$reference_first_time[[1]])
    }

    if (nrow(q) != 1L) {
      stop(
        "Manual match override must identify exactly one Qingyue record for ",
        ov$cnemc_area[[1]], " / ", ov$cnemc_station_name[[1]],
        "; found ", nrow(q), ".",
        call. = FALSE
      )
    }

    q <- q[1, , drop = FALSE]
    assign_cols <- intersect(
      c(
        "reference_station_name", "reference_province_raw", "reference_province",
        "reference_city", "reference_valley", "longitude", "latitude",
        "reference_first_time", "reference_last_time", "qingyue_record_id",
        "station_alias_group", "reference_request_id", "reference_retrieved_at"
      ),
      names(crosswalk)
    )

    for (nm in assign_cols) crosswalk[[nm]][idx] <- q[[nm]][[1]]

    crosswalk$match_method[idx] <- "manual_override"
    crosswalk$match_status[idx] <- "matched"
    crosswalk$match_score[idx] <- cnemc_name_similarity(
      crosswalk$cnemc_station_name[[idx]],
      q$reference_station_name[[1]]
    )
    crosswalk$review_required[idx] <- FALSE
    crosswalk$review_note[idx] <- ov$review_note[[1]]
  }

  crosswalk
}

# -----------------------------------------------------------------------------
# Main crosswalk builder
# -----------------------------------------------------------------------------

build_cnemc_station_crosswalk <- function(
  cnemc_observations_path,
  qingyue_catalog_path,
  output_path,
  overrides_path = NULL,
  qa_dir = dirname(output_path),
  quiet = FALSE
) {
  for (p in c(cnemc_observations_path, qingyue_catalog_path)) {
    if (!file.exists(p)) stop("Missing required input: ", p, call. = FALSE)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
  create_cnemc_override_template(overrides_path)

  cnemc <- if (grepl("\\.rds$", cnemc_observations_path, ignore.case = TRUE)) {
    readRDS(cnemc_observations_path)
  } else {
    readr::read_csv(cnemc_observations_path, show_col_types = FALSE, progress = FALSE)
  }

  qingyue_raw <- readr::read_csv(
    qingyue_catalog_path,
    show_col_types = FALSE,
    progress = FALSE,
    na = c("", "NA")
  )

  reference_retrieved_at <- format(
    file.info(qingyue_catalog_path)$mtime,
    "%Y-%m-%d %H:%M:%S %z"
  )

  inventory <- prepare_cnemc_station_inventory(cnemc)
  qingyue <- prepare_qingyue_catalog(qingyue_raw, reference_retrieved_at)

  rows <- vector("list", nrow(inventory))

  for (i in seq_len(nrow(inventory))) {
    one <- inventory[i, , drop = FALSE]
    m <- match_one_cnemc_station(one, qingyue)

    base <- one |>
      dplyr::mutate(
        match_method = m$method,
        match_status = m$status,
        match_candidate_n = as.integer(m$candidate_n),
        review_required = !identical(m$status, "matched"),
        review_note = m$note
      )

    if (identical(m$status, "matched")) {
      ref <- m$ref[1, , drop = FALSE]
      rows[[i]] <- dplyr::bind_cols(
        base,
        ref |>
          dplyr::select(
            .data$reference_station_name,
            .data$reference_province_raw,
            .data$reference_province,
            .data$reference_city,
            .data$reference_valley,
            .data$longitude,
            .data$latitude,
            .data$reference_first_time,
            .data$reference_last_time,
            .data$qingyue_record_id,
            .data$station_alias_group,
            .data$reference_request_id,
            .data$reference_retrieved_at
          )
      ) |>
        dplyr::mutate(
          match_score = cnemc_name_similarity(.data$cnemc_station_name, .data$reference_station_name)
        )
    } else {
      rows[[i]] <- base |>
        dplyr::mutate(
          reference_station_name = NA_character_,
          reference_province_raw = NA_character_,
          reference_province = NA_character_,
          reference_city = NA_character_,
          reference_valley = NA_character_,
          longitude = NA_real_,
          latitude = NA_real_,
          reference_first_time = NA_character_,
          reference_last_time = NA_character_,
          qingyue_record_id = NA_character_,
          station_alias_group = NA_character_,
          reference_request_id = NA_character_,
          reference_retrieved_at = reference_retrieved_at,
          match_score = NA_real_
        )
    }
  }

  crosswalk <- dplyr::bind_rows(rows) |>
    dplyr::mutate(
      station_identity_id = .data$qingyue_record_id,
      coordinate_source = dplyr::if_else(
        .data$match_status == "matched" & !is.na(.data$longitude) & !is.na(.data$latitude),
        "Qingyue /api/surface_water/staname",
        NA_character_
      ),
      reference_source = "Qingyue/Huawei Cloud Marketplace",
      coordinate_complete = !is.na(.data$longitude) & !is.na(.data$latitude)
    )

  crosswalk <- apply_cnemc_overrides(crosswalk, qingyue, overrides_path)

  # Recompute fields that an override may have changed.
  crosswalk <- crosswalk |>
    dplyr::mutate(
      station_identity_id = .data$qingyue_record_id,
      coordinate_source = dplyr::if_else(
        .data$match_status == "matched" & !is.na(.data$longitude) & !is.na(.data$latitude),
        "Qingyue /api/surface_water/staname",
        NA_character_
      ),
      reference_source = "Qingyue/Huawei Cloud Marketplace",
      coordinate_complete = !is.na(.data$longitude) & !is.na(.data$latitude)
    )

  # Unique join key is mandatory. This prevents row multiplication later.
  duplicated_join_key <- crosswalk |>
    dplyr::count(.data$cnemc_province_key, .data$cnemc_station_key, name = "n") |>
    dplyr::filter(.data$n > 1L)

  if (nrow(duplicated_join_key) > 0L) {
    stop("CNEMC station crosswalk is not unique by normalized province + station key.", call. = FALSE)
  }

  keep_order <- c(
    "cnemc_station_name", "cnemc_area", "cnemc_river_basin",
    "reference_station_name", "reference_province", "reference_province_raw",
    "reference_city", "reference_valley", "longitude", "latitude",
    "reference_first_time", "reference_last_time", "match_method",
    "match_status", "match_score", "match_candidate_n", "station_identity_id",
    "station_alias_group", "coordinate_source", "reference_source",
    "reference_retrieved_at", "coordinate_complete", "review_required",
    "review_note", "cnemc_first_observed", "cnemc_last_observed",
    "cnemc_observation_rows", "cnemc_province_key", "cnemc_station_key",
    "cnemc_station_key_loose", "reference_request_id"
  )

  crosswalk <- crosswalk |>
    dplyr::select(dplyr::any_of(keep_order)) |>
    dplyr::arrange(.data$cnemc_area, .data$cnemc_station_name)

  readr::write_csv(crosswalk, output_path, na = "")

  unmatched_path <- file.path(qa_dir, "cnemc_station_crosswalk_unmatched.csv")
  ambiguous_path <- file.path(qa_dir, "cnemc_station_crosswalk_ambiguous.csv")
  alias_path <- file.path(qa_dir, "cnemc_qingyue_alias_candidates.csv")
  qa_path <- file.path(qa_dir, "cnemc_station_crosswalk_qa.csv")

  readr::write_csv(
    crosswalk |> dplyr::filter(.data$match_status == "unmatched"),
    unmatched_path,
    na = ""
  )
  readr::write_csv(
    crosswalk |> dplyr::filter(.data$match_status == "ambiguous"),
    ambiguous_path,
    na = ""
  )

  alias_candidates <- qingyue |>
    dplyr::filter(!is.na(.data$station_alias_group)) |>
    dplyr::arrange(.data$station_alias_group, .data$reference_first_datetime) |>
    dplyr::select(
      .data$station_alias_group,
      .data$reference_station_name,
      .data$reference_province,
      .data$reference_city,
      .data$reference_valley,
      .data$longitude,
      .data$latitude,
      .data$reference_first_time,
      .data$reference_last_time,
      .data$qingyue_record_id
    )
  readr::write_csv(alias_candidates, alias_path, na = "")

  qa <- tibble::tibble(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    cnemc_station_keys = nrow(crosswalk),
    matched = sum(crosswalk$match_status == "matched", na.rm = TRUE),
    matched_with_coordinates = sum(crosswalk$match_status == "matched" & crosswalk$coordinate_complete, na.rm = TRUE),
    matched_missing_coordinates = sum(crosswalk$match_status == "matched" & !crosswalk$coordinate_complete, na.rm = TRUE),
    ambiguous = sum(crosswalk$match_status == "ambiguous", na.rm = TRUE),
    unmatched = sum(crosswalk$match_status == "unmatched", na.rm = TRUE),
    review_required = sum(crosswalk$review_required, na.rm = TRUE),
    qingyue_catalog_rows = nrow(qingyue),
    qingyue_complete_coordinates = sum(qingyue$coordinate_complete, na.rm = TRUE),
    qingyue_alias_candidate_rows = nrow(alias_candidates)
  )
  readr::write_csv(qa, qa_path, na = "")

  if (!quiet) {
    message("CNEMC station crosswalk written: ", output_path)
    message("  CNEMC station keys: ", nrow(crosswalk))
    message("  matched: ", qa$matched)
    message("  matched with coordinates: ", qa$matched_with_coordinates)
    message("  ambiguous: ", qa$ambiguous)
    message("  unmatched: ", qa$unmatched)
    message("  QA: ", qa_path)
  }

  invisible(crosswalk)
}

ensure_cnemc_station_crosswalk <- function(
  cnemc_observations_path,
  qingyue_catalog_path,
  output_path,
  overrides_path = NULL,
  qa_dir = dirname(output_path),
  force = FALSE,
  quiet = FALSE
) {
  inputs <- c(cnemc_observations_path, qingyue_catalog_path)
  if (!is.null(overrides_path) && file.exists(overrides_path)) {
    inputs <- c(inputs, overrides_path)
  }

  rebuild <- isTRUE(force) || !file.exists(output_path)

  if (!rebuild) {
    out_mtime <- file.info(output_path)$mtime
    in_mtime <- file.info(inputs)$mtime
    rebuild <- any(is.na(in_mtime)) || any(in_mtime > out_mtime)
  }

  if (rebuild) {
    if (!quiet) message("Building/refreshing CNEMC station crosswalk...")
    build_cnemc_station_crosswalk(
      cnemc_observations_path = cnemc_observations_path,
      qingyue_catalog_path = qingyue_catalog_path,
      output_path = output_path,
      overrides_path = overrides_path,
      qa_dir = qa_dir,
      quiet = quiet
    )
  } else if (!quiet) {
    message("CNEMC station crosswalk is current: ", output_path)
  }

  invisible(output_path)
}

# -----------------------------------------------------------------------------
# Attach station metadata to a CNEMC dataset without changing row count
# -----------------------------------------------------------------------------

attach_cnemc_station_reference <- function(data, crosswalk_path) {
  if (!file.exists(crosswalk_path)) {
    stop("Missing CNEMC station crosswalk: ", crosswalk_path, call. = FALSE)
  }

  required <- c("area", "monitoring_section")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "Cannot attach CNEMC station reference; dataset is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  crosswalk <- readr::read_csv(
    crosswalk_path,
    show_col_types = FALSE,
    progress = FALSE,
    na = c("", "NA")
  )

  key_check <- crosswalk |>
    dplyr::count(.data$cnemc_province_key, .data$cnemc_station_key, name = "n") |>
    dplyr::filter(.data$n > 1L)

  if (nrow(key_check) > 0L) {
    stop("Station crosswalk contains duplicate join keys; refusing to join.", call. = FALSE)
  }

  n_before <- nrow(data)

  dat <- data |>
    dplyr::mutate(
      .cnemc_province_key = cnemc_province_key(.data$area),
      .cnemc_station_key = cnemc_station_key(.data$monitoring_section)
    )

  reference_cols <- crosswalk |>
    dplyr::select(
      dplyr::all_of(c("cnemc_province_key", "cnemc_station_key")),
      dplyr::any_of(c(
        "reference_station_name", "reference_province", "reference_province_raw",
        "reference_city", "reference_valley", "longitude", "latitude",
        "reference_first_time", "reference_last_time", "match_method",
        "match_status", "match_score", "station_identity_id", "station_alias_group",
        "coordinate_source", "reference_source", "reference_retrieved_at",
        "coordinate_complete", "review_required", "review_note"
      ))
    ) |>
    dplyr::rename(
      .cnemc_province_key = cnemc_province_key,
      .cnemc_station_key = cnemc_station_key
    )

  # Refuse to silently overwrite any existing final metadata columns.
  protected <- setdiff(names(reference_cols), c(".cnemc_province_key", ".cnemc_station_key"))
  collision <- intersect(protected, names(dat))
  if (length(collision) > 0L) {
    stop(
      "CNEMC dataset already contains station-reference column(s): ",
      paste(collision, collapse = ", "),
      ". Resolve explicitly rather than silently overwriting them.",
      call. = FALSE
    )
  }

  out <- dat |>
    dplyr::left_join(
      reference_cols,
      by = c(".cnemc_province_key", ".cnemc_station_key")
    ) |>
    dplyr::select(-dplyr::all_of(c(".cnemc_province_key", ".cnemc_station_key")))

  if (nrow(out) != n_before) {
    stop(
      "CNEMC station-reference join changed row count from ",
      n_before, " to ", nrow(out), ".",
      call. = FALSE
    )
  }

  out
}
