# ============================================================================
# cnemc_analysis_helpers.R
#
# Derived cleaning helpers for reconciled CNEMC surface-water data.
#
# IMPORTANT:
#   * These functions DO NOT modify raw archives, row_hash, or
#     observation_key_hash.
#   * Apply them only after reconciliation / version selection.
#   * *_raw columns remain untouched for provenance.
# ============================================================================

# Reverse the common "UTF-8 bytes decoded as Windows-1252/Latin-1" mojibake
# found in CNEMC text fields. The input is already an R Unicode string, so this
# reconstructs the one-byte Windows-1252 representation and then interprets
# those bytes as UTF-8. Already-correct Chinese is returned unchanged.
repair_cnemc_mojibake <- function(x) {
  special_cp1252 <- c(
    `8364` = 128L,  # euro
    `8218` = 130L,
    `402`  = 131L,
    `8222` = 132L,
    `8230` = 133L,
    `8224` = 134L,
    `8225` = 135L,
    `710`  = 136L,
    `8240` = 137L,
    `352`  = 138L,
    `8249` = 139L,
    `338`  = 140L,
    `381`  = 142L,
    `8216` = 145L,
    `8217` = 146L,
    `8220` = 147L,
    `8221` = 148L,
    `8226` = 149L,
    `8211` = 150L,
    `8212` = 151L,
    `732`  = 152L,
    `8482` = 153L,
    `353`  = 154L,
    `8250` = 155L,
    `339`  = 156L,
    `382`  = 158L,
    `376`  = 159L
  )

  repair_one <- function(s) {
    if (is.na(s) || !nzchar(s)) return(s)

    cp <- utf8ToInt(enc2utf8(s))
    if (length(cp) == 0L) return(s)

    bytes <- integer(length(cp))
    for (i in seq_along(cp)) {
      code <- cp[[i]]

      if (code >= 0L && code <= 255L) {
        # Includes Latin-1 bytes and the undefined CP1252 control positions
        # (0x81, 0x8D, 0x8F, 0x90, 0x9D) if they survived as controls.
        bytes[[i]] <- code
      } else {
        key <- as.character(code)
        if (!key %in% names(special_cp1252)) {
          # A genuine non-Western Unicode string (e.g. already-correct Chinese)
          # should not be transformed.
          return(s)
        }
        bytes[[i]] <- unname(special_cp1252[[key]])
      }
    }

    candidate <- rawToChar(as.raw(bytes))
    Encoding(candidate) <- "UTF-8"

    # Accept only valid UTF-8 reconstruction. If the string was legitimate
    # Latin text rather than mojibake, this usually fails and the original is
    # retained.
    valid <- suppressWarnings(iconv(candidate, from = "UTF-8", to = "UTF-8"))
    if (is.na(valid)) return(s)

    valid
  }

  vapply(as.character(x), repair_one, character(1), USE.NAMES = FALSE)
}

cnemc_html_to_text <- function(x) {
  x <- repair_cnemc_mojibake(as.character(x))
  x <- gsub("(?i)<br\\s*/?>", " ", x, perl = TRUE)
  x <- gsub("<[^>]+>", "", x, perl = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&mu;", "µ", x, fixed = TRUE)
  x <- gsub("&sup3;", "³", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

# Extract the highest-precision published value from a CNEMC parameter cell.
# Current cells commonly look like:
#   <span title='原始值：24.74'>24.7</span>
# The title is preferred because the displayed value may be rounded.
cnemc_parameter_text <- function(x) {
  x <- repair_cnemc_mojibake(as.character(x))
  out <- cnemc_html_to_text(x)

  has_title <- !is.na(x) & grepl("title\\s*=\\s*['\\\"]", x, perl = TRUE)
  if (any(has_title)) {
    title_text <- sub(
      ".*title\\s*=\\s*['\\\"]([^'\\\"]*)['\\\"].*",
      "\\1",
      x[has_title],
      perl = TRUE
    )
    title_text <- cnemc_html_to_text(title_text)
    title_text <- sub(
      "^.*?(?:原始值|raw\\s*value)\\s*[:：]\\s*",
      "",
      title_text,
      perl = TRUE,
      ignore.case = TRUE
    )
    out[has_title] <- trimws(title_text)
  }

  out[out %in% c("", "--", "—", "-", "NA", "N/A", "null", "NULL")] <- NA_character_
  out
}

cnemc_parameter_qualifier <- function(x) {
  z <- cnemc_parameter_text(x)
  out <- rep(NA_character_, length(z))
  out[!is.na(z) & grepl("^\\s*<=", z)] <- "<="
  out[!is.na(z) & grepl("^\\s*>=", z)] <- ">="
  out[!is.na(z) & grepl("^\\s*<", z) & is.na(out)] <- "<"
  out[!is.na(z) & grepl("^\\s*>", z) & is.na(out)] <- ">"
  out[!is.na(z) & grepl("^\\s*≤", z)] <- "<="
  out[!is.na(z) & grepl("^\\s*≥", z)] <- ">="
  out
}

cnemc_parameter_numeric <- function(x) {
  z <- cnemc_parameter_text(x)
  if (requireNamespace("readr", quietly = TRUE)) {
    return(suppressWarnings(readr::parse_number(z, na = c("", "NA", "N/A"))))
  }

  # Base-R fallback when readr is unavailable.
  z <- sub("^\\s*(?:<=|>=|<|>|≤|≥)\\s*", "", z, perl = TRUE)
  suppressWarnings(as.numeric(z))
}

cnemc_decode_water_class <- function(code) {
  key <- c(
    `1` = "Ⅰ",
    `2` = "Ⅱ",
    `3` = "Ⅲ",
    `4` = "Ⅳ",
    `5` = "Ⅴ",
    `6` = "劣Ⅴ"
  )
  z <- trimws(as.character(code))
  out <- unname(key[z])
  out[!z %in% names(key)] <- NA_character_
  out
}

# Add analysis-ready fields while preserving every original column.
clean_cnemc_analysis_fields <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)

  text_map <- c(
    area = "area_cn",
    river_basin = "river_basin_cn",
    monitoring_section = "monitoring_section_cn"
  )

  for (src in names(text_map)) {
    if (src %in% names(df)) {
      df[[text_map[[src]]]] <- repair_cnemc_mojibake(df[[src]])
    }
  }

  if ("water_quality_class" %in% names(df)) {
    df$water_quality_class_published <- repair_cnemc_mojibake(df$water_quality_class)
  }

  if ("water_quality_class_code" %in% names(df)) {
    rank <- suppressWarnings(as.integer(as.character(df$water_quality_class_code)))
    rank[!rank %in% 1:6] <- NA_integer_
    df$water_quality_class_rank <- rank
    df$water_quality_class_clean <- cnemc_decode_water_class(df$water_quality_class_code)

    # If a readable published class is available but the numeric code is not,
    # retain the published class rather than replacing it with NA.
    if ("water_quality_class_published" %in% names(df)) {
      fill <- is.na(df$water_quality_class_clean) &
        !is.na(df$water_quality_class_published) &
        nzchar(df$water_quality_class_published)
      df$water_quality_class_clean[fill] <- df$water_quality_class_published[fill]
    }
  } else if ("water_quality_class_published" %in% names(df)) {
    df$water_quality_class_clean <- df$water_quality_class_published
  }

  parameter_map <- c(
    water_temperature_c_raw = "water_temperature_c",
    ph_raw = "ph",
    dissolved_oxygen_mg_l_raw = "dissolved_oxygen_mg_l",
    conductivity_raw = "conductivity_us_cm",
    turbidity_ntu_raw = "turbidity_ntu",
    permanganate_index_mg_l_raw = "permanganate_index_mg_l",
    ammonia_nitrogen_mg_l_raw = "ammonia_nitrogen_mg_l",
    total_phosphorus_mg_l_raw = "total_phosphorus_mg_l",
    total_nitrogen_mg_l_raw = "total_nitrogen_mg_l",
    toc_mg_l_raw = "toc_mg_l",
    chlorophyll_a_raw = "chlorophyll_a",
    algal_density_raw = "algal_density_cells_l"
  )

  for (src in names(parameter_map)) {
    if (!src %in% names(df)) next
    dest <- parameter_map[[src]]
    df[[dest]] <- cnemc_parameter_numeric(df[[src]])
    df[[paste0(dest, "_qualifier")]] <- cnemc_parameter_qualifier(df[[src]])
  }

  # Stable human-readable station key for joins to external coordinate sources.
  if (all(c("area_cn", "monitoring_section_cn") %in% names(df))) {
    normalize_key <- function(x) {
      z <- trimws(as.character(x))
      z <- gsub(intToUtf8(12288L), "", z, fixed = TRUE)
      z <- gsub("[[:space:]]+", "", z, perl = TRUE)
      tolower(z)
    }
    df$analysis_station_key <- paste(
      normalize_key(df$area_cn),
      normalize_key(df$monitoring_section_cn),
      sep = "|"
    )
  }

  df
}

# Attach an audited external station-coordinate crosswalk to an already-cleaned
# CNEMC analysis data frame. The join uses analysis_station_key, which is based
# on cleaned province + monitoring-section labels. Raw source fields and hashes
# are left untouched.
attach_cnemc_station_coordinates <- function(
  df,
  crosswalk_path,
  overwrite_existing = FALSE
) {
  if (is.null(df) || nrow(df) == 0L) return(df)

  if (!"analysis_station_key" %in% names(df)) {
    df <- clean_cnemc_analysis_fields(df)
  }

  if (is.null(crosswalk_path) || length(crosswalk_path) == 0L ||
      is.na(crosswalk_path) || !file.exists(crosswalk_path)) {
    return(df)
  }

  cw <- if (requireNamespace("readr", quietly = TRUE)) {
    suppressMessages(readr::read_csv(crosswalk_path, show_col_types = FALSE))
  } else {
    utils::read.csv(crosswalk_path, stringsAsFactors = FALSE, check.names = FALSE)
  }

  required <- c("station_key", "longitude", "latitude")
  if (!all(required %in% names(cw))) {
    stop(
      "CNEMC coordinate crosswalk is missing required column(s): ",
      paste(setdiff(required, names(cw)), collapse = ", "),
      call. = FALSE
    )
  }

  cw$longitude <- suppressWarnings(as.numeric(cw$longitude))
  cw$latitude <- suppressWarnings(as.numeric(cw$latitude))
  cw <- cw[
    !is.na(cw$station_key) & nzchar(as.character(cw$station_key)) &
      is.finite(cw$longitude) & is.finite(cw$latitude),
    , drop = FALSE
  ]

  # The ingestion script excludes conflicting station keys from the published
  # crosswalk. This guard prevents a future malformed crosswalk from silently
  # creating one-to-many joins.
  if (anyDuplicated(cw$station_key)) {
    stop(
      "CNEMC coordinate crosswalk contains duplicated station_key values. ",
      "Resolve conflicts before attaching coordinates.",
      call. = FALSE
    )
  }

  get_or_na <- function(nm) {
    if (nm %in% names(cw)) as.character(cw[[nm]]) else rep(NA_character_, nrow(cw))
  }

  lookup <- data.frame(
    analysis_station_key = as.character(cw$station_key),
    cnemc_crosswalk_longitude = cw$longitude,
    cnemc_crosswalk_latitude = cw$latitude,
    cnemc_crosswalk_city = get_or_na("city_cn"),
    cnemc_crosswalk_coordinate_source = get_or_na("coordinate_source"),
    cnemc_crosswalk_source_url = get_or_na("source_url"),
    stringsAsFactors = FALSE
  )

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required to attach the CNEMC coordinate crosswalk.", call. = FALSE)
  }

  out <- dplyr::left_join(df, lookup, by = "analysis_station_key")

  existing_lon <- if ("station_longitude" %in% names(out)) {
    suppressWarnings(as.numeric(out$station_longitude))
  } else {
    rep(NA_real_, nrow(out))
  }
  existing_lat <- if ("station_latitude" %in% names(out)) {
    suppressWarnings(as.numeric(out$station_latitude))
  } else {
    rep(NA_real_, nrow(out))
  }

  use_crosswalk <- is.finite(out$cnemc_crosswalk_longitude) &
    is.finite(out$cnemc_crosswalk_latitude) &
    (isTRUE(overwrite_existing) | !is.finite(existing_lon) | !is.finite(existing_lat))

  out$station_longitude <- existing_lon
  out$station_latitude <- existing_lat
  out$station_longitude[use_crosswalk] <- out$cnemc_crosswalk_longitude[use_crosswalk]
  out$station_latitude[use_crosswalk] <- out$cnemc_crosswalk_latitude[use_crosswalk]

  if (!"station_city" %in% names(out)) out$station_city <- NA_character_
  city_fill <- use_crosswalk & !is.na(out$cnemc_crosswalk_city) & nzchar(out$cnemc_crosswalk_city)
  out$station_city[city_fill] <- out$cnemc_crosswalk_city[city_fill]

  if (!"station_coordinate_source" %in% names(out)) out$station_coordinate_source <- NA_character_
  src_fill <- use_crosswalk
  out$station_coordinate_source[src_fill] <- out$cnemc_crosswalk_coordinate_source[src_fill]

  if (!"station_coordinate_source_url" %in% names(out)) out$station_coordinate_source_url <- NA_character_
  out$station_coordinate_source_url[src_fill] <- out$cnemc_crosswalk_source_url[src_fill]

  out$cnemc_crosswalk_longitude <- NULL
  out$cnemc_crosswalk_latitude <- NULL
  out$cnemc_crosswalk_city <- NULL
  out$cnemc_crosswalk_coordinate_source <- NULL
  out$cnemc_crosswalk_source_url <- NULL

  out
}
