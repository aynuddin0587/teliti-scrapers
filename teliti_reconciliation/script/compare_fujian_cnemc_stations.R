# Compare Fujian weekly-water and CNEMC automatic-water station crosswalks.
# Run this script locally in R / Positron. It NEVER edits either input file.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
})

# ---- 1. Local input files ---------------------------------------------------
fujian_path <- "D:/# R Project/penelitian/fujian_surfacewater/data/fujian_station_crosswalk.csv"
cnemc_path  <- "D:/# R Project/penelitian/nmemc/data/surfacewater/processed/cnemc_station_crosswalk.csv"
output_dir  <- "D:/# R Project/penelitian/Comparative/output/station_crosswalk_comparison"

# Flag potential co-located stations within this distance (not proof of identity).
max_distance_m <- 500

stopifnot(file.exists(fujian_path), file.exists(cnemc_path))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Read all fields as text to preserve station codes and leading zeroes.
fujian <- read_csv(fujian_path, col_types = cols(.default = col_character()),
                   show_col_types = FALSE)
cnemc  <- read_csv(cnemc_path, col_types = cols(.default = col_character()),
                   show_col_types = FALSE)

cat("Fujian: ", nrow(fujian), " rows\n", sep = "")
cat("CNEMC:  ", nrow(cnemc), " rows\n", sep = "")

# ---- 2. Find station-name fields -------------------------------------------
find_name_columns <- function(dat) {
  preferred <- c(
    "station_name_zh", "station_name_cn", "station_name_chinese",
    "station_name_raw", "station_name_original", "station_name",
    "monitoring_section", "monitoring_section_raw", "monitoring_section_zh",
    "section_name_zh", "section_name", "site_name", "site_name_zh",
    "cnemc_station_name", "fujian_station_name", "station_names_observed",
    "station_aliases", "name_zh", "name_cn", "station", "site", "name"
  )
  inferred <- grep(
    "station.*name|name.*station|section.*name|name.*section|monitoring_section|site.*name|断面|站点",
    names(dat), value = TRUE, ignore.case = TRUE
  )
  found <- unique(c(intersect(preferred, names(dat)), inferred))
  found <- found[!grepl(
    "pinyin|english|translated|address|confidence|method|source|url|code|_id$|^id$|count|quality",
    found, ignore.case = TRUE
  )]
  if (length(found) == 0L) {
    stop("No station-name field identified. Available columns: ",
         paste(names(dat), collapse = ", "))
  }
  found
}

fujian_fields <- find_name_columns(fujian)
cnemc_fields  <- find_name_columns(cnemc)
cat("Fujian name columns: ", paste(fujian_fields, collapse = ", "), "\n", sep = "")
cat("CNEMC name columns:  ", paste(cnemc_fields, collapse = ", "), "\n", sep = "")

# Remove spacing and punctuation, but do not remove meaningful Chinese words.
normalize_name <- function(x) {
  x <- trimws(enc2utf8(as.character(x)))
  x[is.na(x)] <- ""
  x <- gsub("[[:space:]　（）()【】\\[\\]·•，,。.:：、/_-]+", "", x, perl = TRUE)
  x <- tolower(x)
  x[x == ""] <- NA_character_
  x
}

make_names <- function(dat, fields) {
  temp <- tibble(.row_id = seq_len(nrow(dat)))
  for (nm in fields) temp[[nm]] <- as.character(dat[[nm]])
  temp |>
    pivot_longer(cols = all_of(fields), names_to = "name_field", values_to = "matched_name") |>
    separate_rows(matched_name, sep = "\\s*[|;；]\\s*") |>
    mutate(name_key = normalize_name(matched_name)) |>
    filter(!is.na(name_key)) |>
    distinct(.row_id, name_key, .keep_all = TRUE)
}

# ---- 3. Summarize names and optional coordinates ---------------------------
first_nonempty <- function(dat, columns) {
  ans <- rep(NA_character_, nrow(dat))
  for (nm in columns) {
    vals <- trimws(as.character(dat[[nm]]))
    vals[is.na(vals) | !nzchar(vals)] <- NA_character_
    fill <- is.na(ans) & !is.na(vals)
    ans[fill] <- vals[fill]
  }
  ans
}

find_coord_columns <- function(dat, axis) {
  if (axis == "longitude") {
    preferred <- c("longitude", "longitude_wgs84", "lon", "lng", "long",
                   "station_longitude", "longitude_verified", "longitude_resolved")
    pattern <- "longitude|^lon$|^lng$|^long$"
  } else {
    preferred <- c("latitude", "latitude_wgs84", "lat",
                   "station_latitude", "latitude_verified", "latitude_resolved")
    pattern <- "latitude|^lat$"
  }
  unique(c(intersect(preferred, names(dat)),
           grep(pattern, names(dat), ignore.case = TRUE, value = TRUE)))
}

get_coordinate <- function(dat, axis) {
  candidates <- find_coord_columns(dat, axis)
  ans <- rep(NA_real_, nrow(dat))
  for (nm in candidates) {
    vals <- suppressWarnings(parse_number(as.character(dat[[nm]])))
    fill <- is.na(ans) & is.finite(vals)
    ans[fill] <- vals[fill]
  }
  invalid <- !is.na(ans) & (abs(ans) > if (axis == "latitude") 90 else 180)
  ans[invalid] <- NA_real_
  ans
}

fujian_meta <- tibble(
  fujian_row = seq_len(nrow(fujian)),
  fujian_station = first_nonempty(fujian, fujian_fields),
  fujian_lon = get_coordinate(fujian, "longitude"),
  fujian_lat = get_coordinate(fujian, "latitude")
)
cnemc_meta <- tibble(
  cnemc_row = seq_len(nrow(cnemc)),
  cnemc_station = first_nonempty(cnemc, cnemc_fields),
  cnemc_lon = get_coordinate(cnemc, "longitude"),
  cnemc_lat = get_coordinate(cnemc, "latitude")
)

fujian_names <- make_names(fujian, fujian_fields) |>
  rename(fujian_row = .row_id, fujian_name_field = name_field,
         fujian_matched_name = matched_name)
cnemc_names <- make_names(cnemc, cnemc_fields) |>
  rename(cnemc_row = .row_id, cnemc_name_field = name_field,
         cnemc_matched_name = matched_name)

# Haversine great-circle distance, in metres; also works on vectors.
distance_m <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * 6371000 * asin(pmin(1, sqrt(a)))
}

# ---- 4. Exact matches after name normalization -----------------------------
name_matches <- inner_join(fujian_names, cnemc_names, by = "name_key") |>
  distinct(fujian_row, cnemc_row, .keep_all = TRUE) |>
  left_join(fujian_meta, by = "fujian_row") |>
  left_join(cnemc_meta, by = "cnemc_row") |>
  mutate(
    distance_m = round(distance_m(fujian_lon, fujian_lat, cnemc_lon, cnemc_lat)),
    assessment = case_when(
      is.na(distance_m) ~ "Name match; coordinates unavailable",
      distance_m <= max_distance_m ~ "Name and nearby coordinates agree",
      distance_m > 5000 ~ "Name match BUT locations >5 km apart: verify",
      TRUE ~ "Name match; check coordinate difference"
    )
  ) |>
  arrange(fujian_station, cnemc_station)

write_csv(name_matches,
          file.path(output_dir, "01_exact_name_matches.csv"), na = "")

# A wide version retains the original river, basin, station code, and other
# crosswalk fields on both sides for manual identity verification.
fujian_full <- fujian |>
  mutate(fujian_row = row_number()) |>
  rename_with(~ paste0("fujian_", .x), .cols = -fujian_row)
cnemc_full <- cnemc |>
  mutate(cnemc_row = row_number()) |>
  rename_with(~ paste0("cnemc_", .x), .cols = -cnemc_row)

name_matches |>
  left_join(fujian_full, by = "fujian_row") |>
  left_join(cnemc_full, by = "cnemc_row") |>
  write_csv(file.path(output_dir, "04_name_matches_full_metadata.csv"), na = "")

# ---- 5. Possible same sites with different names ---------------------------
# Assess only stations having coordinates in both crosswalks.
fj_geo <- fujian_meta |>
  filter(!is.na(fujian_lon), !is.na(fujian_lat))
cn_geo <- cnemc_meta |>
  filter(!is.na(cnemc_lon), !is.na(cnemc_lat))

nearby_parts <- lapply(seq_len(nrow(fj_geo)), function(i) {
  # First limit candidates to a small bounding box (~1.1 km latitude).
  indices <- which(
    abs(cn_geo$cnemc_lat - fj_geo$fujian_lat[i]) < 0.01 &
    abs(cn_geo$cnemc_lon - fj_geo$fujian_lon[i]) < 0.015
  )
  if (length(indices) == 0L) return(NULL)

  metres <- distance_m(
    fj_geo$fujian_lon[i], fj_geo$fujian_lat[i],
    cn_geo$cnemc_lon[indices], cn_geo$cnemc_lat[indices]
  )
  keep <- which(is.finite(metres) & metres <= max_distance_m)
  if (length(keep) == 0L) return(NULL)

  tibble(
    fujian_row = fj_geo$fujian_row[i],
    cnemc_row = cn_geo$cnemc_row[indices[keep]],
    distance_m = round(metres[keep])
  )
})

geo_pairs <- bind_rows(nearby_parts)
if (nrow(geo_pairs) > 0L) {
  spatial_candidates <- geo_pairs |>
    anti_join(name_matches, by = c("fujian_row", "cnemc_row")) |>
    left_join(fujian_meta, by = "fujian_row") |>
    left_join(cnemc_meta, by = "cnemc_row") |>
    arrange(distance_m)
} else {
  spatial_candidates <- tibble(
    fujian_row = integer(), cnemc_row = integer(), distance_m = numeric(),
    fujian_station = character(), fujian_lon = numeric(), fujian_lat = numeric(),
    cnemc_station = character(), cnemc_lon = numeric(), cnemc_lat = numeric()
  )
}
write_csv(spatial_candidates,
          file.path(output_dir, "02_nearby_coordinates_different_names.csv"), na = "")

unmatched_fujian <- fujian_meta |>
  anti_join(name_matches, by = "fujian_row")
write_csv(unmatched_fujian,
          file.path(output_dir, "03_fujian_without_name_match.csv"), na = "")

# ---- 6. Console report -----------------------------------------------------
cat("\n======= OVERLAP SUMMARY =======\n")
cat("Name-matched station pairs: ", nrow(name_matches), "\n", sep = "")
cat("Distinct Fujian rows with a name match: ",
    n_distinct(name_matches$fujian_row), " / ", nrow(fujian), "\n", sep = "")
cat("Distinct CNEMC rows with a name match: ",
    n_distinct(name_matches$cnemc_row), " / ", nrow(cnemc), "\n", sep = "")
cat("Additional nearby-coordinate candidate pairs (<= ", max_distance_m,
    " m): ", nrow(spatial_candidates), "\n", sep = "")
cat("Fujian rows without a name match: ", nrow(unmatched_fujian), "\n", sep = "")
cat("\nExact matches (first 25):\n")
print(name_matches |>
        select(fujian_station, cnemc_station, fujian_matched_name,
               cnemc_matched_name, distance_m, assessment) |>
        head(25), n = 25)
cat("\nResults saved to: ", output_dir, "\n", sep = "")
