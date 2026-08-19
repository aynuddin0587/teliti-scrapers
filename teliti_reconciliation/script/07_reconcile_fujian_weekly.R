# ============================================================================
# 07_reconcile_fujian_weekly.R
#
# Teliti: PC <-> GitHub reconciliation for Fujian weekly surface-water archive.
#
# Purpose
#   Validate that the Windows collector and GitHub Actions collector represent
#   the same Fujian weekly source data before retiring the Windows schedule.
#
# Validation layers
#   1) annual canonical source-file discovery and schema checks
#   2) annual row counts and content hashes
#   3) observation-key agreement
#   4) published/scientific payload agreement
#   5) backend/internal metadata agreement (diagnostic only)
#   6) current-year refresh/timing differences
#   7) river/station/section coverage consistency
#
# Default locations
#   PC source:
#     D:/# R Project/penelitian/fujian_surfacewater/data/source
#   GitHub backup clone:
#     D:/# R Project/teliti-data-backup/fujian_weekly_surfacewater
#   Output:
#     D:/# R Project/penelitian/teliti_reconciliation/output/fujian_weekly
#
# Optional environment overrides
#   TELITI_ROOT
#   TELITI_BACKUP_ROOT
#   FUJIAN_PC_SOURCE_DIR
#   FUJIAN_GITHUB_ROOT
#   FUJIAN_RECON_OUTPUT_DIR
#
# Run from the main research repository:
#   Rscript teliti_reconciliation/script/07_reconcile_fujian_weekly.R
# ============================================================================

options(stringsAsFactors = FALSE)

# ---- Packages ---------------------------------------------------------------
required_packages <- c("dplyr", "readr", "tibble", "jsonlite", "digest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

# ---- Configuration ----------------------------------------------------------
TELITI_ROOT <- Sys.getenv("TELITI_ROOT", unset = "D:/# R Project/penelitian")
TELITI_BACKUP_ROOT <- Sys.getenv(
  "TELITI_BACKUP_ROOT",
  unset = "D:/# R Project/teliti-data-backup"
)

PC_SOURCE_DIR <- Sys.getenv(
  "FUJIAN_PC_SOURCE_DIR",
  unset = file.path(TELITI_ROOT, "fujian_surfacewater", "data", "source")
)
GITHUB_ROOT <- Sys.getenv(
  "FUJIAN_GITHUB_ROOT",
  unset = file.path(TELITI_BACKUP_ROOT, "fujian_weekly_surfacewater")
)
OUTPUT_DIR <- Sys.getenv(
  "FUJIAN_RECON_OUTPUT_DIR",
  unset = file.path(TELITI_ROOT, "teliti_reconciliation", "output", "fujian_weekly")
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

FIRST_YEAR <- 2004L
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
EXPECTED_YEARS <- seq.int(FIRST_YEAR, CURRENT_YEAR)

# Only these 14 fields are the public weekly-table record used for the primary
# scientific equivalence test. This mirrors the published fields validated by
# the collector itself.
PUBLISHED_FIELDS <- c(
  "s1", "s2", "s3", "s4", "s5",
  "f1", "f2", "f3", "f4", "f5", "f6",
  "s8", "s9", "s10"
)

# Collector/provenance fields are excluded from backend-content comparison.
# The source's own backend fields (recid, metadataid, docorder, etc.) remain.
PROVENANCE_FIELDS <- c(
  "source_page", "collected_at", "snapshot_md5",
  "collector_id", "collector_timezone", "github_run_id",
  "github_run_attempt", "github_commit", "github_sha"
)

# ---- Small helpers ----------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

trim_na <- function(x) {
  z <- trimws(as.character(x))
  z[z == ""] <- NA_character_
  z
}

normalize_station_name <- function(x) {
  z <- trim_na(x)
  if (length(z) == 0L) return(z)
  z <- gsub(intToUtf8(12288L), "", z, fixed = TRUE)
  z <- gsub("[[:space:]]+", "", z, perl = TRUE)
  trim_na(z)
}

normalize_source_label <- function(x) {
  z <- trim_na(x)
  if (length(z) == 0L) return(z)
  z <- gsub(intToUtf8(12288L), " ", z, fixed = TRUE)
  z <- gsub("[[:space:]]+", " ", z, perl = TRUE)
  trim_na(z)
}

safe_chr_col <- function(df, nm) {
  if (nm %in% names(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))
}

md5_text <- function(text) {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste0(text, collapse = ""))), tf)
  unname(tools::md5sum(tf))
}

canonical_table_hash <- function(df, fields) {
  fields <- intersect(fields, names(df))
  if (length(fields) == 0L) return(md5_text("[]"))
  if (nrow(df) == 0L) return(md5_text("[]"))

  x <- as.data.frame(df[, fields, drop = FALSE], stringsAsFactors = FALSE)
  x[] <- lapply(x, as.character)

  sort_fields <- intersect(c("s4", "s5", "s1", "s2", "s3", "recid", "docorder"), fields)
  if (length(sort_fields) > 0L) {
    ord <- do.call(
      order,
      c(lapply(sort_fields, function(nm) x[[nm]]), list(na.last = TRUE))
    )
    x <- x[ord, , drop = FALSE]
  }

  txt <- jsonlite::toJSON(
    x,
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null",
    null = "null",
    digits = NA,
    pretty = FALSE
  )
  md5_text(txt)
}

row_hash <- function(df, fields) {
  fields <- intersect(fields, names(df))
  if (nrow(df) == 0L) return(character())

  vapply(seq_len(nrow(df)), function(i) {
    vals <- vapply(fields, function(nm) {
      z <- df[[nm]][[i]]
      if (length(z) == 0L || is.na(z)) "<NA>" else enc2utf8(as.character(z))
    }, character(1))
    digest::digest(
      paste(paste(fields, vals, sep = "="), collapse = "\u001F"),
      algo = "xxhash64",
      serialize = FALSE
    )
  }, character(1))
}

extract_year <- function(path) {
  suppressWarnings(as.integer(sub(
    "^fujian_weekly_([0-9]{4})\\.rds$",
    "\\1",
    basename(path)
  )))
}

# ---- File discovery ---------------------------------------------------------
list_canonical_year_files <- function(root, recursive = FALSE) {
  if (!dir.exists(root)) return(character())
  list.files(
    root,
    pattern = "^fujian_weekly_[0-9]{4}\\.rds$",
    full.names = TRUE,
    recursive = recursive
  )
}

choose_github_file <- function(paths, year) {
  candidates <- paths[extract_year(paths) == year]
  if (length(candidates) == 0L) return(NA_character_)
  if (length(candidates) == 1L) return(candidates[[1]])

  p <- gsub("\\\\", "/", candidates)
  score <- rep(0, length(p))
  score <- score + ifelse(grepl("/state/", p, ignore.case = TRUE), 100, 0)
  score <- score + ifelse(grepl("/source/", p, ignore.case = TRUE), 50, 0)
  score <- score - ifelse(grepl("/snapshot|/archive", p, ignore.case = TRUE), 100, 0)
  score <- score - nchar(p) / 10000

  best <- which(score == max(score))
  if (length(best) != 1L) {
    stop(
      "Ambiguous GitHub canonical file for year ", year, ":\n  ",
      paste(candidates, collapse = "\n  "),
      call. = FALSE
    )
  }
  candidates[[best]]
}

pc_files_all <- list_canonical_year_files(PC_SOURCE_DIR, recursive = FALSE)
gh_files_all <- list_canonical_year_files(GITHUB_ROOT, recursive = TRUE)

if (length(pc_files_all) == 0L) {
  stop("No PC Fujian canonical year files found under: ", PC_SOURCE_DIR, call. = FALSE)
}
if (length(gh_files_all) == 0L) {
  stop(
    "No GitHub Fujian canonical year files found recursively under: ", GITHUB_ROOT,
    "\nExpected basenames such as fujian_weekly_2004.rds.",
    call. = FALSE
  )
}

file_index <- tibble::tibble(
  year = EXPECTED_YEARS,
  pc_path = vapply(EXPECTED_YEARS, function(y) {
    hit <- pc_files_all[extract_year(pc_files_all) == y]
    if (length(hit) == 0L) NA_character_ else hit[[1]]
  }, character(1)),
  github_path = vapply(EXPECTED_YEARS, function(y) choose_github_file(gh_files_all, y), character(1))
)

read_year <- function(path, expected_year, collector) {
  if (is.na(path) || !file.exists(path)) return(NULL)

  x <- readRDS(path)
  if (!inherits(x, "data.frame")) {
    stop(collector, " year ", expected_year, " is not a data.frame: ", path, call. = FALSE)
  }
  x <- tibble::as_tibble(x)

  missing <- setdiff(PUBLISHED_FIELDS, names(x))
  if (length(missing) > 0L) {
    stop(
      collector, " year ", expected_year,
      " is missing published field(s): ", paste(missing, collapse = ", "),
      "\nFile: ", path,
      call. = FALSE
    )
  }

  years_found <- unique(suppressWarnings(as.integer(trim_na(x$s4))))
  years_found <- years_found[!is.na(years_found)]
  if (length(years_found) > 0L && any(years_found != expected_year)) {
    stop(
      collector, " canonical year file ", expected_year,
      " contains unexpected year(s): ", paste(years_found, collapse = ", "),
      call. = FALSE
    )
  }

  x
}

build_keyed_rows <- function(dat) {
  if (is.null(dat)) return(NULL)

  out <- dat
  out$year_num <- suppressWarnings(as.integer(trim_na(out$s4)))
  out$week_num <- suppressWarnings(as.integer(trim_na(out$s5)))
  out$river_norm <- normalize_source_label(out$s1)
  out$station_norm <- normalize_station_name(out$s2)
  out$section_norm <- normalize_source_label(out$s3)

  recid <- trim_na(safe_chr_col(out, "recid"))
  docorder <- trim_na(safe_chr_col(out, "docorder"))

  fallback <- paste(
    out$year_num,
    out$week_num,
    ifelse(is.na(out$river_norm), "", out$river_norm),
    ifelse(is.na(out$station_norm), "", out$station_norm),
    ifelse(is.na(docorder), "", docorder),
    sep = "|"
  )

  out$observation_key <- ifelse(
    !is.na(recid) & nzchar(recid),
    paste0("recid:", recid),
    fallback
  )
  out$key_source <- ifelse(!is.na(recid) & nzchar(recid), "recid", "fallback")

  out$published_payload_hash <- row_hash(out, PUBLISHED_FIELDS)
  # backend_row_hash is computed later from fields common to both collectors.
  out$backend_row_hash <- NA_character_

  out
}

# ---- Reconcile one annual pair ---------------------------------------------
compare_one_year <- function(year, pc_path, github_path) {
  if (is.na(pc_path) || is.na(github_path)) {
    annual <- tibble::tibble(
      year = year,
      pc_path = pc_path,
      github_path = github_path,
      pc_rows = NA_integer_,
      github_rows = NA_integer_,
      pc_published_hash = NA_character_,
      github_published_hash = NA_character_,
      published_hash_equal = FALSE,
      pc_backend_hash = NA_character_,
      github_backend_hash = NA_character_,
      backend_hash_equal = FALSE,
      pc_unique_keys = NA_integer_,
      github_unique_keys = NA_integer_,
      shared_keys = NA_integer_,
      same_public_payload = NA_integer_,
      same_backend_row = NA_integer_,
      same_key_different_public_payload = NA_integer_,
      pc_only_keys = NA_integer_,
      github_only_keys = NA_integer_,
      public_payload_agreement_pct = NA_real_,
      pc_max_week = NA_integer_,
      github_max_week = NA_integer_,
      year_status = "MISSING_FILE"
    )
    return(list(annual = annual, exceptions = tibble::tibble(), pc = NULL, gh = NULL))
  }

  pc_raw <- read_year(pc_path, year, "PC")
  gh_raw <- read_year(github_path, year, "GitHub")
  pc <- build_keyed_rows(pc_raw)
  gh <- build_keyed_rows(gh_raw)

  pc_dup <- pc |> dplyr::count(.data$observation_key, name = "n") |> dplyr::filter(.data$n > 1L)
  gh_dup <- gh |> dplyr::count(.data$observation_key, name = "n") |> dplyr::filter(.data$n > 1L)
  if (nrow(pc_dup) > 0L || nrow(gh_dup) > 0L) {
    dup_file <- file.path(OUTPUT_DIR, sprintf("duplicate_keys_%d.csv", year))
    dup_out <- dplyr::bind_rows(
      dplyr::mutate(pc_dup, collector = "PC", .before = 1),
      dplyr::mutate(gh_dup, collector = "GitHub", .before = 1)
    )
    readr::write_csv(dup_out, dup_file, na = "")
    stop(
      "Duplicated observation keys detected for year ", year,
      ". Reconciliation stopped to avoid a many-to-many join. See: ", dup_file,
      call. = FALSE
    )
  }

  pc_pub_hash <- canonical_table_hash(pc_raw, PUBLISHED_FIELDS)
  gh_pub_hash <- canonical_table_hash(gh_raw, PUBLISHED_FIELDS)

  pc_backend_fields <- sort(setdiff(names(pc_raw), PROVENANCE_FIELDS))
  gh_backend_fields <- sort(setdiff(names(gh_raw), PROVENANCE_FIELDS))
  common_backend_fields <- intersect(pc_backend_fields, gh_backend_fields)
  pc_backend_hash <- canonical_table_hash(pc_raw, common_backend_fields)
  gh_backend_hash <- canonical_table_hash(gh_raw, common_backend_fields)
  pc$backend_row_hash <- row_hash(pc_raw, common_backend_fields)
  gh$backend_row_hash <- row_hash(gh_raw, common_backend_fields)

  pc_select <- pc |>
    dplyr::select(
      observation_key, key_source, year_num, week_num,
      river_norm, station_norm, section_norm,
      published_payload_hash, backend_row_hash,
      dplyr::all_of(PUBLISHED_FIELDS)
    ) |>
    dplyr::rename_with(~paste0("pc_", .x), -dplyr::all_of("observation_key"))

  gh_select <- gh |>
    dplyr::select(
      observation_key, key_source, year_num, week_num,
      river_norm, station_norm, section_norm,
      published_payload_hash, backend_row_hash,
      dplyr::all_of(PUBLISHED_FIELDS)
    ) |>
    dplyr::rename_with(~paste0("github_", .x), -dplyr::all_of("observation_key"))

  joined <- dplyr::full_join(pc_select, gh_select, by = "observation_key")

  joined <- joined |>
    dplyr::mutate(
      year = year,
      week = dplyr::coalesce(.data$pc_week_num, .data$github_week_num),
      river_system = dplyr::coalesce(.data$pc_river_norm, .data$github_river_norm),
      station_name = dplyr::coalesce(.data$pc_station_norm, .data$github_station_norm),
      section_status = dplyr::coalesce(.data$pc_section_norm, .data$github_section_norm),
      reconciliation_class = dplyr::case_when(
        is.na(.data$pc_published_payload_hash) ~ "github_only_key",
        is.na(.data$github_published_payload_hash) ~ "pc_only_key",
        .data$pc_published_payload_hash != .data$github_published_payload_hash ~ "same_key_different_public_payload",
        .data$pc_backend_row_hash != .data$github_backend_row_hash ~ "same_public_payload_backend_diff",
        TRUE ~ "confirmed_both_identical_backend"
      )
    )

  # List exactly which public fields changed for same-key payload differences.
  public_changed_fields <- function(i) {
    if (joined$reconciliation_class[[i]] != "same_key_different_public_payload") return(NA_character_)
    changed <- PUBLISHED_FIELDS[vapply(PUBLISHED_FIELDS, function(nm) {
      a <- joined[[paste0("pc_", nm)]][[i]]
      b <- joined[[paste0("github_", nm)]][[i]]
      if (is.na(a) && is.na(b)) return(FALSE)
      if (is.na(a) != is.na(b)) return(TRUE)
      !identical(as.character(a), as.character(b))
    }, logical(1))]
    paste(changed, collapse = "|")
  }
  joined$changed_public_fields <- vapply(seq_len(nrow(joined)), public_changed_fields, character(1))

  exceptions <- joined |>
    dplyr::filter(.data$reconciliation_class != "confirmed_both_identical_backend")

  class_counts <- table(joined$reconciliation_class)
  get_n <- function(nm) {
    if (nm %in% names(class_counts)) unname(as.integer(class_counts[[nm]])) else 0L
  }

  shared <- sum(
    !is.na(joined$pc_published_payload_hash) & !is.na(joined$github_published_payload_hash)
  )
  same_public <- sum(
    !is.na(joined$pc_published_payload_hash) &
      !is.na(joined$github_published_payload_hash) &
      joined$pc_published_payload_hash == joined$github_published_payload_hash
  )
  same_backend <- sum(
    !is.na(joined$pc_backend_row_hash) &
      !is.na(joined$github_backend_row_hash) &
      joined$pc_backend_row_hash == joined$github_backend_row_hash
  )

  pc_max_week <- suppressWarnings(max(pc$week_num, na.rm = TRUE))
  gh_max_week <- suppressWarnings(max(gh$week_num, na.rm = TRUE))
  if (!is.finite(pc_max_week)) pc_max_week <- NA_integer_
  if (!is.finite(gh_max_week)) gh_max_week <- NA_integer_

  year_status <- if (
    nrow(pc) == nrow(gh) &&
    get_n("pc_only_key") == 0L &&
    get_n("github_only_key") == 0L &&
    get_n("same_key_different_public_payload") == 0L
  ) {
    if (identical(pc_backend_hash, gh_backend_hash) && same_backend == shared) {
      "EXACT_BACKEND_MATCH"
    } else {
      "PUBLIC_MATCH_BACKEND_METADATA_DIFF"
    }
  } else {
    "DIFFERENT"
  }

  annual <- tibble::tibble(
    year = year,
    pc_path = pc_path,
    github_path = github_path,
    pc_rows = nrow(pc),
    github_rows = nrow(gh),
    pc_published_hash = pc_pub_hash,
    github_published_hash = gh_pub_hash,
    published_hash_equal = identical(pc_pub_hash, gh_pub_hash),
    pc_backend_hash = pc_backend_hash,
    github_backend_hash = gh_backend_hash,
    backend_hash_equal = identical(pc_backend_hash, gh_backend_hash),
    pc_unique_keys = dplyr::n_distinct(pc$observation_key),
    github_unique_keys = dplyr::n_distinct(gh$observation_key),
    shared_keys = shared,
    same_public_payload = same_public,
    same_backend_row = same_backend,
    same_key_different_public_payload = get_n("same_key_different_public_payload"),
    pc_only_keys = get_n("pc_only_key"),
    github_only_keys = get_n("github_only_key"),
    public_payload_agreement_pct = if (shared == 0L) NA_real_ else 100 * same_public / shared,
    pc_max_week = pc_max_week,
    github_max_week = gh_max_week,
    year_status = year_status
  )

  list(annual = annual, exceptions = exceptions, pc = pc, gh = gh)
}

# ---- Run annual comparisons -------------------------------------------------
cat("Fujian weekly PC <-> GitHub reconciliation\n")
cat("PC source:    ", PC_SOURCE_DIR, "\n", sep = "")
cat("GitHub root:  ", GITHUB_ROOT, "\n", sep = "")
cat("Output:       ", OUTPUT_DIR, "\n\n", sep = "")

results <- vector("list", nrow(file_index))
for (i in seq_len(nrow(file_index))) {
  y <- file_index$year[[i]]
  cat("Reconciling ", y, " ... ", sep = "")
  results[[i]] <- compare_one_year(
    year = y,
    pc_path = file_index$pc_path[[i]],
    github_path = file_index$github_path[[i]]
  )
  cat(results[[i]]$annual$year_status[[1]], "\n")
}

annual <- dplyr::bind_rows(lapply(results, `[[`, "annual"))
exceptions <- dplyr::bind_rows(lapply(results, `[[`, "exceptions"))

# ---- Current-year timing diagnostic ----------------------------------------
current_annual <- annual |> dplyr::filter(.data$year == CURRENT_YEAR)
current_exceptions <- exceptions |>
  dplyr::filter(
    .data$year == CURRENT_YEAR,
    .data$reconciliation_class %in% c(
      "github_only_key",
      "pc_only_key",
      "same_key_different_public_payload"
    )
  )

current_recent_only <- FALSE
current_recent_threshold <- NA_integer_
if (nrow(current_annual) == 1L && current_annual$year_status[[1]] == "DIFFERENT") {
  max_week <- suppressWarnings(max(
    c(current_annual$pc_max_week[[1]], current_annual$github_max_week[[1]]),
    na.rm = TRUE
  ))
  if (is.finite(max_week)) {
    # Allow the latest two source weeks as a timing/revision window. This does
    # not erase differences; it classifies them for operational interpretation.
    current_recent_threshold <- max(1L, as.integer(max_week) - 1L)
    current_recent_only <- nrow(current_exceptions) > 0L &&
      all(!is.na(current_exceptions$week)) &&
      all(current_exceptions$week >= current_recent_threshold)
  }
}

if (nrow(current_annual) == 1L && current_recent_only) {
  annual$year_status[annual$year == CURRENT_YEAR] <- "CURRENT_YEAR_RECENT_TIMING_OR_REVISION"
}

# ---- Station / section consistency -----------------------------------------
station_summary_for <- function(which_collector) {
  dat_list <- lapply(results, function(z) z[[which_collector]])
  dat <- dplyr::bind_rows(dat_list)
  if (nrow(dat) == 0L) return(tibble::tibble())

  dat |>
    dplyr::filter(
      !is.na(.data$river_norm) | !is.na(.data$station_norm) | !is.na(.data$section_norm)
    ) |>
    dplyr::group_by(.data$river_norm, .data$station_norm, .data$section_norm) |>
    dplyr::summarise(
      first_year = suppressWarnings(min(.data$year_num, na.rm = TRUE)),
      last_year = suppressWarnings(max(.data$year_num, na.rm = TRUE)),
      n_records = dplyr::n(),
      .groups = "drop"
    )
}

pc_station <- station_summary_for("pc") |>
  dplyr::rename(
    river_system = river_norm,
    station_name = station_norm,
    section_status = section_norm,
    pc_first_year = first_year,
    pc_last_year = last_year,
    pc_records = n_records
  )

gh_station <- station_summary_for("gh") |>
  dplyr::rename(
    river_system = river_norm,
    station_name = station_norm,
    section_status = section_norm,
    github_first_year = first_year,
    github_last_year = last_year,
    github_records = n_records
  )

station_section <- dplyr::full_join(
  pc_station,
  gh_station,
  by = c("river_system", "station_name", "section_status")
) |>
  dplyr::mutate(
    status = dplyr::case_when(
      is.na(.data$pc_records) ~ "github_only_station_section",
      is.na(.data$github_records) ~ "pc_only_station_section",
      TRUE ~ "confirmed_both"
    )
  ) |>
  dplyr::arrange(.data$status, .data$river_system, .data$station_name, .data$section_status)

# ---- Overall decision -------------------------------------------------------
missing_file_years <- annual$year[annual$year_status == "MISSING_FILE"]
historical <- annual |> dplyr::filter(.data$year < CURRENT_YEAR)
historical_public_pass <- nrow(historical) > 0L && all(
  historical$year_status %in% c("EXACT_BACKEND_MATCH", "PUBLIC_MATCH_BACKEND_METADATA_DIFF")
)

current_status <- if (nrow(current_annual) == 1L) {
  annual$year_status[annual$year == CURRENT_YEAR][[1]]
} else {
  "MISSING_FILE"
}

station_only_n <- sum(station_section$status != "confirmed_both")
public_exception_n <- if (nrow(exceptions) == 0L) 0L else sum(
  exceptions$reconciliation_class %in% c(
    "github_only_key",
    "pc_only_key",
    "same_key_different_public_payload"
  )
)
backend_metadata_diff_n <- if (nrow(exceptions) == 0L) 0L else sum(
  exceptions$reconciliation_class == "same_public_payload_backend_diff"
)
total_shared_keys <- sum(annual$shared_keys, na.rm = TRUE)
total_same_public <- sum(annual$same_public_payload, na.rm = TRUE)
overall_public_agreement_pct <- if (total_shared_keys == 0L) {
  NA_real_
} else {
  100 * total_same_public / total_shared_keys
}

if (length(missing_file_years) > 0L) {
  overall_status <- "FAIL"
  overall_reason <- paste0(
    "Missing canonical annual file(s): ", paste(missing_file_years, collapse = ", ")
  )
} else if (historical_public_pass && current_status %in% c(
  "EXACT_BACKEND_MATCH", "PUBLIC_MATCH_BACKEND_METADATA_DIFF"
)) {
  overall_status <- "PASS"
  overall_reason <- "All historical and current-year observation keys and published payloads agree."
} else if (historical_public_pass && current_status == "CURRENT_YEAR_RECENT_TIMING_OR_REVISION") {
  overall_status <- "PASS_WITH_CURRENT_YEAR_TIMING"
  overall_reason <- paste0(
    "All completed historical years agree; current-year exceptions are confined to source week ",
    current_recent_threshold, " or later and are consistent with refresh timing or recent source revision."
  )
} else {
  overall_status <- "REVIEW"
  overall_reason <- paste0(
    "At least one historical year or a non-recent current-year record differs. Review row exceptions before retiring the PC task."
  )
}

# ---- Write outputs ----------------------------------------------------------
annual_file <- file.path(OUTPUT_DIR, "fujian_annual_comparison.csv")
exception_file <- file.path(OUTPUT_DIR, "fujian_row_exceptions.csv")
station_file <- file.path(OUTPUT_DIR, "fujian_station_section_comparison.csv")
summary_file <- file.path(OUTPUT_DIR, "fujian_reconciliation_summary.txt")

readr::write_csv(annual, annual_file, na = "")
readr::write_csv(exceptions, exception_file, na = "")
readr::write_csv(station_section, station_file, na = "")

summary_lines <- c(
  "Teliti Fujian weekly PC <-> GitHub reconciliation",
  paste0("Run date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("Overall status: ", overall_status),
  paste0("Reason: ", overall_reason),
  "",
  paste0("Expected years: ", FIRST_YEAR, "-", CURRENT_YEAR, " (", length(EXPECTED_YEARS), ")"),
  paste0("Historical public-data pass: ", historical_public_pass),
  paste0("Current-year status: ", current_status),
  paste0("Total PC rows: ", sum(annual$pc_rows, na.rm = TRUE)),
  paste0("Total GitHub rows: ", sum(annual$github_rows, na.rm = TRUE)),
  paste0("Public/key exceptions: ", public_exception_n),
  paste0("Backend-metadata-only differences: ", backend_metadata_diff_n),
  paste0("Overall shared-key published-payload agreement (%): ", sprintf("%.6f", overall_public_agreement_pct)),
  paste0("Station/section combinations not represented by both: ", station_only_n),
  "",
  "Annual status counts:",
  paste(capture.output(print(table(annual$year_status))), collapse = "\n"),
  "",
  "Exception class counts:",
  if (nrow(exceptions) == 0L) "none" else paste(capture.output(print(table(exceptions$reconciliation_class))), collapse = "\n"),
  "",
  paste0("Annual comparison: ", annual_file),
  paste0("Row exceptions: ", exception_file),
  paste0("Station/section comparison: ", station_file)
)
writeLines(summary_lines, summary_file, useBytes = TRUE)

# ---- Console summary --------------------------------------------------------
cat("\n================ Fujian reconciliation summary ================\n")
cat("Overall status: ", overall_status, "\n", sep = "")
cat("Reason: ", overall_reason, "\n", sep = "")
cat("Historical public-data pass: ", historical_public_pass, "\n", sep = "")
cat("Current-year status: ", current_status, "\n", sep = "")
cat("Total PC rows: ", sum(annual$pc_rows, na.rm = TRUE), "\n", sep = "")
cat("Total GitHub rows: ", sum(annual$github_rows, na.rm = TRUE), "\n", sep = "")
cat("Public/key exceptions: ", public_exception_n, "\n", sep = "")
cat("Backend-metadata-only differences: ", backend_metadata_diff_n, "\n", sep = "")
cat("Overall shared-key published-payload agreement: ", sprintf("%.6f%%", overall_public_agreement_pct), "\n", sep = "")
cat("Station/section combinations not represented by both: ", station_only_n, "\n", sep = "")
cat("\nAnnual status counts:\n")
print(table(annual$year_status))
cat("\nException class counts:\n")
if (nrow(exceptions) == 0L) cat("none\n") else print(table(exceptions$reconciliation_class))
cat("\nOutputs:\n")
cat("  ", annual_file, "\n", sep = "")
cat("  ", exception_file, "\n", sep = "")
cat("  ", station_file, "\n", sep = "")
cat("  ", summary_file, "\n", sep = "")

# Fail only for missing/structural inputs. REVIEW is a scientifically meaningful
# reconciliation outcome and should still leave its diagnostics available.
if (overall_status == "FAIL") quit(save = "no", status = 1L)
quit(save = "no", status = 0L)