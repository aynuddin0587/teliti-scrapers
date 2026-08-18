# ============================================================
# Teliti private-backup storage audit
# ============================================================

options(stringsAsFactors = FALSE)

BACKUP_REPO <- Sys.getenv("TELITI_BACKUP_REPO", unset = "")
TIMEZONE <- Sys.getenv("TELITI_TIMEZONE", unset = "Asia/Taipei")

if (!nzchar(BACKUP_REPO)) {
  stop("TELITI_BACKUP_REPO is not defined.")
}

if (!dir.exists(file.path(BACKUP_REPO, ".git"))) {
  stop("TELITI_BACKUP_REPO is not a Git repository: ", BACKUP_REPO)
}

BACKUP_REPO <- normalizePath(BACKUP_REPO, winslash = "/", mustWork = TRUE)
setwd(BACKUP_REPO)

OUT_DIR <- file.path(BACKUP_REPO, "system", "storage")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

CURRENT_FILE <- file.path(OUT_DIR, "storage_audit_current.csv")
HISTORY_FILE <- file.path(OUT_DIR, "storage_audit_history.csv")
SUMMARY_FILE <- file.path(OUT_DIR, "storage_audit_summary.md")

now <- Sys.time()
audited_at <- format(now, "%Y-%m-%d %H:%M:%S%z", tz = TIMEZONE)

run_git <- function(args) {
  out <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      "Git command failed: git ", paste(args, collapse = " "), "\n",
      paste(out, collapse = "\n")
    )
  }
  out
}

as_number_or_na <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(x)) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

first_or_na <- function(x) {
  if (length(x) == 0L) NA_character_ else x[[1L]]
}

file_inventory <- function(path) {
  if (!dir.exists(path)) {
    return(list(files = character(), bytes = 0, count = 0L))
  }

  files <- list.files(
    path,
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )

  if (length(files) == 0L) {
    return(list(files = character(), bytes = 0, count = 0L))
  }

  info <- file.info(files)
  bytes <- sum(info$size, na.rm = TRUE)

  list(files = files, bytes = bytes, count = length(files))
}

commit_stats <- function(pathspec) {
  commit_count <- as.integer(first_or_na(
    run_git(c("rev-list", "--count", "HEAD", "--", pathspec))
  ))

  newest <- first_or_na(
    run_git(c("log", "-1", "--format=%cI", "--", pathspec))
  )

  oldest_all <- run_git(c("log", "--reverse", "--format=%cI", "--", pathspec))
  oldest <- first_or_na(oldest_all)

  list(
    commits = commit_count,
    oldest_commit_at = oldest,
    newest_commit_at = newest
  )
}

# ------------------------------------------------------------
# Git object-store statistics
# ------------------------------------------------------------

git_counts <- run_git(c("count-objects", "-v"))

count_map <- list()
for (line in git_counts) {
  parts <- strsplit(line, ":", fixed = TRUE)[[1L]]
  if (length(parts) >= 2L) {
    key <- trimws(parts[[1L]])
    value <- trimws(paste(parts[-1L], collapse = ":"))
    count_map[[key]] <- value
  }
}

loose_kib <- as_number_or_na(count_map[["size"]])
pack_kib <- as_number_or_na(count_map[["size-pack"]])
garbage_kib <- as_number_or_na(count_map[["size-garbage"]])

loose_kib <- ifelse(is.na(loose_kib), 0, loose_kib)
pack_kib <- ifelse(is.na(pack_kib), 0, pack_kib)
garbage_kib <- ifelse(is.na(garbage_kib), 0, garbage_kib)

git_object_mb <- (loose_kib + pack_kib + garbage_kib) / 1024
repo_commits <- as.integer(first_or_na(run_git(c("rev-list", "--count", "HEAD"))))

# ------------------------------------------------------------
# Dataset-level inventory
# ------------------------------------------------------------

top_dirs <- list.dirs(BACKUP_REPO, recursive = FALSE, full.names = FALSE)
top_dirs <- sort(setdiff(top_dirs, c(".git", "system")))

rows <- vector("list", length(top_dirs) + 1L)

for (i in seq_along(top_dirs)) {
  dataset <- top_dirs[[i]]
  dataset_path <- file.path(BACKUP_REPO, dataset)

  inv <- file_inventory(dataset_path)
  snap <- file_inventory(file.path(dataset_path, "snapshots"))
  cs <- commit_stats(dataset)

  rows[[i]] <- data.frame(
    audited_at = audited_at,
    dataset = dataset,
    file_count = inv$count,
    working_size_mb = round(inv$bytes / 1024^2, 4),
    snapshot_count = snap$count,
    commits_touching_dataset = cs$commits,
    oldest_commit_at = cs$oldest_commit_at,
    newest_commit_at = cs$newest_commit_at,
    repo_commit_count = NA_integer_,
    git_object_size_mb = NA_real_,
    git_pack_size_mb = NA_real_,
    git_loose_size_mb = NA_real_,
    size_change_mb_since_previous = NA_real_,
    git_change_mb_since_previous = NA_real_,
    days_since_previous = NA_real_,
    growth_mb_per_day = NA_real_,
    projected_gb_per_year = NA_real_,
    status = NA_character_,
    stringsAsFactors = FALSE
  )
}

repo_files <- list.files(
  BACKUP_REPO,
  recursive = TRUE,
  all.files = TRUE,
  full.names = TRUE,
  include.dirs = FALSE,
  no.. = TRUE
)
repo_files <- repo_files[!grepl("/\\.git/", gsub("\\\\", "/", repo_files))]
repo_bytes <- if (length(repo_files)) sum(file.info(repo_files)$size, na.rm = TRUE) else 0

policy_status <- if (git_object_mb < 500) {
  "HEALTHY"
} else if (git_object_mb < 1024) {
  "WATCH"
} else if (git_object_mb < 5120) {
  "ACTION"
} else {
  "CRITICAL"
}

rows[[length(rows)]] <- data.frame(
  audited_at = audited_at,
  dataset = "_repository",
  file_count = length(repo_files),
  working_size_mb = round(repo_bytes / 1024^2, 4),
  snapshot_count = sum(vapply(top_dirs, function(d) {
    file_inventory(file.path(BACKUP_REPO, d, "snapshots"))$count
  }, integer(1))),
  commits_touching_dataset = NA_integer_,
  oldest_commit_at = first_or_na(run_git(c("log", "--reverse", "--format=%cI"))),
  newest_commit_at = first_or_na(run_git(c("log", "-1", "--format=%cI"))),
  repo_commit_count = repo_commits,
  git_object_size_mb = round(git_object_mb, 4),
  git_pack_size_mb = round(pack_kib / 1024, 4),
  git_loose_size_mb = round(loose_kib / 1024, 4),
  size_change_mb_since_previous = NA_real_,
  git_change_mb_since_previous = NA_real_,
  days_since_previous = NA_real_,
  growth_mb_per_day = NA_real_,
  projected_gb_per_year = NA_real_,
  status = policy_status,
  stringsAsFactors = FALSE
)

current <- do.call(rbind, rows)

# ------------------------------------------------------------
# Compare with previous audit, if available
# ------------------------------------------------------------

history_old <- NULL
if (file.exists(HISTORY_FILE)) {
  history_old <- tryCatch(
    utils::read.csv(HISTORY_FILE, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

if (!is.null(history_old) && nrow(history_old) > 0L && "audited_at" %in% names(history_old)) {
  old_times <- as.POSIXct(
    history_old$audited_at,
    format = "%Y-%m-%d %H:%M:%S%z",
    tz = TIMEZONE
  )

  valid_times <- old_times[!is.na(old_times)]

  if (length(valid_times) > 0L) {
    previous_time <- max(valid_times)
    previous_time_str <- history_old$audited_at[which(old_times == previous_time)[1L]]
    previous <- history_old[history_old$audited_at == previous_time_str, , drop = FALSE]

    days_delta <- as.numeric(difftime(now, previous_time, units = "days"))

    for (i in seq_len(nrow(current))) {
      ds <- current$dataset[[i]]
      prev <- previous[previous$dataset == ds, , drop = FALSE]

      if (nrow(prev) == 1L && is.finite(days_delta) && days_delta > 0) {
        current$days_since_previous[[i]] <- round(days_delta, 4)

        prev_size <- suppressWarnings(as.numeric(prev$working_size_mb[[1L]]))
        if (!is.na(prev_size)) {
          delta <- current$working_size_mb[[i]] - prev_size
          current$size_change_mb_since_previous[[i]] <- round(delta, 4)
        }

        if (ds == "_repository" && "git_object_size_mb" %in% names(prev)) {
          prev_git <- suppressWarnings(as.numeric(prev$git_object_size_mb[[1L]]))
          if (!is.na(prev_git)) {
            git_delta <- current$git_object_size_mb[[i]] - prev_git
            current$git_change_mb_since_previous[[i]] <- round(git_delta, 4)
            growth <- git_delta / days_delta
            current$growth_mb_per_day[[i]] <- round(growth, 4)
            current$projected_gb_per_year[[i]] <- round((growth * 365) / 1024, 4)
          }
        } else if (!is.na(current$size_change_mb_since_previous[[i]])) {
          growth <- current$size_change_mb_since_previous[[i]] / days_delta
          current$growth_mb_per_day[[i]] <- round(growth, 4)
          current$projected_gb_per_year[[i]] <- round((growth * 365) / 1024, 4)
        }
      }
    }
  }
}

# ------------------------------------------------------------
# Write current + append history
# ------------------------------------------------------------

utils::write.csv(current, CURRENT_FILE, row.names = FALSE, na = "")

if (is.null(history_old) || nrow(history_old) == 0L) {
  history_new <- current
} else {
  # Align columns if the audit schema evolves later.
  all_names <- union(names(history_old), names(current))
  for (nm in setdiff(all_names, names(history_old))) history_old[[nm]] <- NA
  for (nm in setdiff(all_names, names(current))) current[[nm]] <- NA
  history_new <- rbind(history_old[, all_names, drop = FALSE], current[, all_names, drop = FALSE])
}

utils::write.csv(history_new, HISTORY_FILE, row.names = FALSE, na = "")

# ------------------------------------------------------------
# Markdown summary
# ------------------------------------------------------------

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "", format(round(x, digits), nsmall = digits, trim = TRUE))
}

summary_lines <- c(
  "# Teliti Backup Storage Audit",
  "",
  paste0("Audited at: **", audited_at, "**"),
  "",
  paste0("Repository status: **", policy_status, "**"),
  paste0("Repository working tree: **", fmt_num(current$working_size_mb[current$dataset == "_repository"]), " MB**"),
  paste0("Local full-history Git object store: **", fmt_num(current$git_object_size_mb[current$dataset == "_repository"]), " MB**"),
  paste0("Repository commits: **", repo_commits, "**"),
  "",
  "Project policy thresholds (conservative internal thresholds, not GitHub hard limits):",
  "- HEALTHY: < 500 MiB Git objects",
  "- WATCH: 500 MiB to < 1 GiB",
  "- ACTION: 1 GiB to < 5 GiB",
  "- CRITICAL: >= 5 GiB",
  "",
  "## Dataset inventory",
  "",
  "| Dataset | Files | Working MB | Snapshots | Commits | MB/day since previous audit | Projected GB/year |",
  "|---|---:|---:|---:|---:|---:|---:|"
)

for (i in which(current$dataset != "_repository")) {
  r <- current[i, ]
  summary_lines <- c(
    summary_lines,
    paste0(
      "| ", r$dataset,
      " | ", r$file_count,
      " | ", fmt_num(r$working_size_mb),
      " | ", r$snapshot_count,
      " | ", r$commits_touching_dataset,
      " | ", fmt_num(r$growth_mb_per_day, 4),
      " | ", fmt_num(r$projected_gb_per_year, 4),
      " |"
    )
  )
}

repo_row <- current[current$dataset == "_repository", , drop = FALSE]
summary_lines <- c(
  summary_lines,
  "",
  "## Repository growth",
  "",
  paste0("Git-object change since previous audit: **", fmt_num(repo_row$git_change_mb_since_previous, 4), " MB**"),
  paste0("Observed Git-object growth: **", fmt_num(repo_row$growth_mb_per_day, 4), " MB/day**"),
  paste0("Simple annualized projection: **", fmt_num(repo_row$projected_gb_per_year, 4), " GB/year**"),
  "",
  "The annualized projection is only meaningful after multiple audits; early values can be highly volatile."
)

writeLines(summary_lines, SUMMARY_FILE, useBytes = TRUE)

cat("Teliti storage audit complete.\n")
cat("Current report:", CURRENT_FILE, "\n")
cat("History:", HISTORY_FILE, "\n")
cat("Summary:", SUMMARY_FILE, "\n")
cat("Repository status:", policy_status, "\n")
cat("Working tree MB:", round(repo_bytes / 1024^2, 3), "\n")
cat("Git object MB:", round(git_object_mb, 3), "\n")