# ============================================================================
# 01_fujian_weekly_surfacewater_archive_v10.R
#
# Fujian Province surface-water weekly report archive, 2004-present.
# Source page:
#   https://sthjt.fujian.gov.cn/wsbs/bmfwcx/szcx/
#
# V11 strategy:
# - Do NOT depend on avalon.vmodels internals.
# - Open the official page with Chromote.
# - Inject a small logger before page JavaScript runs.
# - Capture the page's own XHR/fetch request and response for list2.was.
# - Discover the real backend request, response structure, and pagination.
# - Replay that request from inside the same browser session.
# - Developer Tools confirmed the site's native WAS5 filter grammar:
#     (dockind=10)*(s4=YYYY)*(s5=WEEK)
#   with page=1 and prepage=50 in the site's own request.
# - Annual pagination is intentionally avoided because sortfield=-s4 is not a stable
#   ordering once all rows are filtered to one year. This caused boundary overlap and
#   omission/reordering in V6-V8.
# - Historical backfill therefore queries each year x week partition directly.
# - Each weekly partition is required to fit in one response. The script raises prepage
#   automatically when needed and validates captured rows against that partition's own
#   backend count. No deduplication is performed: duplicate published rows are preserved.
# - Completed historical weeks are checkpointed so interrupted backfills can resume.
# - The 53 weekly counts are compared with the native annual count as a QA check.
# - No browser interaction with the year/week dropdowns is required.
# - WAS5 appends a synthetic footer object {"recid":"id"} to docs. V11 removes
#   only that exact sentinel before any row counting or processing; it is not a water record.
# - Real source recid/metadataid values are preserved as provenance identifiers.
# - Unfiltered results reveal records back to 2004 (earliest observed: 2004 week 37),
#   so the historical collection begins in 2004 even though the GUI year dropdown begins at 2010.
#
# Modes:
#   auto      default; full backfill if any year file is missing, otherwise
#             refresh only the current year.
#   backfill  retrieve each year independently and rebuild all year files.
#   current   retrieve only the current year using a direct server-side s4 filter.
#
# RStudio:
#   source("fujian_surfacewater/script/01_fujian_weekly_surfacewater_archive.R")
#
# Command line:
#   Rscript 01_fujian_weekly_surfacewater_archive.R backfill
#   Rscript 01_fujian_weekly_surfacewater_archive.R current
# ============================================================================

options(stringsAsFactors = FALSE)

# ---- Configuration ---------------------------------------------------------
TELITI_DATA_ROOT <- Sys.getenv(
  "TELITI_DATA_ROOT",
  unset = "D:/# R Project/penelitian"
)

BASE_DIR <- Sys.getenv(
  "FUJIAN_WATER_BASE_DIR",
  unset = ""
)

if (!nzchar(BASE_DIR)) {
  BASE_DIR <- file.path(
    TELITI_DATA_ROOT,
    "fujian_surfacewater"
  )
}

COLLECTOR_TZ <- Sys.getenv(
  "TELITI_TIMEZONE",
  unset = "Asia/Taipei"
)

COLLECTOR_ID <- Sys.getenv(
  "TELITI_COLLECTOR_ID",
  unset = "local_pc"
)

GITHUB_RUN_ID <- Sys.getenv("GITHUB_RUN_ID", unset = "")
GITHUB_RUN_ATTEMPT <- Sys.getenv("GITHUB_RUN_ATTEMPT", unset = "")
GITHUB_SHA <- Sys.getenv("GITHUB_SHA", unset = "")

SOURCE_URL <- "https://sthjt.fujian.gov.cn/wsbs/bmfwcx/szcx/"
FIRST_YEAR <- 2004L
CURRENT_YEAR <- as.integer(format(Sys.time(), "%Y", tz = COLLECTOR_TZ))
WAIT_TIMEOUT_SEC <- 75
PAGE_DELAY_SEC <- 0.40
MAX_PAGE_RETRIES <- 6L
YEAR_PAUSE_SEC <- 1.50
WEEK_PREPAGE <- 500L
MAX_SINGLE_RESPONSE_PREPAGE <- 5000L

SOURCE_DIR <- file.path(BASE_DIR, "data", "source")
ARCHIVE_DIR <- file.path(SOURCE_DIR, "archive")
PROCESSED_DIR <- file.path(BASE_DIR, "data", "processed")
LOG_DIR <- file.path(BASE_DIR, "log")
SCRIPT_DIR <- file.path(BASE_DIR, "script")
CHECKPOINT_DIR <- file.path(SOURCE_DIR, ".checkpoints")
DIAGNOSTIC_DIR <- file.path(SOURCE_DIR, "diagnostics")

invisible(lapply(
  c(BASE_DIR, SOURCE_DIR, ARCHIVE_DIR, PROCESSED_DIR, LOG_DIR, SCRIPT_DIR, CHECKPOINT_DIR, DIAGNOSTIC_DIR),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

# ---- Packages --------------------------------------------------------------
required_packages <- c("chromote", "jsonlite", "dplyr", "readr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them first with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---- Logging ---------------------------------------------------------------
log_file <- file.path(
  LOG_DIR,
  paste0(
    "fujian_weekly_surfacewater_",
    format(Sys.time(), "%Y%m%d", tz = COLLECTOR_TZ),
    ".log"
  )
)

log_msg <- function(...) {
  msg <- paste0(...)
  line <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = COLLECTOR_TZ),
    " | ",
    msg
  )
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

# ---- Mode ------------------------------------------------------------------
get_mode <- function() {
  env_mode <- Sys.getenv("FUJIAN_WATER_MODE", unset = "")
  if (nzchar(env_mode)) return(tolower(env_mode))

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 0 && tolower(args[[1]]) %in% c("auto", "backfill", "current")) {
    return(tolower(args[[1]]))
  }
  "auto"
}

MODE <- get_mode()
if (!MODE %in% c("auto", "backfill", "current")) {
  stop("Mode must be one of: auto, backfill, current", call. = FALSE)
}

canonical_source_file <- function(year) {
  file.path(SOURCE_DIR, sprintf("fujian_weekly_%d.rds", year))
}

existing_years <- function() {
  x <- list.files(
    SOURCE_DIR,
    pattern = "^fujian_weekly_[0-9]{4}\\.rds$",
    full.names = FALSE
  )
  if (length(x) == 0) return(integer())
  as.integer(sub("^fujian_weekly_([0-9]{4})\\.rds$", "\\1", x))
}

all_years <- seq.int(FIRST_YEAR, CURRENT_YEAR)
missing_years <- setdiff(all_years, existing_years())
NEED_BACKFILL <- MODE == "backfill" || (MODE == "auto" && length(missing_years) > 0)

# ---- General helpers -------------------------------------------------------
trim_na <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

md5_text <- function(text) {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), tf)
  unname(tools::md5sum(tf))
}

stable_snapshot_md5 <- function(df) {
  if (nrow(df) == 0) return(md5_text("[]"))

  sort_cols <- intersect(c("s4", "s5", "s1", "s2", "s3"), names(df))
  if (length(sort_cols) > 0) {
    ord <- do.call(
      order,
      c(lapply(sort_cols, function(nm) as.character(df[[nm]])), list(na.last = TRUE))
    )
    df <- df[ord, , drop = FALSE]
  }

  keep <- setdiff(names(df), c("source_page", "collected_at", "snapshot_md5"))
  txt <- jsonlite::toJSON(
    df[, keep, drop = FALSE],
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null",
    null = "null",
    digits = NA,
    pretty = FALSE
  )
  md5_text(txt)
}

# ---- Chromote helpers ------------------------------------------------------
extract_cdp_value <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.null(x$result$value)) return(x$result$value)
  if (!is.null(x$result$result$value)) return(x$result$result$value)
  if (!is.null(x$value)) return(x$value)
  NULL
}

eval_js <- function(session, expression) {
  out <- session$Runtime$evaluate(
    expression = expression,
    returnByValue = TRUE,
    awaitPromise = TRUE
  )
  extract_cdp_value(out)
}

wait_until <- function(session, expression, timeout_sec = WAIT_TIMEOUT_SEC, poll_sec = 0.30) {
  start <- Sys.time()
  repeat {
    value <- tryCatch(
      eval_js(session, paste0("Boolean(", expression, ")")),
      error = function(e) FALSE
    )
    if (isTRUE(value)) return(TRUE)

    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout_sec) {
      return(FALSE)
    }
    Sys.sleep(poll_sec)
  }
}

# This logger is injected BEFORE the site's JavaScript runs. It records the
# public list request and its response without needing Avalon internals.
LOGGER_JS <- paste0(
  "(() => {",
  "if (window.__fj_logger_installed) return;",
  "window.__fj_logger_installed = true;",
  "window.__fj_netlog = [];",
  "const push = x => { try { window.__fj_netlog.push(x); } catch(e) {} };",
  "const X = XMLHttpRequest.prototype;",
  "const oOpen = X.open, oSend = X.send, oSet = X.setRequestHeader;",
  "X.open = function(method, url) {",
  "this.__fj_method = String(method || 'GET').toUpperCase();",
  "this.__fj_url = String(url || '');",
  "this.__fj_headers = {};",
  "return oOpen.apply(this, arguments);",
  "};",
  "X.setRequestHeader = function(k, v) {",
  "try { this.__fj_headers[String(k)] = String(v); } catch(e) {}",
  "return oSet.apply(this, arguments);",
  "};",
  "X.send = function(body) {",
  "const self = this;",
  "const reqBody = body == null ? '' : String(body);",
  "self.addEventListener('loadend', function() {",
  "let txt = '';",
  "try { txt = self.responseText == null ? '' : String(self.responseText); } catch(e) {}",
  "if (!txt) { try { if (self.response != null) txt = typeof self.response === 'string' ? self.response : JSON.stringify(self.response); } catch(e) {} }",
  "push({kind:'xhr',method:self.__fj_method||'GET',url:self.responseURL||self.__fj_url||'',",
  "request_body:reqBody,request_headers:self.__fj_headers||{},status:Number(self.status||0),response_text:txt});",
  "}, {once:true});",
  "return oSend.apply(this, arguments);",
  "};",
  "if (window.fetch) {",
  "const oFetch = window.fetch;",
  "window.fetch = function(input, init) {",
  "const u = typeof input === 'string' ? input : (input && input.url ? input.url : '');",
  "const m = init && init.method ? String(init.method).toUpperCase() : 'GET';",
  "const b = init && init.body != null ? String(init.body) : '';",
  "return oFetch.apply(this, arguments).then(function(resp) {",
  "try { const c = resp.clone(); c.text().then(function(txt) {",
  "push({kind:'fetch',method:m,url:resp.url||u,request_body:b,request_headers:{},",
  "status:Number(resp.status||0),response_text:String(txt||'')});",
  "}); } catch(e) {}",
  "return resp;",
  "});",
  "};",
  "}",
  "})();"
)

read_netlog <- function(session) {
  txt <- eval_js(
    session,
    "JSON.stringify(window.__fj_netlog || [])"
  )
  if (is.null(txt) || !nzchar(txt)) return(list())
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

netlog_count <- function(session) {
  as.integer(eval_js(session, "(window.__fj_netlog || []).length") %||% 0L)
}

# ---- Response discovery ----------------------------------------------------
looks_like_record <- function(x) {
  is.list(x) && all(c("s1", "s2", "s4", "s5") %in% names(x))
}

# WAS5 appends a synthetic footer/sentinel object to the docs array.
# Developer Tools/raw response confirmed the exact form: {"recid":"id"}.
# It is not included in the backend count and must never become a data row.
is_was5_sentinel <- function(x) {
  if (!is.list(x) || length(x) != 1L) return(FALSE)
  nms <- names(x)
  identical(nms, "recid") && identical(as.character(x$recid %||% ""), "id")
}

strip_was5_sentinel <- function(docs) {
  if (is.null(docs) || length(docs) == 0L) {
    return(list(docs = list(), sentinel_count = 0L))
  }
  is_sentinel <- vapply(docs, is_was5_sentinel, logical(1))
  list(
    docs = docs[!is_sentinel],
    sentinel_count = as.integer(sum(is_sentinel))
  )
}

find_record_container <- function(obj, depth = 0L) {
  if (depth > 6L || !is.list(obj)) return(NULL)

  if (!is.null(obj$docs) && is.list(obj$docs)) {
    cleaned <- strip_was5_sentinel(obj$docs)
    docs <- cleaned$docs
    if (length(docs) == 0 || looks_like_record(docs[[1]])) {
      return(list(
        docs = docs,
        count = suppressWarnings(as.integer(obj$count %||% length(docs))),
        pagenum = suppressWarnings(as.integer(obj$pagenum %||% 1L)),
        sentinel_count = cleaned$sentinel_count
      ))
    }
  }

  if (length(obj) > 0 && is.null(names(obj))) {
    cleaned <- strip_was5_sentinel(obj)
    docs <- cleaned$docs
    if (length(docs) == 0 || looks_like_record(docs[[1]])) {
      return(list(
        docs = docs,
        count = length(docs),
        pagenum = 1L,
        sentinel_count = cleaned$sentinel_count
      ))
    }
  }

  nms <- names(obj)
  if (is.null(nms)) return(NULL)
  for (nm in nms) {
    child <- obj[[nm]]
    if (is.list(child)) {
      hit <- find_record_container(child, depth + 1L)
      if (!is.null(hit)) return(hit)
    }
  }
  NULL
}

strip_utf8_bom <- function(text) {
  if (is.null(text) || length(text) == 0L) return(text)

  # Avoid a PCRE \x{FEFF} pattern: some Windows R/PCRE builds reject it.
  # Browser responseText normally exposes the UTF-8 BOM as the Unicode
  # character U+FEFF, so remove it directly without a regular expression.
  bom <- intToUtf8(65279L)
  has_bom <- !is.na(text) & startsWith(text, bom)
  if (any(has_bom)) {
    text[has_bom] <- substring(text[has_bom], 2L)
  }
  text
}

# WAS5 sometimes emits literal control characters inside quoted JSON strings.
# This first appears prominently in the 2013 archive, e.g. station/section labels
# containing a raw line break. Strict JSON parsers reject those responses even
# though browsers display them. Repair only control characters that occur INSIDE
# JSON strings; whitespace outside strings is left untouched. The raw HTTP text is
# never overwritten or altered on disk.
repair_json_string_controls <- function(text) {
  if (is.null(text) || length(text) == 0L || !nzchar(text)) {
    return(list(text = text, repaired = 0L))
  }

  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  out <- character(length(chars))
  in_string <- FALSE
  escaped <- FALSE
  repaired <- 0L

  for (i in seq_along(chars)) {
    ch <- chars[[i]]

    if (in_string) {
      if (escaped) {
        out[[i]] <- ch
        escaped <- FALSE
      } else if (identical(ch, "\\")) {
        out[[i]] <- ch
        escaped <- TRUE
      } else if (identical(ch, "\"")) {
        out[[i]] <- ch
        in_string <- FALSE
      } else if (identical(ch, "\n")) {
        out[[i]] <- "\\n"
        repaired <- repaired + 1L
      } else if (identical(ch, "\r")) {
        out[[i]] <- "\\r"
        repaired <- repaired + 1L
      } else if (identical(ch, "\t")) {
        out[[i]] <- "\\t"
        repaired <- repaired + 1L
      } else {
        out[[i]] <- ch
      }
    } else {
      out[[i]] <- ch
      if (identical(ch, "\"")) in_string <- TRUE
    }
  }

  list(text = paste0(out, collapse = ""), repaired = repaired)
}

parse_data_response <- function(text) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  text <- strip_utf8_bom(text)
  text <- trimws(text)

  # Tolerate a simple JSONP wrapper if the site changes transport style.
  if (grepl("^[A-Za-z_$][A-Za-z0-9_$.]*\\s*\\(", text)) {
    text <- sub("^[^(]*\\(", "", text)
    text <- sub("\\)\\s*;?\\s*$", "", text)
  }

  repaired <- repair_json_string_controls(text)
  parse_text <- repaired$text

  obj <- tryCatch(
    jsonlite::fromJSON(parse_text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(obj)) return(NULL)

  # Some services return a JSON string containing another JSON document.
  if (is.character(obj) && length(obj) == 1L && grepl("^[\\[{]", trimws(obj))) {
    inner_repaired <- repair_json_string_controls(obj)
    obj <- tryCatch(
      jsonlite::fromJSON(inner_repaired$text, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(obj)) return(NULL)
    repaired$repaired <- repaired$repaired + inner_repaired$repaired
  }

  hit <- find_record_container(obj)
  if (!is.null(hit)) hit$json_control_repairs <- as.integer(repaired$repaired)
  hit
}

candidate_entries <- function(logs, after_index = 0L) {
  out <- list()
  if (length(logs) == 0) return(out)

  for (i in seq_along(logs)) {
    if (i <= after_index) next
    e <- logs[[i]]
    if (is.null(e$status) || as.integer(e$status) < 200L || as.integer(e$status) >= 400L) next
    info <- parse_data_response(e$response_text %||% "")
    if (!is.null(info)) {
      out[[length(out) + 1L]] <- list(index = i, entry = e, info = info)
    }
  }
  out
}

wait_for_candidate_after <- function(session, after_index = 0L, timeout_sec = WAIT_TIMEOUT_SEC) {
  start <- Sys.time()
  repeat {
    logs <- read_netlog(session)
    hits <- candidate_entries(logs, after_index = after_index)
    if (length(hits) > 0) return(hits[[length(hits)]])

    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout_sec) {
      return(NULL)
    }
    Sys.sleep(0.35)
  }
}

# ---- Request parameter helpers --------------------------------------------
parse_form_pairs <- function(x) {
  if (is.null(x) || !nzchar(x)) return(data.frame(key = character(), value = character()))
  parts <- strsplit(x, "&", fixed = TRUE)[[1]]
  rows <- lapply(parts, function(p) {
    z <- strsplit(p, "=", fixed = TRUE)[[1]]
    key_raw <- z[[1]] %||% ""
    val_raw <- if (length(z) >= 2) paste(z[-1], collapse = "=") else ""
    data.frame(
      key = URLdecode(gsub("+", " ", key_raw, fixed = TRUE)),
      value = URLdecode(gsub("+", " ", val_raw, fixed = TRUE)),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

url_query_text <- function(url) {
  if (!grepl("?", url, fixed = TRUE)) return("")
  sub("^[^?]*\\?", "", url)
}

url_without_query <- function(url) sub("\\?.*$", "", url)

extract_request_params <- function(entry) {
  url <- entry$url %||% ""
  body <- entry$request_body %||% ""
  method <- toupper(entry$method %||% "GET")

  q <- parse_form_pairs(url_query_text(url))
  b <- data.frame(key = character(), value = character())
  body_kind <- "none"

  if (nzchar(body)) {
    if (grepl("^\\s*\\{", body)) {
      jo <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE), error = function(e) NULL)
      if (is.list(jo) && !is.null(names(jo))) {
        b <- data.frame(
          key = names(jo),
          value = vapply(jo, function(v) paste(as.character(v), collapse = ","), character(1)),
          stringsAsFactors = FALSE
        )
        body_kind <- "json"
      }
    } else {
      b <- parse_form_pairs(body)
      body_kind <- "form"
    }
  }

  list(method = method, query = q, body = b, body_kind = body_kind)
}

identify_page_parameter <- function(first_entry, second_entry = NULL) {
  p1 <- extract_request_params(first_entry)
  p2 <- if (!is.null(second_entry)) extract_request_params(second_entry) else NULL

  priority <- tolower(c(
    "pageindex", "page_index", "page", "pageno", "page_no",
    "pagenum", "page_num", "currpage", "currentpage", "current_page"
  ))

  inspect_location <- function(a, b = NULL, location) {
    if (nrow(a) == 0) return(NULL)
    keys_lower <- tolower(a$key)

    if (!is.null(b) && nrow(b) > 0) {
      for (i in seq_len(nrow(a))) {
        j <- which(tolower(b$key) == keys_lower[[i]])
        if (length(j) == 0) next
        v1 <- a$value[[i]]
        v2 <- b$value[[j[[1]]]]
        if (!identical(v1, v2) && suppressWarnings(as.integer(v2)) == 2L) {
          return(list(location = location, name = a$key[[i]]))
        }
      }
    }

    for (nm in priority) {
      i <- which(keys_lower == nm)
      if (length(i) > 0) return(list(location = location, name = a$key[[i[[1]]]]))
    }
    NULL
  }

  hit <- inspect_location(p1$body, if (!is.null(p2)) p2$body else NULL, "body")
  if (!is.null(hit)) return(hit)
  hit <- inspect_location(p1$query, if (!is.null(p2)) p2$query else NULL, "query")
  if (!is.null(hit)) return(hit)

  # Sometimes page 1 omits the page parameter and page 2 introduces it.
  if (!is.null(p2)) {
    hit <- inspect_location(p2$body, NULL, "body")
    if (!is.null(hit)) return(hit)
    hit <- inspect_location(p2$query, NULL, "query")
    if (!is.null(hit)) return(hit)
  }

  NULL
}

url_encode_value <- function(x) utils::URLencode(as.character(x), reserved = TRUE)

set_form_parameter <- function(text, name, value) {
  parts <- if (nzchar(text)) strsplit(text, "&", fixed = TRUE)[[1]] else character()
  found <- FALSE
  if (length(parts) > 0) {
    for (i in seq_along(parts)) {
      z <- strsplit(parts[[i]], "=", fixed = TRUE)[[1]]
      key_raw <- z[[1]] %||% ""
      key_dec <- URLdecode(gsub("+", " ", key_raw, fixed = TRUE))
      if (tolower(key_dec) == tolower(name)) {
        parts[[i]] <- paste0(key_raw, "=", url_encode_value(value))
        found <- TRUE
        break
      }
    }
  }
  if (!found) {
    parts <- c(parts, paste0(url_encode_value(name), "=", url_encode_value(value)))
  }
  paste(parts, collapse = "&")
}

set_json_parameter <- function(text, name, value) {
  obj <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  nms <- names(obj)
  hit <- which(tolower(nms) == tolower(name))
  if (length(hit) > 0) {
    obj[[hit[[1]]]] <- value
  } else {
    obj[[name]] <- value
  }
  jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", na = "null")
}

set_page_in_entry <- function(entry, page_spec, page_number) {
  out <- entry
  params <- extract_request_params(entry)

  if (page_spec$location == "body") {
    body <- entry$request_body %||% ""
    if (params$body_kind == "json") {
      out$request_body <- set_json_parameter(body, page_spec$name, as.integer(page_number))
    } else {
      out$request_body <- set_form_parameter(body, page_spec$name, as.integer(page_number))
    }
  } else {
    base <- url_without_query(entry$url %||% "")
    q <- set_form_parameter(url_query_text(entry$url %||% ""), page_spec$name, as.integer(page_number))
    out$url <- paste0(base, "?", q)
  }
  out
}


set_parameter_in_entry <- function(entry, name, value) {
  out <- entry
  params <- extract_request_params(entry)

  body_idx <- which(tolower(params$body$key) == tolower(name))
  query_idx <- which(tolower(params$query$key) == tolower(name))

  if (length(body_idx) > 0L) {
    body <- entry$request_body %||% ""
    if (params$body_kind == "json") {
      out$request_body <- set_json_parameter(body, name, value)
    } else {
      out$request_body <- set_form_parameter(body, name, value)
    }
    return(out)
  }

  if (length(query_idx) > 0L) {
    base <- url_without_query(entry$url %||% "")
    q <- set_form_parameter(url_query_text(entry$url %||% ""), name, value)
    out$url <- paste0(base, "?", q)
    return(out)
  }

  NULL
}

get_parameter_from_entry <- function(entry, name) {
  params <- extract_request_params(entry)
  i <- which(tolower(params$body$key) == tolower(name))
  if (length(i) > 0L) return(params$body$value[[i[[1]]]])
  i <- which(tolower(params$query$key) == tolower(name))
  if (length(i) > 0L) return(params$query$value[[i[[1]]]])
  NULL
}

# ---- Browser-side request replay ------------------------------------------
safe_fetch_headers <- function(headers) {
  if (is.null(headers) || length(headers) == 0) return(list())
  if (is.null(names(headers))) return(list())

  allowed <- c("content-type", "x-requested-with", "accept")
  keep <- tolower(names(headers)) %in% allowed
  headers[keep]
}

browser_fetch_entry <- function(session, entry) {
  method <- toupper(entry$method %||% "GET")
  url <- entry$url %||% stop("Captured request has no URL", call. = FALSE)
  body <- entry$request_body %||% ""
  headers <- safe_fetch_headers(entry$request_headers %||% list())

  url_js <- jsonlite::toJSON(url, auto_unbox = TRUE)
  method_js <- jsonlite::toJSON(method, auto_unbox = TRUE)
  headers_js <- jsonlite::toJSON(headers, auto_unbox = TRUE, null = "null")

  body_part <- ""
  if (!method %in% c("GET", "HEAD")) {
    body_js <- jsonlite::toJSON(body, auto_unbox = TRUE)
    body_part <- paste0(",body:", body_js)
  }

  expr <- paste0(
    "(async()=>{",
    "const r=await fetch(", url_js, ",{method:", method_js,
    ",headers:", headers_js, body_part,
    ",credentials:'include',cache:'no-store'});",
    "const t=await r.text();",
    "return JSON.stringify({status:r.status,url:r.url,text:t});",
    "})()"
  )

  txt <- eval_js(session, expr)
  ans <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  if (as.integer(ans$status %||% 0L) < 200L || as.integer(ans$status %||% 0L) >= 400L) {
    stop(
      "Backend replay returned HTTP ", ans$status %||% NA_integer_,
      " for ", ans$url %||% url,
      call. = FALSE
    )
  }
  ans$text %||% ""
}

records_to_tibble <- function(records, source_page) {
  if (is.null(records) || length(records) == 0) return(tibble::tibble())

  cols <- unique(unlist(lapply(records, names), use.names = FALSE))
  values <- lapply(cols, function(nm) {
    vapply(records, function(rec) {
      x <- rec[[nm]]
      if (is.null(x) || length(x) == 0) return(NA_character_)
      if (length(x) > 1) return(paste(as.character(x), collapse = "|"))
      as.character(x)
    }, character(1))
  })
  names(values) <- cols
  out <- tibble::as_tibble(values)
  out$source_page <- as.integer(source_page)
  out
}

sanitize_file_component <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", as.character(x))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- "dataset"
  x
}

write_unexpected_response <- function(text, label, page, attempt, error_message = NULL) {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = COLLECTOR_TZ)
  fn <- file.path(
    DIAGNOSTIC_DIR,
    sprintf(
      "unexpected_%s_page%04d_attempt%02d_%s.txt",
      sanitize_file_component(label), as.integer(page), as.integer(attempt), stamp
    )
  )
  payload <- c(
    paste0("label: ", label),
    paste0("page: ", page),
    paste0("attempt: ", attempt),
    paste0("captured_at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    if (!is.null(error_message)) paste0("error: ", error_message) else NULL,
    "--- response/body preview ---",
    if (is.null(text) || !nzchar(text)) "<empty>" else substr(text, 1L, 200000L)
  )
  writeLines(payload, fn, useBytes = TRUE)
  fn
}

fetch_page_with_retry <- function(session, request_entry, page_number, label) {
  last_error <- NULL
  last_text <- ""

  for (attempt in seq_len(MAX_PAGE_RETRIES)) {
    result <- tryCatch({
      txt <- browser_fetch_entry(session, request_entry)
      last_text <- txt
      info <- parse_data_response(txt)
      if (!is.null(info)) {
        return(list(ok = TRUE, info = info, text = txt, attempt = attempt))
      }
      last_error <- "HTTP response was received but it was not the expected weekly-water JSON."
      NULL
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })

    if (!is.null(result) && isTRUE(result$ok)) return(result)

    diag_file <- write_unexpected_response(
      last_text,
      label = label,
      page = page_number,
      attempt = attempt,
      error_message = last_error
    )

    if (attempt < MAX_PAGE_RETRIES) {
      delay <- min(30, 1.5 * (2 ^ (attempt - 1))) + stats::runif(1, 0, 0.75)
      log_msg(
        label, ": page ", page_number, " attempt ", attempt, "/", MAX_PAGE_RETRIES,
        " failed (", last_error %||% "unexpected response", "). Retrying after ",
        sprintf("%.1f", delay), " s; diagnostic: ", basename(diag_file)
      )
      Sys.sleep(delay)
    } else {
      stop(
        label, ": page ", page_number, " failed after ", MAX_PAGE_RETRIES,
        " attempts. Progress has been checkpointed. Last diagnostic: ", diag_file,
        call. = FALSE
      )
    }
  }
}

fetch_all_pages <- function(session, first_hit, page_spec, label = "dataset", checkpoint_key = NULL) {
  total_pages <- suppressWarnings(as.integer(first_hit$info$pagenum %||% 1L))
  total_count <- suppressWarnings(as.integer(first_hit$info$count %||% NA_integer_))
  if (is.na(total_pages) || total_pages < 1L) total_pages <- 1L

  log_msg(
    label, ": backend reports ",
    ifelse(is.na(total_count), "unknown", total_count),
    " records across ", total_pages, " page(s)"
  )

  if (total_pages > 1L && is.null(page_spec)) {
    stop(
      "The backend has multiple pages, but the page-number parameter could not be identified. ",
      "A discovery file has been saved; share it so the request can be mapped exactly.",
      call. = FALSE
    )
  }

  checkpoint_path <- NULL
  manifest_file <- NULL
  if (!is.null(checkpoint_key) && nzchar(checkpoint_key)) {
    checkpoint_path <- file.path(CHECKPOINT_DIR, sanitize_file_component(checkpoint_key))
    dir.create(checkpoint_path, recursive = TRUE, showWarnings = FALSE)
    manifest_file <- file.path(checkpoint_path, "manifest.rds")

    old_manifest <- if (file.exists(manifest_file)) {
      tryCatch(readRDS(manifest_file), error = function(e) NULL)
    } else NULL

    compatible <- !is.null(old_manifest) &&
      identical(as.integer(old_manifest$total_pages), as.integer(total_pages)) &&
      (is.na(total_count) || is.na(old_manifest$total_count) ||
         identical(as.integer(old_manifest$total_count), as.integer(total_count)))

    if (!is.null(old_manifest) && !compatible) {
      log_msg(label, ": source pagination changed; clearing stale checkpoint")
      unlink(checkpoint_path, recursive = TRUE, force = TRUE)
      dir.create(checkpoint_path, recursive = TRUE, showWarnings = FALSE)
    }

    saveRDS(
      list(
        label = label,
        total_pages = as.integer(total_pages),
        total_count = as.integer(total_count),
        updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z", tz = COLLECTOR_TZ)
      ),
      manifest_file,
      version = 3
    )
  }

  page_file <- function(p) {
    if (is.null(checkpoint_path)) return(NULL)
    file.path(checkpoint_path, sprintf("page_%04d.rds", as.integer(p)))
  }

  pages <- vector("list", total_pages)

  # Always trust the newly captured page 1 because it validates the live query.
  pages[[1]] <- records_to_tibble(first_hit$info$docs, 1L)
  pf1 <- page_file(1L)
  if (!is.null(pf1)) saveRDS(pages[[1]], pf1, compress = "xz", version = 3)

  resumed_pages <- 0L
  if (total_pages > 1L) {
    for (p in 2:total_pages) {
      pf <- page_file(p)
      if (!is.null(pf) && file.exists(pf)) {
        cached <- tryCatch(readRDS(pf), error = function(e) NULL)
        if (inherits(cached, "data.frame")) {
          pages[[p]] <- cached
          resumed_pages <- resumed_pages + 1L
          next
        }
      }

      req <- set_page_in_entry(first_hit$entry, page_spec, p)
      got <- fetch_page_with_retry(session, req, p, label)
      pages[[p]] <- records_to_tibble(got$info$docs, p)
      if (!is.null(pf)) saveRDS(pages[[p]], pf, compress = "xz", version = 3)

      if (p == total_pages || p %% 25L == 0L) {
        log_msg(label, ": captured page ", p, "/", total_pages)
      }
      Sys.sleep(PAGE_DELAY_SEC + stats::runif(1, 0, 0.15))
    }
  }

  if (resumed_pages > 0L) {
    log_msg(label, ": resumed ", resumed_pages, " previously checkpointed page(s)")
  }

  dat <- dplyr::bind_rows(pages)

  # WAS5 can return overlapping records at page boundaries.  We reconcile in
  # two conservative stages:
  #   1) exact equality across all backend fields (except our source_page), then
  #   2) equality across the fields actually published by the Fujian weekly table.
  # Stage 2 matters because WAS5 may attach internal metadata that can differ even
  # when the public/scientific record is the same.  Automatic removal occurs ONLY
  # when one of these reconciliations matches the backend-reported count exactly.
  if (!is.na(total_count) && nrow(dat) != total_count) {
    source_cols <- setdiff(names(dat), "source_page")
    exact_dup_mask <- if (length(source_cols) > 0L) {
      duplicated(as.data.frame(dat[, source_cols, drop = FALSE]))
    } else {
      rep(FALSE, nrow(dat))
    }
    exact_duplicate_rows <- sum(exact_dup_mask)
    exact_dedup_n <- nrow(dat) - exact_duplicate_rows

    published_fields <- intersect(
      c(
        "s1", "s2", "s3", "s4", "s5",
        "f1", "f2", "f3", "f4", "f5", "f6",
        "s8", "s9", "s10"
      ),
      names(dat)
    )
    published_dup_mask <- if (length(published_fields) > 0L) {
      duplicated(as.data.frame(dat[, published_fields, drop = FALSE]))
    } else {
      rep(FALSE, nrow(dat))
    }
    published_duplicate_rows <- sum(published_dup_mask)
    published_dedup_n <- nrow(dat) - published_duplicate_rows

    reconciled <- FALSE
    if (nrow(dat) > total_count && exact_duplicate_rows > 0L && exact_dedup_n == total_count) {
      log_msg(
        label, ": WAS5 pagination returned ", exact_duplicate_rows,
        " exact cross-page duplicate row(s); removed after validation against backend count ",
        total_count
      )
      dat <- dat[!exact_dup_mask, , drop = FALSE]
      reconciled <- TRUE
    } else if (
      nrow(dat) > total_count &&
      length(published_fields) == 14L &&
      published_duplicate_rows > 0L &&
      published_dedup_n == total_count
    ) {
      nonexact_published_dups <- sum(published_dup_mask & !exact_dup_mask)
      log_msg(
        label, ": WAS5 pagination returned ", published_duplicate_rows,
        " duplicate public record(s) across page boundaries; ",
        exact_duplicate_rows, " were byte-equivalent across backend fields and ",
        nonexact_published_dups, " differed only in non-published backend metadata. ",
        "Removed only after the 14 published fields reconciled exactly to backend count ",
        total_count
      )
      dat <- dat[!published_dup_mask, , drop = FALSE]
      reconciled <- TRUE
    }

    if (!reconciled) {
      diag_file <- file.path(
        DIAGNOSTIC_DIR,
        paste0(
          "row_count_mismatch_", sanitize_file_component(label), "_",
          format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"
        )
      )

      page_counts <- if ("source_page" %in% names(dat)) {
        as.data.frame(table(dat$source_page), stringsAsFactors = FALSE)
      } else {
        data.frame()
      }
      if (nrow(page_counts) > 0L) names(page_counts) <- c("source_page", "captured_rows")

      exact_examples <- if (exact_duplicate_rows > 0L) {
        utils::head(dat[exact_dup_mask, , drop = FALSE], 25L)
      } else NULL
      published_examples <- if (published_duplicate_rows > 0L) {
        utils::head(dat[published_dup_mask, , drop = FALSE], 25L)
      } else NULL

      saveRDS(
        list(
          label = label,
          backend_count = total_count,
          backend_pages = total_pages,
          captured_rows = nrow(dat),
          exact_duplicate_rows_ignoring_source_page = exact_duplicate_rows,
          rows_after_exact_dedup = exact_dedup_n,
          published_fields_used = published_fields,
          duplicate_public_records = published_duplicate_rows,
          rows_after_public_record_dedup = published_dedup_n,
          page_counts = page_counts,
          exact_duplicate_examples = exact_examples,
          public_duplicate_examples = published_examples,
          note = paste0(
            "No automatic row removal was performed. Neither exact-backend-field deduplication nor ",
            "deduplication on the 14 fields published by the Fujian weekly table reconciled captured ",
            "rows exactly with the backend-reported count."
          )
        ),
        diag_file,
        version = 3
      )

      stop(
        label, " row-count validation failed: backend reports ", total_count,
        " records but ", nrow(dat), " were captured; exact duplicate rows = ",
        exact_duplicate_rows, " (", exact_dedup_n, " after exact dedup); duplicate public records = ",
        published_duplicate_rows, " (", published_dedup_n, " after public-record dedup). ",
        "Checkpoint retained. Diagnostic saved: ", diag_file,
        call. = FALSE
      )
    }
  }

  if (!is.null(checkpoint_path) && dir.exists(checkpoint_path)) {
    unlink(checkpoint_path, recursive = TRUE, force = TRUE)
  }
  dat
}

# ---- UI actions used only to capture a filtered request --------------------
click_next_page <- function(session) {
  js <- paste0(
    "(()=>{",
    "const a=[...document.querySelectorAll('.jg_ym a')].find(x=>x.textContent.trim()==='\\u4e0b\\u4e00\\u9875');",
    "if(!a) return false; a.click(); return true;",
    "})()"
  )
  isTRUE(eval_js(session, js))
}

capture_second_page <- function(session, first_hit) {
  if (as.integer(first_hit$info$pagenum %||% 1L) <= 1L) return(NULL)

  before <- netlog_count(session)
  if (!click_next_page(session)) return(NULL)
  hit <- wait_for_candidate_after(session, after_index = before, timeout_sec = 30)
  hit
}

capture_year_filter <- function(session, year) {
  before <- netlog_count(session)
  js <- sprintf(
    paste0(
      "(()=>{",
      "const ys=document.querySelector('select[ms-duplex=\"searchForm.year\"]');",
      "const ws=document.querySelector('select[ms-duplex=\"searchForm.week\"]');",
      "const b=document.querySelector('.cx_btn');",
      "if(!ys||!b) return false;",
      "ys.value='%d';",
      "ys.dispatchEvent(new Event('input',{bubbles:true}));",
      "ys.dispatchEvent(new Event('change',{bubbles:true}));",
      "if(ws){ws.value='';ws.dispatchEvent(new Event('input',{bubbles:true}));ws.dispatchEvent(new Event('change',{bubbles:true}));}",
      "b.click();return true;",
      "})()"
    ),
    year
  )

  ok <- tryCatch(isTRUE(eval_js(session, js)), error = function(e) FALSE)
  if (!ok) return(NULL)

  hit <- wait_for_candidate_after(session, after_index = before, timeout_sec = 40)
  if (is.null(hit)) return(NULL)

  docs <- hit$info$docs
  if (length(docs) > 0) {
    yrs <- unique(vapply(docs, function(x) as.character(x$s4 %||% ""), character(1)))
    yrs <- yrs[nzchar(yrs)]
    if (length(yrs) > 0 && any(yrs != as.character(year))) return(NULL)
  }
  hit
}


# ---- Native WAS5 year/week filters -----------------------------------------
# Developer Tools capture from the official page established the exact filter
# grammar used by list2.was:
#   all weeks in 2010: (dockind=10)*(s4=2010)
#   week 1 in 2010:    (dockind=10)*(s4=2010)*(s5=1)
# We reproduce that syntax exactly rather than approximating it with SQL AND.
make_native_filter_hit <- function(
    session,
    base_hit,
    page_spec,
    year,
    week = NULL,
    prepage = WEEK_PREPAGE,
    label = NULL
) {
  classsql_value <- if (is.null(week)) {
    sprintf("(dockind=10)*(s4=%d)", as.integer(year))
  } else {
    sprintf("(dockind=10)*(s4=%d)*(s5=%d)", as.integer(year), as.integer(week))
  }

  entry <- set_parameter_in_entry(base_hit$entry, "classsql", classsql_value)
  if (is.null(entry)) {
    stop("The discovered request does not contain a classsql parameter.", call. = FALSE)
  }

  if (!is.null(page_spec)) {
    entry <- set_page_in_entry(entry, page_spec, 1L)
  } else {
    tmp <- set_parameter_in_entry(entry, "page", 1L)
    if (!is.null(tmp)) entry <- tmp
  }

  tmp <- set_parameter_in_entry(entry, "prepage", as.integer(prepage))
  if (!is.null(tmp)) entry <- tmp

  # The site's own request carries a random r parameter. Refresh it when present,
  # but do not require it because it is only a cache-buster.
  tmp <- set_parameter_in_entry(entry, "r", sprintf("%.17f", stats::runif(1)))
  if (!is.null(tmp)) entry <- tmp

  if (is.null(label)) {
    label <- if (is.null(week)) paste0("Year ", year) else paste0("Year ", year, " week ", week)
  }

  got <- fetch_page_with_retry(session, entry, 1L, label)
  info <- got$info
  docs <- info$docs %||% list()

  if (length(docs) > 0L) {
    yrs <- unique(vapply(docs, function(x) as.character(x$s4 %||% ""), character(1)))
    yrs <- yrs[nzchar(yrs)]
    if (length(yrs) > 0L && any(yrs != as.character(year))) {
      stop(
        label, ": native year-filter validation failed; response contains year(s): ",
        paste(yrs, collapse = ", "),
        call. = FALSE
      )
    }

    if (!is.null(week)) {
      wks <- unique(vapply(docs, function(x) as.character(x$s5 %||% ""), character(1)))
      wks <- wks[nzchar(wks)]
      if (length(wks) > 0L && any(wks != as.character(week))) {
        stop(
          label, ": native week-filter validation failed; response contains week(s): ",
          paste(wks, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  list(
    entry = entry,
    info = info,
    classsql = classsql_value,
    prepage = as.integer(prepage)
  )
}

fetch_week_partition <- function(session, base_hit, page_spec, year, week) {
  label <- paste0("Year ", year, " week ", week)

  hit <- make_native_filter_hit(
    session = session,
    base_hit = base_hit,
    page_spec = page_spec,
    year = year,
    week = week,
    prepage = WEEK_PREPAGE,
    label = label
  )

  total_count <- suppressWarnings(as.integer(hit$info$count %||% length(hit$info$docs %||% list())))
  total_pages <- suppressWarnings(as.integer(hit$info$pagenum %||% 1L))
  docs_n <- length(hit$info$docs %||% list())

  if (is.na(total_count)) total_count <- docs_n
  if (is.na(total_pages) || total_pages < 1L) total_pages <- 1L

  # If the site's normal prepage is too small for a particular week, ask WAS5 for
  # enough rows to keep that week in ONE response. We deliberately do not paginate
  # a partition whose sort key is non-unique.
  if (total_pages > 1L || docs_n != total_count) {
    needed <- min(
      MAX_SINGLE_RESPONSE_PREPAGE,
      max(WEEK_PREPAGE, total_count + 10L)
    )

    if (needed > hit$prepage) {
      hit <- make_native_filter_hit(
        session = session,
        base_hit = base_hit,
        page_spec = page_spec,
        year = year,
        week = week,
        prepage = needed,
        label = paste0(label, " expanded-prepage")
      )
      total_count <- suppressWarnings(as.integer(hit$info$count %||% length(hit$info$docs %||% list())))
      total_pages <- suppressWarnings(as.integer(hit$info$pagenum %||% 1L))
      docs_n <- length(hit$info$docs %||% list())
      if (is.na(total_count)) total_count <- docs_n
      if (is.na(total_pages) || total_pages < 1L) total_pages <- 1L
    }
  }

  if (total_pages != 1L || docs_n != total_count) {
    diag_file <- file.path(
      DIAGNOSTIC_DIR,
      sprintf(
        "week_partition_not_single_page_%d_%02d_%s.rds",
        as.integer(year), as.integer(week), format(Sys.time(), "%Y%m%d_%H%M%S")
      )
    )
    saveRDS(
      list(
        year = as.integer(year),
        week = as.integer(week),
        classsql = hit$classsql,
        prepage = hit$prepage,
        backend_count = total_count,
        backend_pages = total_pages,
        docs_returned = docs_n,
        request = hit$entry,
        note = paste0(
          "The native year-week partition did not fit in one response. No pagination or ",
          "deduplication was attempted because sortfield=-s4 is not a stable unique order ",
          "inside a fixed year/week partition."
        )
      ),
      diag_file,
      version = 3
    )
    stop(
      label, ": backend reports ", total_count, " record(s) across ", total_pages,
      " page(s), but only ", docs_n, " document(s) were returned even with prepage=",
      hit$prepage, ". Diagnostic saved: ", diag_file,
      call. = FALSE
    )
  }

  dat <- records_to_tibble(hit$info$docs %||% list(), 1L)
  list(
    data = dat,
    count = as.integer(total_count),
    classsql = hit$classsql,
    prepage = hit$prepage
  )
}

collect_year_by_week <- function(session, base_hit, page_spec, year, use_checkpoint = TRUE) {
  label <- paste0("Year ", year)

  # First request gets the native annual count. If the year can fit in one response,
  # V11 expands prepage and uses that single response; otherwise it falls back to weeks.
  annual_hit <- make_native_filter_hit(
    session = session,
    base_hit = base_hit,
    page_spec = page_spec,
    year = year,
    week = NULL,
    prepage = WEEK_PREPAGE,
    label = paste0(label, " annual-QA")
  )
  annual_count <- suppressWarnings(as.integer(annual_hit$info$count %||% NA_integer_))
  annual_pages <- suppressWarnings(as.integer(annual_hit$info$pagenum %||% 1L))
  annual_docs_n <- length(annual_hit$info$docs %||% list())
  log_msg(
    label, ": native annual filter accepted: ", annual_hit$classsql,
    "; backend count = ", ifelse(is.na(annual_count), "unknown", annual_count),
    "; cleaned docs in first response = ", annual_docs_n
  )

  # Fast path: if possible, retrieve the entire year in ONE WAS5 response.
  # This is safe because no page-boundary ordering is involved. The synthetic
  # {recid:'id'} footer has already been removed by parse_data_response().
  annual_full_hit <- annual_hit
  if (
    !is.na(annual_count) &&
    (annual_pages > 1L || annual_docs_n != annual_count) &&
    annual_count <= MAX_SINGLE_RESPONSE_PREPAGE
  ) {
    needed <- min(
      MAX_SINGLE_RESPONSE_PREPAGE,
      max(WEEK_PREPAGE, annual_count + 10L)
    )
    if (needed > annual_hit$prepage) {
      annual_full_hit <- make_native_filter_hit(
        session = session,
        base_hit = base_hit,
        page_spec = page_spec,
        year = year,
        week = NULL,
        prepage = needed,
        label = paste0(label, " annual-single-response")
      )
      annual_pages <- suppressWarnings(as.integer(annual_full_hit$info$pagenum %||% 1L))
      annual_docs_n <- length(annual_full_hit$info$docs %||% list())
    }
  }

  if (
    !is.na(annual_count) && annual_pages == 1L &&
    annual_docs_n == annual_count
  ) {
    dat <- records_to_tibble(annual_full_hit$info$docs %||% list(), 1L)
    validate_source_fields(dat)

    returned_years <- unique(trim_na(dat$s4))
    returned_years <- returned_years[!is.na(returned_years)]
    if (length(returned_years) > 0L && any(returned_years != as.character(year))) {
      stop(
        label, ": annual single-response data contain unexpected year(s): ",
        paste(returned_years, collapse = ", "),
        call. = FALSE
      )
    }

    log_msg(
      label, ": annual single-response QA passed; ", nrow(dat),
      " real records = backend count; weekly fallback not required"
    )
    return(dat)
  }

  log_msg(
    label, ": annual data could not be validated as one complete response; ",
    "falling back to native year x week partitions"
  )

  checkpoint_path <- file.path(CHECKPOINT_DIR, paste0("yearweek_", year))
  if (use_checkpoint) {
    dir.create(checkpoint_path, recursive = TRUE, showWarnings = FALSE)
  }

  week_parts <- vector("list", 53L)
  week_counts <- integer(53L)
  resumed <- 0L

  for (week in 1:53) {
    cp_file <- if (use_checkpoint) {
      file.path(checkpoint_path, sprintf("week_%02d.rds", week))
    } else NULL

    cached <- NULL
    if (!is.null(cp_file) && file.exists(cp_file)) {
      cached <- tryCatch(readRDS(cp_file), error = function(e) NULL)
      if (
        is.list(cached) && inherits(cached$data, "data.frame") &&
        identical(as.integer(cached$year), as.integer(year)) &&
        identical(as.integer(cached$week), as.integer(week))
      ) {
        week_parts[[week]] <- cached$data
        week_counts[[week]] <- as.integer(cached$count %||% nrow(cached$data))
        resumed <- resumed + 1L
        next
      }
    }

    part <- fetch_week_partition(session, base_hit, page_spec, year, week)
    week_parts[[week]] <- part$data
    week_counts[[week]] <- part$count

    if (!is.null(cp_file)) {
      saveRDS(
        list(
          year = as.integer(year),
          week = as.integer(week),
          count = as.integer(part$count),
          classsql = part$classsql,
          prepage = part$prepage,
          data = part$data,
          captured_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z", tz = COLLECTOR_TZ)
        ),
        cp_file,
        compress = "xz",
        version = 3
      )
    }

    if (week %% 10L == 0L || week == 53L) {
      log_msg(
        label, ": completed week ", week, "/53; cumulative backend rows = ",
        sum(week_counts[seq_len(week)])
      )
    }
    Sys.sleep(PAGE_DELAY_SEC + stats::runif(1, 0, 0.15))
  }

  if (resumed > 0L) {
    log_msg(label, ": resumed ", resumed, " previously checkpointed weekly partition(s)")
  }

  dat <- dplyr::bind_rows(week_parts)
  validate_source_fields(dat)

  if (nrow(dat) != sum(week_counts)) {
    stop(
      label, ": internal weekly assembly mismatch: ", nrow(dat),
      " rows assembled but weekly backend counts sum to ", sum(week_counts), ".",
      call. = FALSE
    )
  }

  returned_years <- unique(trim_na(dat$s4))
  returned_years <- returned_years[!is.na(returned_years)]
  if (length(returned_years) > 0L && any(returned_years != as.character(year))) {
    stop(
      label, ": assembled weekly data contain unexpected year(s): ",
      paste(returned_years, collapse = ", "),
      call. = FALSE
    )
  }

  weekly_total <- sum(week_counts)
  if (!is.na(annual_count) && weekly_total != annual_count) {
    diag_file <- file.path(
      DIAGNOSTIC_DIR,
      sprintf("annual_vs_weekly_count_%d_%s.rds", as.integer(year), format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
    saveRDS(
      list(
        year = as.integer(year),
        annual_backend_count = annual_count,
        weekly_backend_counts = stats::setNames(week_counts, sprintf("week_%02d", 1:53)),
        weekly_total = weekly_total,
        data_rows = nrow(dat),
        annual_classsql = annual_hit$classsql,
        note = paste0(
          "Each year-week partition individually matched its backend count, but the sum of ",
          "weekly counts did not match the backend's native annual count."
        )
      ),
      diag_file,
      version = 3
    )

    if (year < CURRENT_YEAR) {
      stop(
        label, ": weekly counts sum to ", weekly_total,
        " but native annual backend count is ", annual_count,
        ". Historical year should be stable; diagnostic saved: ", diag_file,
        call. = FALSE
      )
    } else {
      log_msg(
        "WARNING: ", label, ": weekly counts sum to ", weekly_total,
        " while annual count is ", annual_count,
        ". Current-year publication may have changed during collection. Diagnostic: ", diag_file
      )
    }
  } else {
    log_msg(
      label, ": weekly partition QA passed; ", weekly_total,
      " rows from 53 week queries",
      if (!is.na(annual_count)) paste0(" = annual backend count ", annual_count) else ""
    )
  }

  if (use_checkpoint && dir.exists(checkpoint_path)) {
    unlink(checkpoint_path, recursive = TRUE, force = TRUE)
  }

  dat
}

# ---- Source archival -------------------------------------------------------
archive_year_snapshot <- function(year, raw_df, backend_url, source_mode) {
  hash <- stable_snapshot_md5(raw_df)
  canonical <- canonical_source_file(year)
  meta_file <- file.path(SOURCE_DIR, sprintf("fujian_weekly_%d_meta.rds", year))

  old_hash <- NA_character_
  if (file.exists(meta_file)) {
    old <- tryCatch(readRDS(meta_file), error = function(e) NULL)
    if (!is.null(old$snapshot_md5)) old_hash <- old$snapshot_md5
  }

  changed <- !file.exists(canonical) || is.na(old_hash) || !identical(old_hash, hash)
  checked_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z", tz = COLLECTOR_TZ)

  if (changed) {
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = COLLECTOR_TZ)
    year_archive_dir <- file.path(ARCHIVE_DIR, as.character(year))
    dir.create(year_archive_dir, recursive = TRUE, showWarnings = FALSE)
    archive_file <- file.path(
      year_archive_dir,
      sprintf("fujian_weekly_%d_%s.rds", year, stamp)
    )
    saveRDS(raw_df, archive_file, compress = "xz", version = 3)
    saveRDS(raw_df, canonical, compress = "xz", version = 3)
    log_msg(
      "Year ", year, ": changed; ", nrow(raw_df), " rows; md5=", hash,
      "; archived as ", basename(archive_file)
    )
  } else {
    log_msg("Year ", year, ": unchanged; ", nrow(raw_df), " rows; md5=", hash)
  }

  meta <- list(
    year = as.integer(year),
    source_page = SOURCE_URL,
    backend_url = backend_url,
    source_mode = source_mode,
    rows = as.integer(nrow(raw_df)),
    snapshot_md5 = hash,
    checked_at = checked_at,
    changed = changed,
    collector_id = COLLECTOR_ID,
    github_run_id = if (nzchar(GITHUB_RUN_ID)) GITHUB_RUN_ID else NA_character_,
    github_run_attempt = if (nzchar(GITHUB_RUN_ATTEMPT)) GITHUB_RUN_ATTEMPT else NA_character_,
    scraper_code_commit = if (nzchar(GITHUB_SHA)) GITHUB_SHA else NA_character_
  )
  saveRDS(meta, meta_file, version = 3)
  invisible(meta)
}

# ---- Processing ------------------------------------------------------------
validate_source_fields <- function(dat) {
  required <- c(
    "s1", "s2", "s3", "s4", "s5",
    "f1", "f2", "f3", "f4", "f5", "f6",
    "s8", "s9", "s10"
  )
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0) {
    stop(
      "The Fujian backend response is missing expected field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

source_col_or_na <- function(df, nm) {
  if (nm %in% names(df)) return(df[[nm]])
  rep(NA_character_, nrow(df))
}

was5_period_to_date <- function(x) {
  z <- trim_na(x)
  out <- as.Date(rep(NA_character_, length(z)))

  # Older records commonly use Excel/WAS-style serial day numbers.
  is_serial <- !is.na(z) & grepl("^[0-9]+(?:\\.[0-9]+)?$", z)
  if (any(is_serial)) {
    nums <- suppressWarnings(as.numeric(z[is_serial]))
    out[is_serial] <- as.Date(nums, origin = "1899-12-30")
  }

  # From at least 2013, the same fields can also contain DD/MM/YYYY strings,
  # sometimes mixed with serial values within the same response.
  is_dmy <- !is.na(z) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", z)
  if (any(is_dmy)) {
    out[is_dmy] <- as.Date(z[is_dmy], format = "%d/%m/%Y")
  }

  # Defensive support if the backend later emits ISO dates.
  is_iso <- !is.na(z) & grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", z)
  if (any(is_iso)) {
    out[is_iso] <- as.Date(z[is_iso], format = "%Y-%m-%d")
  }

  out
}

normalize_station_name <- function(x) {
  z <- trim_na(x)
  if (length(z) == 0L) return(z)
  full_width_space <- intToUtf8(12288L)
  z <- gsub(full_width_space, "", z, fixed = TRUE)
  z <- gsub("[[:space:]]+", "", z, perl = TRUE)
  trim_na(z)
}

normalize_source_label <- function(x) {
  z <- trim_na(x)
  if (length(z) == 0L) return(z)
  full_width_space <- intToUtf8(12288L)
  z <- gsub(full_width_space, " ", z, fixed = TRUE)
  z <- gsub("[[:space:]]+", " ", z, perl = TRUE)
  trim_na(z)
}

standardize_year_source <- function(raw_df, year, meta) {
  if (nrow(raw_df) == 0) return(tibble::tibble())

  out <- tibble::tibble(
    source_recid = trim_na(source_col_or_na(raw_df, "recid")),
    source_metadataid = trim_na(source_col_or_na(raw_df, "metadataid")),
    source_docorder = trim_na(source_col_or_na(raw_df, "docorder")),
    river_system_raw = trim_na(raw_df$s1),
    river_system = normalize_source_label(raw_df$s1),
    station_name_raw = trim_na(raw_df$s2),
    station_name = normalize_station_name(raw_df$s2),
    section_status_raw = trim_na(raw_df$s3),
    section_status = normalize_source_label(raw_df$s3),
    year = suppressWarnings(as.integer(trim_na(raw_df$s4))),
    week = suppressWarnings(as.integer(trim_na(raw_df$s5))),
    source_period_start_raw = trim_na(source_col_or_na(raw_df, "s6")),
    source_period_end_raw = trim_na(source_col_or_na(raw_df, "s7")),
    report_period_start = was5_period_to_date(source_col_or_na(raw_df, "s6")),
    report_period_end = was5_period_to_date(source_col_or_na(raw_df, "s7")),
    ph_raw = trim_na(raw_df$f1),
    dissolved_oxygen_mg_l_raw = trim_na(raw_df$f2),
    permanganate_index_mg_l_raw = trim_na(raw_df$f3),
    total_phosphorus_mg_l_raw = trim_na(raw_df$f4),
    ammonia_nitrogen_mg_l_raw = trim_na(raw_df$f5),
    total_nitrogen_mg_l_raw = trim_na(raw_df$f6),
    previous_week_water_quality = trim_na(raw_df$s8),
    current_week_water_quality = trim_na(raw_df$s9),
    main_pollution_indicator = trim_na(raw_df$s10),
    source_page_number = if ("source_page" %in% names(raw_df)) as.integer(raw_df$source_page) else NA_integer_,
    source_year_file = basename(canonical_source_file(year)),
    source_snapshot_md5 = meta$snapshot_md5,
    source_checked_at = meta$checked_at,
    collector_id = as.character(meta$collector_id %||% NA_character_),
    github_run_id = as.character(meta$github_run_id %||% NA_character_),
    github_run_attempt = as.character(meta$github_run_attempt %||% NA_character_),
    scraper_code_commit = as.character(meta$scraper_code_commit %||% NA_character_)
  )

  out$report_year_week <- ifelse(
    !is.na(out$year) & !is.na(out$week),
    sprintf("%04d-week-%02d", out$year, out$week),
    NA_character_
  )

  fallback_key <- paste(
    out$year,
    out$week,
    ifelse(is.na(out$river_system), "", out$river_system),
    ifelse(is.na(out$station_name), "", out$station_name),
    ifelse(is.na(out$source_docorder), "", out$source_docorder),
    sep = "|"
  )
  out$observation_key <- ifelse(
    !is.na(out$source_recid) & nzchar(out$source_recid),
    paste0("recid:", out$source_recid),
    fallback_key
  )
  out
}

rebuild_master <- function() {
  source_files <- list.files(
    SOURCE_DIR,
    pattern = "^fujian_weekly_[0-9]{4}\\.rds$",
    full.names = TRUE
  )
  if (length(source_files) == 0) {
    log_msg("No canonical source year files available; master not rebuilt")
    return(invisible(NULL))
  }

  years <- as.integer(sub(
    "^fujian_weekly_([0-9]{4})\\.rds$",
    "\\1",
    basename(source_files)
  ))
  ord <- order(years)
  years <- years[ord]
  source_files <- source_files[ord]

  parts <- vector("list", length(source_files))
  manifests <- vector("list", length(source_files))

  for (i in seq_along(source_files)) {
    year <- years[[i]]
    raw_df <- readRDS(source_files[[i]])
    meta_file <- file.path(SOURCE_DIR, sprintf("fujian_weekly_%d_meta.rds", year))
    meta <- if (file.exists(meta_file)) readRDS(meta_file) else list(
      snapshot_md5 = NA_character_,
      checked_at = NA_character_,
      backend_url = NA_character_,
      source_mode = NA_character_
    )

    parts[[i]] <- standardize_year_source(raw_df, year, meta)
    manifests[[i]] <- tibble::tibble(
      year = year,
      rows = nrow(raw_df),
      source_file = basename(source_files[[i]]),
      snapshot_md5 = as.character(meta$snapshot_md5 %||% NA_character_),
      checked_at = as.character(meta$checked_at %||% NA_character_),
      backend_url = as.character(meta$backend_url %||% NA_character_),
      source_mode = as.character(meta$source_mode %||% NA_character_),
      collector_id = as.character(meta$collector_id %||% NA_character_),
      github_run_id = as.character(meta$github_run_id %||% NA_character_),
      github_run_attempt = as.character(meta$github_run_attempt %||% NA_character_),
      scraper_code_commit = as.character(meta$scraper_code_commit %||% NA_character_)
    )
  }

  master <- dplyr::bind_rows(parts)
  if (nrow(master) > 0) {
    master <- dplyr::arrange(
      master,
      .data$year,
      .data$week,
      .data$river_system,
      .data$station_name
    )
  }

  exact_dups <- sum(duplicated(master))
  key_dups <- if ("observation_key" %in% names(master)) {
    sum(duplicated(master$observation_key))
  } else 0L

  rds_file <- file.path(PROCESSED_DIR, "fujian_weekly_surfacewater_master.rds")
  csv_file <- file.path(PROCESSED_DIR, "fujian_weekly_surfacewater_master.csv.gz")
  manifest_file <- file.path(PROCESSED_DIR, "fujian_weekly_surfacewater_manifest.csv")

  saveRDS(master, rds_file, compress = "xz", version = 3)
  readr::write_csv(master, csv_file, na = "")
  readr::write_csv(dplyr::bind_rows(manifests), manifest_file, na = "")

  log_msg(
    "Master rebuilt: ", nrow(master), " rows across ", length(source_files),
    " year file(s); exact duplicate rows: ", exact_dups,
    "; duplicated observation keys: ", key_dups
  )
  log_msg("RDS: ", rds_file)
  log_msg("CSV.GZ: ", csv_file)
  log_msg("Manifest: ", manifest_file)

  invisible(master)
}

# ---- Run -------------------------------------------------------------------
log_msg("START Fujian weekly surface-water archive V11")
log_msg("Source: ", SOURCE_URL)
log_msg("Mode: ", MODE)
log_msg("Collector ID: ", COLLECTOR_ID)
log_msg("Collector timezone: ", COLLECTOR_TZ)
chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
log_msg(
  "Chrome executable: ",
  if (is.null(chrome_path) || length(chrome_path) == 0L) "not found" else as.character(chrome_path[[1]])
)
if (NEED_BACKFILL) {
  log_msg("Collection plan: native year-week historical backfill, 2004-", CURRENT_YEAR)
} else {
  log_msg("Collection plan: refresh current year ", CURRENT_YEAR)
}

session <- NULL
run_ok <- FALSE

tryCatch({
  session <- chromote::ChromoteSession$new()

  browser_version <- tryCatch(session$Browser$getVersion(), error = function(e) NULL)
  if (!is.null(browser_version$product)) {
    log_msg("Browser product: ", browser_version$product)
  }

  # Use the installed Chrome version's own UA, but remove the Headless token.
  try({
    bv <- session$Browser$getVersion()
    ua <- bv$userAgent %||% ""
    if (nzchar(ua)) {
      ua2 <- gsub("HeadlessChrome", "Chrome", ua, fixed = TRUE)
      session$Emulation$setUserAgentOverride(userAgent = ua2)
    }
  }, silent = TRUE)

  session$Page$addScriptToEvaluateOnNewDocument(source = LOGGER_JS)

  log_msg("Opening official Fujian weekly-report page and discovering its data request")
  session$go_to(SOURCE_URL)

  first_hit <- wait_for_candidate_after(session, after_index = 0L)
  if (is.null(first_hit)) {
    diag_file <- file.path(SOURCE_DIR, "fujian_weekly_discovery_failed.rds")
    diag <- list(
      source_url = SOURCE_URL,
      title = tryCatch(eval_js(session, "document.title"), error = function(e) NA_character_),
      location = tryCatch(eval_js(session, "location.href"), error = function(e) NA_character_),
      ready_state = tryCatch(eval_js(session, "document.readyState"), error = function(e) NA_character_),
      netlog = tryCatch(read_netlog(session), error = function(e) list()),
      html_text = tryCatch(eval_js(session, "document.body ? document.body.innerText.slice(0,5000) : ''"), error = function(e) NA_character_)
    )
    saveRDS(diag, diag_file, version = 3)
    stop(
      "The page opened but no weekly-data XHR response was captured. Diagnostic saved to: ",
      diag_file,
      call. = FALSE
    )
  }

  backend_url <- first_hit$entry$url %||% ""
  log_msg("Discovered backend request: ", first_hit$entry$method %||% "", " ", backend_url)
  log_msg(
    "Initial response: ", first_hit$info$count %||% NA_integer_,
    " records across ", first_hit$info$pagenum %||% NA_integer_, " page(s)"
  )
  if (as.integer(first_hit$info$sentinel_count %||% 0L) > 0L) {
    log_msg(
      "WAS5 footer sentinel detected and excluded from records: ",
      first_hit$info$sentinel_count, " object(s) matching {recid:'id'}"
    )
  }

  # If page 1 does not make the page-number parameter obvious, use the page's
  # own Next link once and compare page-1/page-2 requests.
  second_hit <- NULL
  page_spec <- identify_page_parameter(first_hit$entry)
  if (as.integer(first_hit$info$pagenum %||% 1L) > 1L) {
    second_hit <- capture_second_page(session, first_hit)
    if (!is.null(second_hit)) {
      page_spec <- identify_page_parameter(first_hit$entry, second_hit$entry)
    }
  }

  discovery <- list(
    discovered_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z", tz = COLLECTOR_TZ),
    source_url = SOURCE_URL,
    first_request = first_hit$entry,
    first_response_count = first_hit$info$count,
    first_response_pages = first_hit$info$pagenum,
    second_request = if (!is.null(second_hit)) second_hit$entry else NULL,
    page_parameter = page_spec
  )
  discovery_file <- file.path(SOURCE_DIR, "fujian_weekly_backend_discovery.rds")
  saveRDS(discovery, discovery_file, version = 3)
  log_msg("Backend discovery saved: ", discovery_file)
  if (!is.null(page_spec)) {
    log_msg("Pagination parameter: ", page_spec$location, " / ", page_spec$name)
  }

  any_changed <- FALSE

  if (NEED_BACKFILL) {
    years_to_collect <- if (MODE == "backfill") all_years else missing_years

    log_msg(
      "Historical backfill will use the site's native year x week classsql filters. Years: ",
      paste(years_to_collect, collapse = ", ")
    )

    for (year in years_to_collect) {
      log_msg("Year ", year, ": collecting native weekly partitions s4=", year, ", s5=1..53")

      raw_year <- collect_year_by_week(
        session = session,
        base_hit = first_hit,
        page_spec = page_spec,
        year = year,
        use_checkpoint = year < CURRENT_YEAR
      )

      meta <- archive_year_snapshot(
        year = year,
        raw_df = raw_year,
        backend_url = backend_url,
        source_mode = "native-classsql-year-week-partitions"
      )
      any_changed <- any_changed || isTRUE(meta$changed)

      Sys.sleep(YEAR_PAUSE_SEC + stats::runif(1, 0, 0.75))
    }
  } else {
    # Routine refresh uses the same native weekly partitions. Because the source is
    # a weekly report, this is intentionally conservative and avoids all unstable
    # annual pagination. Current-year checkpoints are not reused so revisions to
    # previously published weeks are detected.
    raw_current <- collect_year_by_week(
      session = session,
      base_hit = first_hit,
      page_spec = page_spec,
      year = CURRENT_YEAR,
      use_checkpoint = FALSE
    )

    meta <- archive_year_snapshot(
      year = CURRENT_YEAR,
      raw_df = raw_current,
      backend_url = backend_url,
      source_mode = "native-classsql-year-week-refresh"
    )
    any_changed <- isTRUE(meta$changed)
  }

  if (any_changed || !file.exists(file.path(PROCESSED_DIR, "fujian_weekly_surfacewater_master.rds"))) {
    rebuild_master()
  } else {
    log_msg("No source changes detected; existing processed master retained")
  }

  run_ok <- TRUE
}, error = function(e) {
  log_msg("ERROR: ", conditionMessage(e))
  stop(e)
}, finally = {
  if (!is.null(session)) try(session$close(), silent = TRUE)
  if (run_ok) log_msg("END Fujian weekly surface-water archive V11")
})