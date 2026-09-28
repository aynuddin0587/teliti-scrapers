#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
})

# ==============================================================================
# 16_epmap_cnemc_coordinate_demo.R (全国全量省市自动重置版)
# ==============================================================================

PROJECT_DIR <- normalizePath(".", winslash = "/", mustWork = TRUE)

BASE_DIR <- file.path(
  PROJECT_DIR,
  "nmemc", "data", "surfacewater", "epmap_coordinate_demo"
)
RAW_DIR <- file.path(BASE_DIR, "raw")
OUT_DIR <- file.path(BASE_DIR, "processed")
QUERY_FILE <- file.path(BASE_DIR, "epmap_coordinate_queries.csv")

MASTER_FILE <- file.path(OUT_DIR, "epmap_cnemc_coordinate_demo_master.csv")
VALID_COORD_FILE <- file.path(OUT_DIR, "epmap_cnemc_coordinate_valid_coordinates.csv")
QUERY_LOG_FILE <- file.path(OUT_DIR, "epmap_cnemc_coordinate_query_log.csv")
INBOX_EXPORT <- file.path(
  PROJECT_DIR, "nmemc", "data", "surfacewater",
  "coordinate_inbox", "epmap_demo_coordinates.csv"
)

ENDPOINT <- "https://data.epmap.org/api/data_down/determine"
REFERER <- "https://data.epmap.org/product/water?tab=download"

REQUEST_DELAY_SECONDS <- 3
MAX_RETRIES <- 3L
FORCE_REFRESH <- identical(tolower(Sys.getenv("EPMAP_FORCE_REFRESH", "false")), "true")

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

msg <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\\u00A0\\u3000]", " ")
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

valid_lonlat <- function(lon, lat) {
  is.finite(lon) & is.finite(lat) &
    lon >= 70 & lon <= 140 &
    lat >= 15 & lat <= 55
}

safe_slug <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "query")
}

read_existing_csv <- function(path) {
  if (!file.exists(path)) return(tibble())
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

write_csv_utf8 <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_excel_csv(x, path, na = "")
}

# 全量 368 个省市级行政区清单
get_national_province_city_list <- function() {
  tribble(
    ~province, ~city,
    "北京市", "北京市", "天津市", "天津市", "上海市", "上海市", "重庆市", "重庆市",
    "河北省", "石家庄市", "河北省", "唐山市", "河北省", "秦皇岛市", "河北省", "邯郸市", "河北省", "邢台市", "河北省", "保定市", "河北省", "张家口市", "河北省", "承德市", "河北省", "沧州市", "河北省", "廊坊市", "河北省", "衡水市",
    "山西省", "太原市", "山西省", "大同市", "山西省", "阳泉市", "山西省", "长治市", "山西省", "晋城市", "山西省", "朔州市", "山西省", "晋中市", "山西省", "运城市", "山西省", "忻州市", "山西省", "临汾市", "山西省", "吕梁市",
    "内蒙古自治区", "呼和浩特市", "内蒙古自治区", "包头市", "内蒙古自治区", "乌海市", "内蒙古自治区", "赤峰市", "内蒙古自治区", "通辽市", "内蒙古自治区", "鄂尔多斯市", "内蒙古自治区", "呼伦贝尔市", "内蒙古自治区", "巴彦淖尔市", "内蒙古自治区", "乌兰察布市", "内蒙古自治区", "兴安盟", "内蒙古自治区", "锡林郭勒盟", "内蒙古自治区", "阿拉善盟",
    "辽宁省", "沈阳市", "辽宁省", "大连市", "辽宁省", "鞍山市", "辽宁省", "抚顺市", "辽宁省", "本溪市", "辽宁省", "丹东市", "辽宁省", "锦州市", "辽宁省", "营口市", "辽宁省", "阜新市", "辽宁省", "辽阳市", "辽宁省", "盘锦市", "辽宁省", "铁岭市", "辽宁省", "朝阳市", "辽宁省", "葫芦岛市",
    "吉林省", "长春市", "吉林省", "吉林市", "吉林省", "四平市", "吉林省", "辽源市", "吉林省", "通化市", "吉林省", "白山市", "吉林省", "松原市", "吉林省", "白城市", "吉林省", "延边朝鲜族自治州",
    "黑龙江省", "哈尔滨市", "黑龙江省", "齐齐哈尔市", "黑龙江省", "鸡西市", "黑龙江省", "鹤岗市", "黑龙江省", "双鸭山市", "黑龙江省", "大庆市", "黑龙江省", "伊春市", "黑龙江省", "佳木斯市", "黑龙江省", "七台河市", "黑龙江省", "牡丹江市", "黑龙江省", "黑河市", "黑龙江省", "绥化市", "黑龙江省", "大兴安岭地区",
    "江苏省", "南京市", "江苏省", "无锡市", "江苏省", "徐州市", "江苏省", "常州市", "江苏省", "苏州市", "江苏省", "南通市", "江苏省", "连云港市", "江苏省", "淮安市", "江苏省", "盐城市", "江苏省", "扬州市", "江苏省", "镇江市", "江苏省", "泰州市", "江苏省", "宿迁市",
    "浙江省", "杭州市", "浙江省", "宁波市", "浙江省", "温州市", "浙江省", "嘉兴市", "浙江省", "湖州市", "浙江省", "绍兴市", "浙江省", "金华市", "浙江省", "衢州市", "浙江省", "舟山市", "浙江省", "台州市", "浙江省", "丽水市",
    "安徽省", "合肥市", "安徽省", "芜湖市", "安徽省", "蚌埠市", "安徽省", "淮南市", "安徽省", "马鞍山市", "安徽省", "淮北市", "安徽省", "铜陵市", "安徽省", "安庆市", "安徽省", "黄山市", "安徽省", "滁州市", "安徽省", "阜阳市", "安徽省", "宿州市", "安徽省", "六安市", "安徽省", "亳州市", "安徽省", "池州市", "安徽省", "宣城市",
    "福建省", "福州市", "福建省", "厦门市", "福建省", "莆田市", "福建省", "三明市", "福建省", "泉州市", "福建省", "漳州市", "福建省", "南平市", "福建省", "龙岩市", "福建省", "宁德市",
    "江西省", "南昌市", "江西省", "景德镇市", "江西省", "萍乡市", "江西省", "九江市", "江西省", "新余市", "江西省", "鹰潭市", "江西省", "赣州市", "江西省", "吉安市", "江西省", "宜春市", "江西省", "抚州市", "江西省", "上饶市",
    "山东省", "济南市", "山东省", "青岛市", "山东省", "淄博市", "山东省", "枣庄市", "山东省", "东营市", "山东省", "烟台市", "山东省", "潍坊市", "山东省", "济宁市", "山东省", "泰安市", "山东省", "威海市", "山东省", "日照市", "山东省", "临沂市", "山东省", "德州市", "山东省", "聊城市", "山东省", "滨州市", "山东省", "菏泽市",
    "河南省", "郑州市", "河南省", "开封市", "河南省", "洛阳市", "河南省", "平顶山市", "河南省", "安阳市", "河南省", "鹤壁市", "河南省", "新乡市", "河南省", "焦作市", "河南省", "濮阳市", "河南省", "许昌市", "河南省", "漯河市", "河南省", "三门峡市", "河南省", "南阳市", "河南省", "商丘市", "河南省", "信阳市", "河南省", "周口市", "河南省", "驻马店市",
    "湖北省", "武汉市", "湖北省", "黄石市", "湖北省", "十堰市", "湖北省", "宜昌市", "湖北省", "襄阳市", "湖北省", "鄂州市", "湖北省", "荆门市", "湖北省", "孝感市", "湖北省", "荆州市", "湖北省", "黄冈市", "湖北省", "咸宁市", "湖北省", "随州市", "湖北省", "恩施土家族苗族自治州",
    "湖南省", "长沙市", "湖南省", "株洲市", "湖南省", "湘潭市", "湖南省", "衡阳市", "湖南省", "邵阳市", "湖南省", "岳阳市", "湖南省", "常德市", "湖南省", "张家界市", "湖南省", "益阳市", "湖南省", "郴州市", "湖南省", "永州市", "湖南省", "怀化市", "湖南省", "娄底市", "湖南省", "湘西土家族苗族自治州",
    "广东省", "广州市", "广东省", "韶关市", "广东省", "深圳市", "广东省", "珠海市", "广东省", "汕头市", "广东省", "佛山市", "广东省", "江门市", "广东省", "湛江市", "广东省", "茂名市", "广东省", "肇庆市", "广东省", "惠州市", "广东省", "梅州市", "广东省", "汕尾市", "广东省", "河源市", "广东省", "阳江市", "广东省", "清远市", "广东省", "东莞市", "广东省", "中山市", "广东省", "潮州市", "广东省", "揭阳市", "广东省", "云浮市",
    "广西壮族自治区", "南宁市", "广西壮族自治区", "柳州市", "广西壮族自治区", "桂林市", "广西壮族自治区", "梧州市", "广西壮族自治区", "北海市", "广西壮族自治区", "防城港市", "广西壮族自治区", "钦州市", "广西壮族自治区", "贵港市", "广西壮族自治区", "玉林市", "广西壮族自治区", "百色市", "广西壮族自治区", "贺州市", "广西壮族自治区", "河池市", "广西壮族自治区", "来宾市", "广西壮族自治区", "崇左市",
    "海南省", "海口市", "海南省", "三亚市", "海南省", "三沙市", "海南省", "儋州市",
    "四川省", "成都市", "四川省", "自贡市", "四川省", "攀枝花市", "四川省", "泸州市", "四川省", "德阳市", "四川省", "绵阳市", "四川省", "广元市", "四川省", "遂宁市", "四川省", "内江市", "四川省", "乐山市", "四川省", "南充市", "四川省", "眉山市", "四川省", "宜宾市", "四川省", "广安市", "四川省", "达州市", "四川省", "雅安市", "四川省", "巴中市", "四川省", "资阳市", "四川省", "阿坝藏族羌族自治州", "四川省", "甘孜藏族自治州", "四川省", "凉山彝族自治州",
    "贵州省", "贵阳市", "贵州省", "六盘水市", "贵州省", "遵义市", "贵州省", "安顺市", "贵州省", "毕节市", "贵州省", "铜仁市", "贵州省", "黔西南布依族苗族自治州", "贵州省", "黔东南苗族侗族自治州", "贵州省", "黔南布依族苗族自治州",
    "云南省", "昆明市", "云南省", "曲靖市", "云南省", "玉溪市", "云南省", "保山市", "云南省", "昭通市", "云南省", "丽江市", "云南省", "普洱市", "云南省", "临沧市", "云南省", "楚雄彝族自治州", "云南省", "红河哈尼族彝族自治州", "云南省", "文山壮族苗族自治州", "云南省", "西双版纳傣族自治州", "云南省", "大理白族自治州", "云南省", "德宏傣族景颇族自治州", "云南省", "怒江傈僳族自治州", "云南省", "迪庆藏族自治州",
    "西藏自治区", "拉萨市", "西藏自治区", "日喀则市", "西藏自治区", "昌都市", "西藏自治区", "林芝市", "西藏自治区", "山南市", "西藏自治区", "那曲市", "西藏自治区", "阿里地区",
    "陕西省", "西安市", "陕西省", "铜川市", "陕西省", "宝鸡市", "陕西省", "咸阳市", "陕西省", "渭南市", "陕西省", "延安市", "陕西省", "汉中市", "陕西省", "榆林市", "陕西省", "安康市", "陕西省", "商洛市",
    "甘肃省", "兰州市", "甘肃省", "嘉峪关市", "甘肃省", "金昌市", "甘肃省", "白银市", "甘肃省", "天水市", "甘肃省", "武威市", "甘肃省", "张掖市", "甘肃省", "平凉市", "甘肃省", "酒泉市", "甘肃省", "庆阳市", "甘肃省", "定西市", "甘肃省", "陇南市", "甘肃省", "临夏回族自治州", "甘肃省", "甘南藏族自治州",
    "青海省", "西宁市", "青海省", "海东市", "青海省", "海北藏族自治州", "青海省", "黄南藏族自治州", "青海省", "海南藏族自治州", "青海省", "果洛藏族自治州", "青海省", "玉树藏族自治州", "青海省", "海西蒙古族藏族自治州",
    "宁夏回族自治区", "银川市", "宁夏回族自治区", "石嘴山市", "宁夏回族自治区", "吴忠市", "宁夏回族自治区", "固原市", "宁夏回族自治区", "中卫市",
    "新疆维吾尔自治区", "乌鲁木齐市", "新疆维吾尔自治区", "克拉玛依市", "新疆维吾尔自治区", "吐鲁番市", "新疆维吾尔自治区", "哈密市", "新疆维吾尔自治区", "昌吉回族自治州", "新疆维吾尔自治区", "博尔塔拉蒙古自治州", "新疆维吾尔自治区", "巴音郭楞蒙古自治州", "新疆维吾尔自治区", "阿克苏地区", "新疆维吾尔自治区", "克孜勒苏柯尔克孜自治州", "新疆维吾尔自治区", "喀什地区", "新疆维吾尔自治区", "和田地区", "新疆维吾尔自治区", "伊犁哈萨克自治州", "新疆维吾尔自治区", "塔城地区", "新疆维吾尔自治区", "阿勒泰地区"
  )
}

# ------------------------------------------------------------------------------
# Authentication
# ------------------------------------------------------------------------------

bearer_token <- Sys.getenv("EPMAP_BEARER_TOKEN", "")
csrf_token <- Sys.getenv("EPMAP_CSRF_TOKEN", "")
cookie_header <- Sys.getenv("EPMAP_COOKIE", "")

if (!nzchar(bearer_token)) {
  stop(
    paste0(
      "Missing EPMAP_BEARER_TOKEN.\n",
      "Set a fresh token in your local environment before running this script."
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# Query list 初始化与全量加载（修正核心判断）
# ------------------------------------------------------------------------------

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

existing_queries <- read_existing_csv(QUERY_FILE)

# 若文件不存在，或行数 <= 2（旧模板），强行用 368 个省市全量列表覆盖
if (!file.exists(QUERY_FILE) || nrow(existing_queries) <= 2) {
  msg("检测到查询列表缺失或为旧测试模板，正在生成全国 368 个省市的全量查询列表...")
  queries_to_write <- get_national_province_city_list() %>% mutate(enabled = TRUE)
  write_csv_utf8(queries_to_write, QUERY_FILE)
  queries <- queries_to_write
} else {
  queries <- existing_queries
}

required_query_cols <- c("province", "city")
missing_query_cols <- setdiff(required_query_cols, names(queries))
if (length(missing_query_cols) > 0L) {
  stop(
    "Query file is missing column(s): ",
    paste(missing_query_cols, collapse = ", "),
    call. = FALSE
  )
}

if (!"enabled" %in% names(queries)) queries$enabled <- TRUE

queries <- queries %>%
  mutate(
    province = clean_text(province),
    city = clean_text(city),
    enabled = as.logical(enabled)
  ) %>%
  filter(isTRUE(enabled) | enabled %in% TRUE) %>%
  filter(!is.na(province), !is.na(city)) %>%
  distinct(province, city, .keep_all = TRUE)

msg("Loaded ", nrow(queries), " enabled queries to execute.")

# ------------------------------------------------------------------------------
# Request builder
# ------------------------------------------------------------------------------

build_request <- function(province, city) {
  body <- list(
    product_data = list(
      start_time = "",
      end_time = "",
      province = list(),
      city = list(),
      stock_name = list(),
      category = "国控地表水",
      category_id = 3,
      product = "国控地表水断面基础资料",
      product_id = 5
    ),
    product_args = list(
      province_city = list(list(province, city))
    )
  )

  req <- request(ENDPOINT) %>%
    req_method("POST") %>%
    req_headers(
      Accept = "application/json, text/plain, */*",
      Authorization = paste("Bearer", bearer_token),
      Origin = "https://data.epmap.org",
      Referer = REFERER,
      `X-Requested-With` = "XMLHttpRequest"
    ) %>%
    req_body_json(body, auto_unbox = TRUE)

  if (nzchar(csrf_token)) {
    req <- req %>% req_headers(`X-CSRFToken` = csrf_token)
  }
  if (nzchar(cookie_header)) {
    req <- req %>% req_headers(Cookie = cookie_header)
  }

  req
}

perform_query <- function(province, city) {
  last_error <- NULL

  for (attempt in seq_len(MAX_RETRIES)) {
    result <- tryCatch({
      resp <- build_request(province, city) %>%
        req_timeout(30) %>%
        req_perform()

      status <- resp_status(resp)

      if (status == 401 || status == 403) {
        stop("EPMap authentication failed (HTTP ", status, "). Refresh your local session/token.")
      }

      if (status == 429) {
        stop("EPMap rate-limited the request (HTTP 429).")
      }

      if (status >= 500) {
        stop("EPMap server returned HTTP ", status, ".")
      }

      resp_check_status(resp)
      txt <- resp_body_string(resp)
      parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

      list(
        ok = TRUE,
        http_status = status,
        text = txt,
        parsed = parsed,
        error = NA_character_
      )
    }, error = function(e) {
      list(
        ok = FALSE,
        http_status = NA_integer_,
        text = NA_character_,
        parsed = NULL,
        error = conditionMessage(e)
      )
    })

    if (isTRUE(result$ok)) return(result)

    last_error <- result$error

    if (grepl("authentication failed|401|403", last_error, ignore.case = TRUE)) {
      break
    }

    if (attempt < MAX_RETRIES) {
      Sys.sleep(REQUEST_DELAY_SECONDS * attempt)
    }
  }

  list(
    ok = FALSE,
    http_status = NA_integer_,
    text = NA_character_,
    parsed = NULL,
    error = last_error %||% "Unknown request failure"
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

# ------------------------------------------------------------------------------
# Response parser
# ------------------------------------------------------------------------------

parse_demo <- function(parsed, province_query, city_query, collected_at, raw_file) {
  demo <- parsed$demo %||% list()

  if (length(demo) == 0L) return(tibble())

  rows <- purrr::map_dfr(demo, function(z) {
    tibble(
      province = clean_text(z[["省份"]] %||% NA_character_),
      city = clean_text(z[["城市"]] %||% NA_character_),
      river = clean_text(z[["河流"]] %||% NA_character_),
      river_basin = clean_text(z[["流域"]] %||% NA_character_),
      monitoring_section = clean_text(z[["断面名称"]] %||% NA_character_),
      longitude = safe_num(z[["经度"]] %||% NA_character_),
      latitude = safe_num(z[["纬度"]] %||% NA_character_),
      section_attribute = clean_text(z[["断面属性"]] %||% NA_character_),
      description = clean_text(z[["简介"]] %||% NA_character_),
      local_management = clean_text(z[["属地管理"]] %||% NA_character_),
      published_data_start = clean_text(z[["发布数据开始时间"]] %||% NA_character_),
      published_data_end = clean_text(z[["发布数据截止时间"]] %||% NA_character_)
    )
  })

  rows %>%
    mutate(
      coordinate_valid = valid_lonlat(longitude, latitude),
      query_province = province_query,
      query_city = city_query,
      source = "EPMap demo: 国控地表水断面基础资料",
      source_endpoint = ENDPOINT,
      collected_at = collected_at,
      raw_response_file = raw_file
    )
}

# ------------------------------------------------------------------------------
# Log & Skip logic
# ------------------------------------------------------------------------------

query_log <- read_existing_csv(QUERY_LOG_FILE)

already_successful <- function(province, city) {
  if (FORCE_REFRESH || nrow(query_log) == 0L) return(FALSE)
  if (!all(c("province", "city", "status") %in% names(query_log))) return(FALSE)

  any(
    query_log$province == province &
      query_log$city == city &
      query_log$status == "ok",
    na.rm = TRUE
  )
}

# ------------------------------------------------------------------------------
# Collect Loop
# ------------------------------------------------------------------------------

new_rows <- list()
new_logs <- list()

for (i in seq_len(nrow(queries))) {
  province <- queries$province[[i]]
  city <- queries$city[[i]]

  if (already_successful(province, city)) {
    msg("SKIP [", i, "/", nrow(queries), "] already collected: ", province, " / ", city)
    next
  }

  msg("Querying [", i, "/", nrow(queries), "]: ", province, " / ", city)
  collected_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")

  res <- perform_query(province, city)

  if (!isTRUE(res$ok)) {
    msg("FAILED: ", province, " / ", city, " | ", res$error)
    new_logs[[length(new_logs) + 1L]] <- tibble(
      province = province,
      city = city,
      collected_at = collected_at,
      status = "failed",
      http_status = res$http_status,
      data_size = NA_integer_,
      demo_rows = NA_integer_,
      valid_coordinate_rows = NA_integer_,
      demo_matches_reported_size = NA,
      error = res$error
    )
    Sys.sleep(REQUEST_DELAY_SECONDS)
    next
  }

  timestamp_tag <- format(Sys.time(), "%Y%m%d_%H%M%S")
  raw_name <- paste0(
    timestamp_tag, "_",
    safe_slug(province), "_", safe_slug(city), ".json"
  )
  raw_path <- file.path(RAW_DIR, raw_name)
  writeLines(res$text, raw_path, useBytes = TRUE)

  parsed_rows <- parse_demo(
    res$parsed,
    province_query = province,
    city_query = city,
    collected_at = collected_at,
    raw_file = raw_path
  )

  data_size <- suppressWarnings(as.integer(res$parsed$data_size %||% NA_integer_))
  demo_rows <- nrow(parsed_rows)
  valid_coord_rows <- if (demo_rows > 0L) sum(parsed_rows$coordinate_valid, na.rm = TRUE) else 0L

  msg(
    "OK: demo rows=", demo_rows,
    " | valid coordinates=", valid_coord_rows,
    " | reported data_size=", data_size
  )

  if (demo_rows > 0L) {
    new_rows[[length(new_rows) + 1L]] <- parsed_rows
  }

  new_logs[[length(new_logs) + 1L]] <- tibble(
    province = province,
    city = city,
    collected_at = collected_at,
    status = "ok",
    http_status = res$http_status,
    data_size = data_size,
    demo_rows = demo_rows,
    valid_coordinate_rows = valid_coord_rows,
    demo_matches_reported_size = !is.na(data_size) && data_size == demo_rows,
    error = NA_character_
  )

  Sys.sleep(REQUEST_DELAY_SECONDS)
}

# ------------------------------------------------------------------------------
# Save accumulated master
# ------------------------------------------------------------------------------

old_master <- read_existing_csv(MASTER_FILE)
new_master <- if (length(new_rows) > 0L) bind_rows(new_rows) else tibble()

master <- bind_rows(old_master, new_master)

if (nrow(master) > 0L) {
  master <- master %>%
    mutate(
      province = clean_text(province),
      city = clean_text(city),
      river = clean_text(river),
      river_basin = clean_text(river_basin),
      monitoring_section = clean_text(monitoring_section),
      longitude = safe_num(longitude),
      latitude = safe_num(latitude),
      coordinate_valid = valid_lonlat(longitude, latitude)
    ) %>%
    arrange(province, city, monitoring_section, desc(collected_at)) %>%
    distinct(
      province,
      city,
      river_basin,
      monitoring_section,
      longitude,
      latitude,
      .keep_all = TRUE
    )

  write_csv_utf8(master, MASTER_FILE)

  valid_coords <- master %>%
    filter(coordinate_valid) %>%
    arrange(province, city, monitoring_section, desc(collected_at)) %>%
    distinct(province, city, monitoring_section, .keep_all = TRUE)

  write_csv_utf8(valid_coords, VALID_COORD_FILE)

  dir.create(dirname(INBOX_EXPORT), recursive = TRUE, showWarnings = FALSE)
  inbox_coords <- valid_coords %>%
    select(
      province, city, river, river_basin, monitoring_section,
      longitude, latitude, section_attribute, published_data_start,
      published_data_end, source, collected_at
    )
  write_csv_utf8(inbox_coords, INBOX_EXPORT)
}

new_log <- if (length(new_logs) > 0L) bind_rows(new_logs) else tibble()
query_log_final <- bind_rows(query_log, new_log)
if (nrow(query_log_final) > 0L) {
  write_csv_utf8(query_log_final, QUERY_LOG_FILE)
}

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

msg("EPMap demo-coordinate collection complete.")
cat("\nOutput directory:\n  ", OUT_DIR, "\n", sep = "")
cat("Master demo observations:\n  ", MASTER_FILE, "\n", sep = "")
cat("Valid-coordinate crosswalk input:\n  ", VALID_COORD_FILE, "\n", sep = "")
cat("Query log:\n  ", QUERY_LOG_FILE, "\n", sep = "")
cat("Coordinate-inbox export:\n  ", INBOX_EXPORT, "\n", sep = "")