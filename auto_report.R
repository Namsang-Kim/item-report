.libPaths("/home/namsangkim/item-report/renv/library/R-4.0/x86_64-pc-linux-gnu")

# 📁 작업 디렉토리 설정
setwd("/home/namsangkim/item-report")

# 📦 필수 패키지 로드
library(bit)
library(bit64)
library(datarizer)
library(DBI)
library(RMySQL)
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
library(gridExtra)
library(glue)

# 🕒 시간대 및 기본 설정
Sys.setenv(TZ = "Asia/Seoul")
SITE_ID <- "11790"
today_label <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# 📥 데이터 불러오기
site_mst <- get_query(to = "athena", "
  SELECT * 
  FROM dashboard.site_mst 
  WHERE site_id = {SITE_ID}", output_type = "data.table")
SITE_NAME <- site_mst[site_id == SITE_ID]$site_name

item_profile_dt <- get_query(to = "athena", "
  SELECT * 
  FROM profiling.item_profile_daily
  WHERE site_id = {SITE_ID}
  AND date(date_id) BETWEEN date_add('day', -14, current_date) AND date_add('day', -1, current_date)
", output_type = "data.table")

item_mst <- get_query(to = "athena", "
  SELECT item_id, item_name 
  FROM src_meta.item_mst
  WHERE site_id = {SITE_ID}", output_type = "data.table")

# 🧼 사은품 제외 및 병합
item_mst <- item_mst %>% filter(!grepl("사은품", item_name))
item_profile_dt <- item_profile_dt %>% inner_join(item_mst, by = "item_id")

# 📊 일별 지표 계산
daily_item_trend <- item_profile_dt %>%
  mutate(date = ymd(date_id)) %>%
  group_by(item_id, item_name, date) %>%
  summarise(
    imp = sum(imp_cnt, na.rm = TRUE),
    click = sum(click_cnt, na.rm = TRUE),
    conversion = sum(conversion_cnt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CTR = ifelse(imp > 0, round(click / imp * 100, 2), 0),
    CVR = ifelse(click > 0, round(conversion / click * 100, 2), 0)
  )

# 🔢 판매 비중 계산
total_conversion_by_date <- daily_item_trend %>%
  group_by(date) %>%
  summarise(total_conversion = sum(conversion, na.rm = TRUE), .groups = "drop")

daily_item_trend <- daily_item_trend %>%
  left_join(total_conversion_by_date, by = "date") %>%
  mutate(sales_share = ifelse(total_conversion > 0, round(conversion / total_conversion * 100, 2), 0))

# 📌 최근 날짜 기준 지표 상승률 계산
latest_date <- max(daily_item_trend$date)
prev_dates <- daily_item_trend %>% filter(date < latest_date)

ctr_cvr_growth <- daily_item_trend %>%
  filter(date == latest_date) %>%
  select(item_id, item_name, CTR, CVR) %>%
  rename(recent_ctr = CTR, recent_cvr = CVR) %>%
  left_join(
    prev_dates %>%
      group_by(item_id, item_name) %>%
      summarise(
        past_ctr = mean(CTR, na.rm = TRUE),
        past_cvr = mean(CVR, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("item_id", "item_name")
  ) %>%
  mutate(
    ctr_growth = ifelse(past_ctr > 0, (recent_ctr - past_ctr) / past_ctr * 100, NA),
    cvr_growth = ifelse(past_cvr > 0, (recent_cvr - past_cvr) / past_cvr * 100, NA)
  )

# 🏆 Top 10 상승 상품 선정
top_10_items <- ctr_cvr_growth %>%
  arrange(desc(ctr_growth + cvr_growth)) %>%
  slice(1:10) %>%
  mutate(rank = row_number()) %>%
  select(rank, item_id, item_name, ctr_growth, cvr_growth)

# 📋 Top 10 표 저장
table_plot <- tableGrob(
  top_10_items %>%
    rename(
      순위 = rank,
      `상품 ID` = item_id,
      `상품명` = item_name,
      `CTR 상승률 (%)` = ctr_growth,
      `CVR 상승률 (%)` = cvr_growth
    )
)

ggsave("top_10_table.png", table_plot, width = 10, height = 4, dpi = 300)

# 📈 추이 그래프 함수
plot_item_trend <- function(df, item_id, item_name) {
  df <- df %>% filter(item_id == !!item_id)
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = CTR, color = "CTR"), linewidth = 1.2) +
    geom_line(aes(y = CVR, color = "CVR"), linewidth = 1.2) +
    geom_line(aes(y = sales_share, color = "판매 비중 (%)"), linetype = "dashed", linewidth = 1.1) +
    scale_y_continuous(name = "비율 (%)") +
    labs(
      title = paste0("상품: ", item_name, " (ID: ", item_id, ")"),
      x = "날짜", y = NULL, color = "지표"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )
}

# 📉 Top 10 추이 그래프
top_10_trend_plots <- lapply(1:nrow(top_10_items), function(i) {
  item_id <- top_10_items$item_id[i]
  item_name <- top_10_items$item_name[i]
  plot_item_trend(daily_item_trend, item_id, item_name)
})

# 📤 그래프 저장
ggsave("top_10_items_report.png",
       plot = grid.arrange(grobs = top_10_trend_plots, ncol = 1),
       width = 8, height = 36, dpi = 300)

# 🌐 HTML 생성
html_code <- glue('
<!DOCTYPE html>
<html lang="ko">
<head>
  <meta charset="UTF-8">
  <title>CTR / CVR 상승 상품 리포트</title>
  <style>
    body {{ font-family: sans-serif; text-align: center; margin: 2em; }}
    img {{ max-width: 95%; margin-bottom: 2em; }}
  </style>
</head>
<body>
  <h1>CTR / CVR 상승 상품 리포트</h1>
  <p>{today_label} 기준 최근 14일 CTR/CVR이 상승한 상위 10개 상품입니다.</p>
  <img src="top_10_table.png" alt="Top10 상품 표">
  <img src="top_10_items_report.png" alt="Top10 상품 추이 그래프">
</body>
</html>
')

writeLines(html_code, "index.html")

# 🚀 Git 자동 푸시 (충돌 방지 포함)
system("git config user.name 'github-actions'")
system("git config user.email 'actions@github.com'")

# 변경 파일 전체 스테이징
system("git add -A", intern = TRUE)

# 커밋
commit_msg <- system("git commit -m '자동 리포트 갱신'", intern = TRUE)

if (!any(grepl("no changes added to commit", commit_msg))) {
  cat("✅ 변경사항 커밋 완료\n")
  
  pull_status <- tryCatch({
    system("git pull --rebase", intern = TRUE)
  }, error = function(e) e)
  
  if (!inherits(pull_status, "error")) {
    cat("✅ git pull 성공 → push 진행\n")
    push_status <- system("git push origin main", intern = TRUE)
    if (is.null(attr(push_status, "status"))) {
      cat("✅ git push 완료\n")
    } else {
      cat("❌ git push 실패\n")
    }
  } else {
    cat("⚠️ git pull 실패 - 충돌 가능성 있음. 푸시 중단됨.\n")
  }
} else {
  cat("ℹ️ 커밋할 변경사항 없음. Git push 생략됨.\n")
}