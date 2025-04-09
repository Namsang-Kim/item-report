# 📦 패키지 로드
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
library(gridExtra)

# 🌐 기본 설정
Sys.setenv(TZ = "Asia/Seoul")
SITE_ID <- "11790"

# 📥 데이터 불러오기
site_mst <- get_query(to = "prod", "
  SELECT * 
  FROM dashboard.site_mst 
  WHERE site_id = {SITE_ID}",
                      output_type = "data.table")

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
  WHERE site_id = {SITE_ID}",
                      output_type = "data.table"
)

# 🧼 사은품 제외 및 병합
item_mst <- item_mst %>% filter(!grepl("사은품", item_name))
item_profile_dt <- item_profile_dt %>% inner_join(item_mst, by = "item_id")

# 📊 일별 상품 지표 계산
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

# 🔢 전체 전환수 대비 판매 비중 계산
total_conversion_by_date <- daily_item_trend %>%
  group_by(date) %>%
  summarise(total_conversion = sum(conversion, na.rm = TRUE), .groups = "drop")

daily_item_trend <- daily_item_trend %>%
  left_join(total_conversion_by_date, by = "date") %>%
  mutate(sales_share = ifelse(total_conversion > 0, round(conversion / total_conversion * 100, 2), 0))

# 🔍 최근 대비 CTR/CVR 증가율 계산
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

# 1️⃣ 최근 14일간 전환수 기준 상위 100개 상품 선별
top_100_items <- daily_item_trend %>%
  group_by(item_id, item_name) %>%
  summarise(total_conversion = sum(conversion, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_conversion)) %>%
  slice(1:100)

# 2️⃣ 해당 상품들만 필터링
filtered_trend <- daily_item_trend %>%
  filter(item_id %in% top_100_items$item_id)

# 3️⃣ CTR / CVR 증가율 계산
latest_date <- max(filtered_trend$date)
prev_dates <- filtered_trend %>% filter(date < latest_date)

ctr_cvr_growth <- filtered_trend %>%
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

# 4️⃣ top 3 추출
top_growth_items <- ctr_cvr_growth %>%
  arrange(desc(ctr_growth + cvr_growth)) %>%
  slice(1:3)

# 📈 시각화 함수 정의
plot_item_trend <- function(df, item_id, item_name) {
  df <- df %>% filter(item_id == !!item_id)
  
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = CTR, color = "CTR"), size = 1.2) +
    geom_line(aes(y = CVR, color = "CVR"), size = 1.2) +
    geom_line(aes(y = sales_share, color = "판매 비중 (%)"), linetype = "dashed", size = 1.1) +
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

# 🖼 top 3 상품 시각화
plots <- lapply(1:nrow(top_growth_items), function(i) {
  item_id <- top_growth_items$item_id[i]
  item_name <- top_growth_items$item_name[i]
  plot_item_trend(daily_item_trend, item_id, item_name)
})

# 🧱 grid 출력
grid.arrange(grobs = plots, ncol = 1)

ggsave("top_items_report.png", plot = grid.arrange(grobs = plots, ncol = 1),
       width = 8, height = 12, dpi = 300)

# 🌐 HTML 생성
html_code <- '
<!DOCTYPE html>
<html lang="ko">
<head>
  <meta charset="UTF-8">
  <title>CTR / CVR 상승 상품 리포트</title>
  <style>
    body { font-family: sans-serif; text-align: center; margin: 2em; }
    img { max-width: 90%; margin-bottom: 2em; }
  </style>
</head>
<body>
  <h1>CTR / CVR 상승 상품 리포트</h1>
  <p>최근 14일 기준 CTR/CVR이 상승한 상위 3개 상품입니다.</p>
  <img src="top_items_report.png" alt="Top 상품 리포트">
</body>
</html>
'

writeLines(html_code, "index.html")

# 🚀 Git 자동 커밋 & 푸시
system("git config user.name 'github-actions'")
system("git config user.email 'actions@github.com'")
system("git add index.html top_items_report.png", intern = TRUE)
system("git commit -m '자동 리포트 갱신' || echo 'No changes to commit'", intern = TRUE)
system("git push origin main", intern = TRUE)