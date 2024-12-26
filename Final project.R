#在男、女刑事嫌疑犯中，受教育程度占比最高的前三名分別為何？
#刑事嫌疑犯中，男、女何者較多？多幾倍？
#教育是否會使刑事嫌疑犯減少？

data <- read_csv("89~105年刑事案件嫌疑犯人數之教育程度─按性別、機關別分.csv")

# 確保 tidyverse 已加載
library(tidyverse)

# 讀取資料並重新命名欄位
data <- read_csv("89~105年刑事案件嫌疑犯人數之教育程度─按性別、機關別分.csv") |>
  dplyr::rename(
    suspect_count = `嫌疑犯人數`,
    total = `總計`,
    illiterate = `不識字`,
    self_taught = `自修`,
    elementary_school = `國小`,
    junior_high_school = `國中`,
    high_school_vocational = `高中(職)`,
    college = `大專`,
    graduate_school = `研究所`,
    unknown = `不詳`
  )

# 分割 `suspect_count` 並建立新欄位
data <- data |>
  tidyr::separate(
    suspect_count,
    into = c("year", "gender", "agency"),
    sep = "/", # 使用 "/" 分隔
    remove = FALSE # 保留原始欄位
  ) |>
  mutate(across(c(year, gender, agency), str_trim)) # 手動清理空格

# 查看結果
glimpse(data)

# 轉換資料
data <- data |>
  tidyr::pivot_longer(
    cols = c(illiterate, self_taught, elementary_school, junior_high_school,
             high_school_vocational, college, graduate_school, unknown),
    names_to = "education_level",
    values_to = "count"
  ) |>
  mutate(
    # 清理非數字字符並轉換
    count = as.integer(stringr::str_replace_all(count, "[^0-9]", "")),
    total = as.integer(stringr::str_replace_all(total, "[^0-9]", "")),
    # 教育程度作為順序變數
    education_level = forcats::fct_relevel(
      education_level,
      c("illiterate", "self_taught", "elementary_school",
        "junior_high_school", "high_school_vocational",
        "college", "graduate_school", "unknown")
    ),
    # 名義變數轉為因子
    year = as.factor(year),
    gender = as.factor(gender),
    agency = as.factor(agency)
  )

# 查看結果
glimpse(data)

# 計算教育程度佔比並轉換為百分比
data_with_percentages <- data |>
  group_by(suspect_count, total) |>
  mutate(
    percentage = (count / total) * 100
  )

# 四捨五入至小數點後兩位
data_with_percentages <- data_with_percentages |>
  mutate(
    percentage = round(percentage, 2)
  )

# 查看結果
print(data_with_percentages)

# 將百分比展開為寬格式
data_wide_percentages <- data_with_percentages |>
  select(suspect_count, education_level, percentage) |>
  tidyr::pivot_wider(
    names_from = education_level,
    values_from = percentage
  )
print(data_wide_percentages)

#在男刑事嫌疑犯中，受教育程度占比最高的前三名分別為何？
# 定義要篩選的條件
filter_conditions <- c(
  "89年/ 男/ 機關別總計", "90年/ 男/ 機關別總計", "91年/ 男/ 機關別總計",
  "92年/ 男/ 機關別總計", "93年/ 男/ 機關別總計", "94年/ 男/ 機關別總計",
  "95年/ 男/ 機關別總計", "96年/ 男/ 機關別總計", "97年/ 男/ 機關別總計",
  "98年/ 男/ 機關別總計", "99年/ 男/ 機關別總計", "100年/ 男/ 機關別總計",
  "101年/ 男/ 機關別總計", "102年/ 男/ 機關別總計", "103年/ 男/ 機關別總計",
  "104年/ 男/ 機關別總計", "105年/ 男/ 機關別總計", "106年/ 男/ 機關別總計",
  "107年/ 男/ 機關別總計", "108年/ 男/ 機關別總計", "109年/ 男/ 機關別總計",
  "110年/ 男/ 機關別總計", "111年/ 男/ 機關別總計"
)

# 篩選資料
filtered_male <- data_with_percentages |>
  dplyr::filter(suspect_count %in% filter_conditions)

# 查看結果
print(filtered_male)

# 確保 year 為數值型別
filtered_male <- filtered_male |>
  mutate(year = as.numeric(stringr::str_remove(year, "年")))

# 使用 ggplot2 繪製折線圖
library(ggplot2)

ggplot(filtered_male, aes(x = year, y = total)) +
  geom_line(color = "blue", size = 1) + # 添加折線
  geom_point(color = "red", size = 2) + # 添加點
  labs(
    title = "年度總嫌疑犯人數變化趨勢",
    x = "年份",
    y = "嫌疑犯總人數"
  ) +
  theme_minimal() + # 使用簡潔主題
  theme(
    text = element_text(family = "sans", size = 14),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

#整體來說是上升
#嫌疑犯受教育程度的前三名分別為高中（職）再來是國中接著是大學

#在女刑事嫌疑犯中，受教育程度占比最高的前三名分別為何？
# 定義篩選條件
filter_conditions_female <- c(
  "89年/ 女/ 機關別總計", "90年/ 女/ 機關別總計", "91年/ 女/ 機關別總計",
  "92年/ 女/ 機關別總計", "93年/ 女/ 機關別總計", "94年/ 女/ 機關別總計",
  "95年/ 女/ 機關別總計", "96年/ 女/ 機關別總計", "97年/ 女/ 機關別總計",
  "98年/ 女/ 機關別總計", "99年/ 女/ 機關別總計", "100年/ 女/ 機關別總計",
  "101年/ 女/ 機關別總計", "102年/ 女/ 機關別總計", "103年/ 女/ 機關別總計",
  "104年/ 女/ 機關別總計", "105年/ 女/ 機關別總計", "106年/ 女/ 機關別總計",
  "107年/ 女/ 機關別總計", "108年/ 女/ 機關別總計", "109年/ 女/ 機關別總計",
  "110年/ 女/ 機關別總計", "111年/ 女/ 機關別總計"
)

# 篩選符合條件的資料
filtered_female <- data_with_percentages |>
  dplyr::filter(suspect_count %in% filter_conditions_female)
library(ggplot2)

# 繪製總數的折線圖
ggplot(filtered_female, aes(x = year, y = total, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "女性嫌疑犯總數趨勢",
    x = "年份",
    y = "總數"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16)
  )

#整體來說是上升
#嫌疑犯受教育程度的前三名分別為高中（職）再來是大學接著是國中

#刑事嫌疑犯中，男比女多幾倍？
# 篩選男性和女性的資料
male_data <- data_with_percentages |>
  dplyr::filter(gender == "男")

female_data <- data_with_percentages |>
  dplyr::filter(gender == "女")

# 合併男性和女性的總數資料，按年份對齊
male_duplicates <- male_data |>
  dplyr::count(year) |>
  dplyr::filter(n > 1)

female_duplicates <- female_data |>
  dplyr::count(year) |>
  dplyr::filter(n > 1)

print(male_duplicates)
print(female_duplicates)

male_data <- male_data |>
  dplyr::group_by(year) |>
  dplyr::summarise(male_total = sum(total, na.rm = TRUE), .groups = "drop")

female_data <- female_data |>
  dplyr::group_by(year) |>
  dplyr::summarise(female_total = sum(total, na.rm = TRUE), .groups = "drop")

combined_data <- male_data |>
  dplyr::inner_join(female_data, by = "year")

unique(combined_data$year)

combined_data <- combined_data |>
  dplyr::mutate(year = gsub("[^0-9]", "", year)) |>
  dplyr::mutate(year = as.numeric(year))

any(is.na(combined_data$year))

combined_data <- full_years |>
  dplyr::left_join(combined_data, by = "year") |>
  dplyr::mutate(
    male_total = tidyr::replace_na(male_total, 0),
    female_total = tidyr::replace_na(female_total, 0)
  ) |>
  dplyr::mutate(
    male_to_female_ratio = ifelse(female_total == 0, NA, male_total / female_total)
  )

full_years <- data.frame(year = 89:111)
combined_data <- full_years |>
  dplyr::left_join(combined_data, by = "year") |>
  dplyr::mutate(
    male_total = tidyr::replace_na(male_total, 0),
    female_total = tidyr::replace_na(female_total, 0),
    male_to_female_ratio = tidyr::replace_na(male_to_female_ratio, 0)
  )

combined_data <- combined_data |>
  dplyr::mutate(year = as.numeric(as.character(year)))

head(combined_data)

# 計算男性是女性的幾倍
combined_data <- combined_data |>
  dplyr::mutate(male_to_female_ratio = male_total / female_total)

# 繪製男性是女性的倍數的折線圖
library(ggplot2)

ggplot(combined_data, aes(x = year, y = male_to_female_ratio)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "orange", size = 2) +
  scale_x_continuous(breaks = 89:111, labels = as.character(89:111)) +
  labs(
    title = "男性總數是女性總數的倍數趨勢",
    x = "年份",
    y = "倍數"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16)
  )

# 篩選 "111年/ 女/ 機關別總計"
selected_female <- data_with_percentages |>
  dplyr::filter(suspect_count == "111年/ 女/ 機關別總計") |>
  select(total, suspect_count, education_level, percentage) |>
  tidyr::pivot_wider(
    names_from = education_level,
    values_from = percentage
  )

# 查看結果
glimpse(selected_female)

#89~105每年男生皆比女生多
#男生的犯罪率是女生的X倍以上，這可能也體現出了男性與女性在個體或個性上的差異

#教育是否會使刑事嫌疑犯減少？
library(dplyr)

# 篩選出指定的條件
data_filtered <- data %>%
  filter(suspect_count %in% c(
    "89年/ 性別總計/ 機關別總計", "90年/ 性別總計/ 機關別總計", "91年/性別總計/ 機關別總計",
    "92年/性別總計/ 機關別總計", "93年/ 性別總計/ 機關別總計", "94年/ 性別總計/ 機關別總計",
    "95年/ 性別總計/ 機關別總計", "96年/ 性別總計/ 機關別總計", "97年/ 性別總計/ 機關別總計",
    "98年/ 性別總計/ 機關別總計", "99年/ 性別總計/ 機關別總計", "100年/ 性別總計/ 機關別總計",
    "101年/性別總計/ 機關別總計", "102年/ 性別總計/ 機關別總計", "103年/ 性別總計/ 機關別總計",
    "104年/ 性別總計/ 機關別總計", "105年/ 性別總計/ 機關別總計", "106年/ 男/ 機關別總計",
    "107年/ 性別總計/ 機關別總計", "108年/ 性別總計/ 機關別總計", "109年/ 性別總計/ 機關別總計",
    "110年/ 性別總計/ 機關別總計", "111年/ 性別總計/ 機關別總計"
  ))

# 查看篩選結果
glimpse(data_filtered)

library(ggplot2)
library(dplyr)

# 繪製每個教育程度 (education_level) 的折線圖
ggplot(data, aes(x = year, y = count, group = education_level, color = education_level)) +
  geom_line() + 
  facet_wrap(~ education_level, scales = "free_y") + # 為每個教育程度創建子圖
  labs(
    title = "不同教育程度的嫌疑犯人數變化",
    x = "年份",
    y = "嫌疑犯人數 (count)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # 調整X軸標籤角度
    legend.position = "none"  # 隱藏圖例
  )

# 過濾掉包含NA的行
data_filtered <- data_filtered %>%
  filter(!is.na(year) & !is.na(count))
data_filtered <- data_filtered %>%
  mutate(year = as.integer(str_replace(year, "年", "")) + 1911) # 去掉 "年" 並轉為西元年
summary(data_filtered$year)
summary(data_filtered$count)
ggplot(data_filtered, aes(x = year, y = count, group = education_level, color = education_level)) +
  geom_line() + 
  facet_wrap(~ education_level, scales = "free_y") + # 為每個教育程度創建子圖
  labs(
    title = "不同教育程度的嫌疑犯人數變化",
    x = "年份",
    y = "嫌疑犯人數 (count)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # 調整X軸標籤角度
    legend.position = "none"  # 隱藏圖例
  )

# 繪製 total 的折線圖
ggplot(data_filtered, aes(x = year, y = total, group = 1)) +
  geom_line(color = "blue") +  # 繪製折線
  labs(
    title = "總嫌疑犯人數變化",
    x = "年份",
    y = "總嫌疑犯人數 (total)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # 調整X軸標籤角度
    legend.position = "none"  # 隱藏圖例
  )

# 繪製 total 和 education_level count 的折線圖
data_combined <- data_filtered %>%
  select(year, education_level, count, total) %>%
  pivot_longer(cols = c(count, total), names_to = "type", values_to = "value")

ggplot(data_combined, aes(x = year, y = value, group = interaction(education_level, type), color = type)) +
  geom_line() + 
  facet_wrap(~ education_level, scales = "free_y") +  # 為每個教育程度創建子圖
  labs(
    title = "不同教育程度的嫌疑犯人數變化 (count 和 total)",
    x = "年份",
    y = "人數"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # 調整X軸標籤角度
    legend.position = "top"  # 顯示圖例
  )


#十二年國教
#國中高中轉捩點
#大部分的刑事案件嫌疑者的受教育程度為高中（職）生，而大學跟國中生的比例是差不多的
#表示即使普遍的人皆受到了相對高等的教育，但刑事案件的數量依舊沒有減少








