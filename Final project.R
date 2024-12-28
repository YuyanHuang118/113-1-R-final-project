#男、女刑事嫌疑犯在2000年~2016年中，年度總嫌疑犯人數變化趨勢為何？
#男、女刑事嫌疑犯在2000年~2016年中，受教育程度占比最高的前三名分別為何？
#對比男、女刑事嫌疑犯在2000年~2016年中，受教育程度占比最高的前三名
#在2000年~2016年中的刑事嫌疑犯，男、女何者較多？多幾倍？
#以2000年~2016年的刑事案件嫌疑犯人數之教育程度，推斷教育是否會使刑事嫌疑犯減少？

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

# 修改欄位名稱，加上 "（%）" 後綴
data_wide_percentages <- data_wide_percentages |>
  dplyr::rename_with(
    .cols = c(
      "illiterate", "self_taught", "elementary_school", 
      "junior_high_school", "high_school_vocational", 
      "college", "graduate_school", "unknown"
    ),
    .fn = ~ paste0(., "（%）")
  )

# 驗證修改結果
glimpse(data_wide_percentages)

#男刑事嫌疑犯在2000年~2016年中，年度總嫌疑犯人數變化趨勢為何？
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
filtered_male <- data_wide_percentages |>
  dplyr::filter(suspect_count %in% filter_conditions)

# 查看結果
print(filtered_male)

# 分割並移除 suspect_count
filtered_male <- filtered_male |>
  tidyr::separate(
    col = suspect_count,
    into = c("year", "gender", "agency"),
    sep = "/ ", # 根據分隔符進行切分
    remove = TRUE # 分割後移除原始欄位
  ) |>
  mutate(
    year = as.numeric(stringr::str_remove(year, "年")) # 確保 year 為數值型別
  )

# 查看結果
print(filtered_male)

# 將 year 欄位從民國年轉換為西元年
filtered_male <- filtered_male |>
  mutate(year = year + 1911)

# 查看結果
print(filtered_male)

# 確保 year 為數值型別
filtered_male <- filtered_male |>
  mutate(year = as.numeric(stringr::str_remove(year, "年")))

# 使用 ggplot2 繪製折線圖
library(ggplot2)

ggplot(filtered_male, aes(x = year, y = total)) +
  geom_line(color = "blue", linewidth = 1) + # 修正為 linewidth
  geom_point(color = "red", size = 2) + # 點的大小仍使用 size
  labs(
    title = "男性嫌疑犯總數趨勢",
    x = "年份",
    y = "嫌疑犯總人數"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", size = 14),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

#在2000~2016中，以整體趨勢來說，男生總嫌疑犯數量有增加趨勢

#在男刑事嫌疑犯中，受教育程度占比最高的前三名分別為何？
# 取消分組並刪除 total 欄位
filtered_male <- filtered_male |>
  dplyr::ungroup() |> 
  dplyr::select(-total)

# 查看結果
print(filtered_male)

# 加總不同年份的百分比
summed_percentages <- filtered_male |>
  summarize(
    `illiterate（%）_sum` = sum(`illiterate（%）`, na.rm = TRUE),
    `self_taught（%）_sum` = sum(`self_taught（%）`, na.rm = TRUE),
    `elementary_school（%）_sum` = sum(`elementary_school（%）`, na.rm = TRUE),
    `junior_high_school（%）_sum` = sum(`junior_high_school（%）`, na.rm = TRUE),
    `high_school_vocational（%）_sum` = sum(`high_school_vocational（%）`, na.rm = TRUE),
    `college（%）_sum` = sum(`college（%）`, na.rm = TRUE),
    `graduate_school（%）_sum` = sum(`graduate_school（%）`, na.rm = TRUE),
    `unknown（%）_sum` = sum(`unknown（%）`, na.rm = TRUE)
  )

# 查看加總結果
print(summed_percentages)

# 將數據轉換為長格式
summed_percentages_long <- summed_percentages |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "education_level",
    values_to = "percentage"
  )

# 排序並選取百分比最高的前三名
top3 <- summed_percentages_long |>
  dplyr::arrange(desc(percentage)) |>
  dplyr::slice_head(n = 3)

top3_male <- top3 |> 
  dplyr::mutate(gender = "Male")

# 查看結果
print(top3_male)

#在2000年~2016年中，男生嫌疑犯受教育程度的前三名
#分別為高中（職）接著是國中，再來是小學

#女刑事嫌疑犯在2000年~2016年中，年度總嫌疑犯人數變化趨勢為何？
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
filtered_female <- data_wide_percentages |>
  dplyr::filter(suspect_count %in% filter_conditions_female)

# 查看結果
print(filtered_female)

# 分割並移除 suspect_count
filtered_female <- filtered_female |>
  tidyr::separate(
    col = suspect_count,
    into = c("year", "gender", "agency"),
    sep = "/ ", # 根據分隔符進行切分
    remove = TRUE # 分割後移除原始欄位
  ) |>
  mutate(
    year = as.numeric(stringr::str_remove(year, "年")) # 確保 year 為數值型別
  )

# 查看結果
print(filtered_female)

# 將 year 欄位從民國年轉換為西元年
filtered_female <- filtered_female |>
  mutate(year = year + 1911)

# 查看結果
print(filtered_female)

# 確保 year 為數值型別
filtered_female <- filtered_female |>
  mutate(year = as.numeric(stringr::str_remove(year, "年")))

# 使用 ggplot2 繪製折線圖
library(ggplot2)

# 繪製總數的折線圖
ggplot(filtered_female, aes(x = year, y = total, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +  # 使用 linewidth 取代 size
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

#在2000年~2016年中，以整體趨勢來說，女生總嫌疑犯數量也有增加趨勢

#在女刑事嫌疑犯中，受教育程度占比最高的前三名分別為何？
# 取消分組並刪除 total 欄位
filtered_female <- filtered_female |>
  dplyr::ungroup() |> 
  dplyr::select(-total)

# 查看結果
print(filtered_female)

# 加總不同年份的百分比
summed_percentages <- filtered_female |>
  summarize(
    `illiterate（%）_sum` = sum(`illiterate（%）`, na.rm = TRUE),
    `self_taught（%）_sum` = sum(`self_taught（%）`, na.rm = TRUE),
    `elementary_school（%）_sum` = sum(`elementary_school（%）`, na.rm = TRUE),
    `junior_high_school（%）_sum` = sum(`junior_high_school（%）`, na.rm = TRUE),
    `high_school_vocational（%）_sum` = sum(`high_school_vocational（%）`, na.rm = TRUE),
    `college（%）_sum` = sum(`college（%）`, na.rm = TRUE),
    `graduate_school（%）_sum` = sum(`graduate_school（%）`, na.rm = TRUE),
    `unknown（%）_sum` = sum(`unknown（%）`, na.rm = TRUE)
  )

# 查看加總結果
print(summed_percentages)

# 將數據轉換為長格式
summed_percentages_long <- summed_percentages |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "education_level",
    values_to = "percentage"
  )

# 排序並選取百分比最高的前三名
top3 <- summed_percentages_long |>
  dplyr::arrange(desc(percentage)) |>
  dplyr::slice_head(n = 3)

# 為女性結果新增性別標註
top3_female <- top3 |> 
  dplyr::mutate(gender = "Female")

# 查看結果
print(top3_female)

#在2000年~2016年中，女嫌疑犯受教育程度的前三名
#分別為高中（職）接著是國中，再來是大學

# 男、女對比
print(top3_male)

#第一名與第二名都是一樣的，但男生的第三名是小學，而女生則是大學
#可能與男生、女生，天生個體上的差異有關

#刑事嫌疑犯中，男比女多幾倍？
unique(data_with_percentages$gender)

data_with_percentages <- data_with_percentages |>
  mutate(gender = stringr::str_trim(gender)) # 去掉空格

# 篩選男性與女性數據
gender_totals <- data_with_percentages |>
  dplyr::filter(gender %in% c("男", "女")) |>  # 選取男性與女性數據
  dplyr::group_by(year, gender) |>            # 根據年份與性別分組
  dplyr::summarize(total = sum(total, na.rm = TRUE), .groups = "drop") # 移除分組提示

# 檢查篩選後的數據
print(gender_totals)

library(dplyr)
library(stringr)
library(lubridate)

# 將 year 欄位從民國年轉換為西元年並排列
gender_totals <- gender_totals |>
  mutate(year = as.integer(str_remove(year, "年")) + 1911) |> # 去掉 "年" 並加上 1911
  arrange(year) # 按年份升序排列

# 檢查結果
print(gender_totals)

# 將數據轉為寬格式
gender_wide <- gender_totals |>
  tidyr::pivot_wider(
    names_from = gender,
    values_from = total,
    names_prefix = "gender_"
  )

# 計算男性數量是女性的幾倍
gender_wide <- gender_wide |>
  dplyr::mutate(male_to_female_ratio = gender_男 / gender_女)

# 查看結果
print(gender_wide)

library(ggplot2)

# 繪製折線圖
ggplot(gender_wide, aes(x = year, y = male_to_female_ratio)) +
  geom_line(color = "blue", size = 1) + # 繪製折線
  geom_point(color = "red", size = 2) + # 添加數據點
  labs(
    title = "刑事嫌疑犯中男性對女性的倍數變化",
    x = "年份",
    y = "男性是女性的倍數"
  ) +
  theme_minimal() + # 使用簡約主題
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # 標題居中
    axis.title = element_text(size = 14), # 坐標軸標題
    axis.text = element_text(size = 12)   # 坐標軸刻度文字
  )

#雖然在2000年~2016年中，每年男嫌疑犯皆比女嫌疑犯多了四倍以上
#但是以整體趨勢來說，在刑事嫌疑犯中男性對於女性的倍數有減少的趨勢
#也就是說，在這段時間內，女嫌疑犯增加漲幅大於男嫌疑犯

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
    "104年/ 性別總計/ 機關別總計", "105年/ 性別總計/ 機關別總計"
  ))

# 查看篩選結果
glimpse(data_filtered)

# 分割並移除 suspect_count
data_filtered <- data_filtered |>
  tidyr::separate(
    col = suspect_count,
    into = c("year", "gender", "agency"),
    sep = "/ ", # 根據分隔符進行切分
    remove = TRUE # 分割後移除原始欄位
  ) |>
  mutate(
    year = as.numeric(stringr::str_remove(year, "年")) # 確保 year 為數值型別
  )

# 查看結果
print(data_filtered)

# 將 year 欄位從民國年轉換為西元年
data_filtered <- data_filtered |>
  mutate(year = year + 1911)

# 查看結果
print(data_filtered)

# 確保 year 為數值型別
data_filtered <- data_filtered |>
  mutate(year = as.numeric(stringr::str_remove(year, "年")))

library(dplyr)
library(ggplot2)

# 1. 獨立 total 的數據
data_total <- data_filtered |>
  group_by(year) |>
  summarise(count = unique(total)) |>
  ungroup()

# 2. 教育程度的數據（去除 total）
data_education <- data_filtered

# 3. 繪製 total 的折線圖
plot_total <- ggplot(data_total, aes(x = year, y = count)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "刑事嫌疑犯總數的變化",
    x = "年份",
    y = "嫌疑犯總數"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 4. 繪製教育程度的折線圖
plot_education <- ggplot(data_education, aes(x = year, y = count, group = education_level, color = education_level)) +
  geom_line(linewidth = 1) +
  facet_wrap(~education_level, scales = "free_y") +
  labs(
    title = "不同教育程度的刑事嫌疑犯數量變化",
    x = "年份",
    y = "嫌疑犯數量"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # 隱藏圖例
  )

# 5. 顯示圖表
print(plot_education)
print(plot_total)

#在2000年~2016年中，以整體趨勢來說，僅有不識字跟小學的數量是呈下降趨勢
#自學和國中數量搖擺不定沒有固定趨勢
#而高中（職）、大學、研究所還有不詳的數量是呈上升趨勢

#在2000年嫌疑犯的教育程度數量第一名是國中，而到了2016年則變成了高中
#表示嫌疑犯的教育程度是有往上提高的趨勢

#臺灣的學習風氣鼎盛，很多人都有接受高中以上的教育
#可是total整體是呈上升趨勢
#代表即使普遍的人皆受到了相對高等的教育，但刑事案件的數量依舊沒有減少

#因此我們可以知道，在2000年~2016年中，教育並沒有降低刑事嫌疑犯數量
