#在男、女刑事嫌疑犯中，受教育程度占比最高的前三名分別為何？
#刑事嫌疑犯中，男、女占比為何？何者較多？多幾倍？
#教育是否會使刑事嫌疑犯減少？

data <- read_csv("c0620202-2134091124.csv")

# 確保 tidyverse 已加載
library(tidyverse)

# 讀取資料並重新命名欄位
data <- read_csv("c0620202-2134091124.csv") |>
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

# 將選定的欄位統一轉換為數值型別，非數值資料轉換為 NA
data <- data |>
  mutate(across(
    c(illiterate, self_taught, elementary_school, junior_high_school, 
      high_school_vocational, college, graduate_school, unknown),
    ~ as.numeric(stringr::str_replace_all(., "[^0-9.]", "")) # 移除非數字字符後轉換為數值
  ))

# 建立代表教育程度的順序變數
data <- data |>
  tidyr::pivot_longer(
    cols = c(illiterate, self_taught, elementary_school, junior_high_school, 
             high_school_vocational, college, graduate_school, unknown),
    names_to = "education_level",
    values_to = "count"
  ) |>
  mutate(
    education_level = forcats::fct_relevel(
      education_level,
      c("illiterate", "self_taught", "elementary_school", 
        "junior_high_school", "high_school_vocational", 
        "college", "graduate_school", "unknown")
    )
  )

# 查看轉換後的資料
print(data)

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

print(data_with_percentages)

# 將百分比展開為寬格式
data_wide_percentages <- data_with_percentages |>
  select(suspect_count, education_level, percentage) |>
  tidyr::pivot_wider(
    names_from = education_level,
    values_from = percentage
  )
print(data_wide_percentages)

# 篩選 "111年/ 男/ 機關別總計"
selected_male <- data_with_percentages |>
  dplyr::filter(suspect_count == "111年/ 男/ 機關別總計") |>
  select(total, suspect_count, education_level, percentage) |>
  tidyr::pivot_wider(
    names_from = education_level,
    values_from = percentage
  )

# 查看結果
glimpse(selected_male)

#按照大小排列
#嫌疑犯受教育程度的前三名分別為高中（職）再來是國中接著是大學

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

#按照大小排列
#嫌疑犯受教育程度的前三名分別為高中（職）再來是大學接著是國中

#請幫我對照"111年/ 男/ 機關別總計"那一列中的"total"與"111年/ 女/ 機關別總計"那一列中的"total"
#算出男性是女性的幾倍
#男生的犯罪率是女生的X倍，這可能也體現出了男性與女性在個體或個性上的差異

# 篩選 "111年/ 性別總計/ 機關別總計" 的資料
summary_data <- data_wide_percentages |>
  dplyr::filter(suspect_count == "111年/ 性別總計/ 機關別總計")

# 查看結果
glimpse(summary_data)

#大部分的刑事案件嫌疑者的受教育程度為高中（職）生，而大學跟國中生的比例是差不多的
#表示即使普遍的人皆受到了相對高等的教育，但刑事案件的數量依舊沒有減少

#把檔案的列分類
#把時間變成西元年






