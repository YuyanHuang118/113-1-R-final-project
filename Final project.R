#教育是否會使刑事犯罪率降低
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

# 篩選包含特定文字的行
filtered_data <- data |>
  dplyr::filter(apply(across(everything(), as.character), 1, function(row) {
    any(stringr::str_detect(row, "111年/ 性別總計/ 機關別總計"))
  }))

# 查看結果
filtered_data

colnames(data)


data_with_ratios <- data |>
  mutate(
    illiterate_ratio = `illiterate` / `total`,
    self_taught_ratio = `self_taught` / `total`,
    elementary_school_ratio = `elementary_school` / `total`,
    junior_high_school_ratio = `junior_high_school` / `total`,
    high_school_vocational_ratio = `high_school_vocational` / `total`,
    college_ratio = `college` / `total`,
    graduate_school_ratio = `graduate_school` / `total`,
    unknown_ratio = `unknown` / `total`
  )

# 查看結果
filtered_data

#把檔案的列分類
#把時間變成西元年

#請幫我把此檔案＂c0620202-2134091124.csv"中的"111年/ 男/ 機關別總計"那一列單獨列出來
#重新計算每個欄位相對於"total"的比例，並以百分比呈現
#嫌疑犯受教育程度的前三名分別為高中（職）再來是國中接著是大學

#請幫我把此檔案＂c0620202-2134091124.csv"中的"111年/ 女/ 機關別總計"那一列單獨列出來
#重新計算每個欄位相對於"total"的比例，並以百分比呈現
#嫌疑犯受教育程度的前三名分別為高中（職）再來是大學接著是國中

#請幫我對照"111年/ 男/ 機關別總計"那一列中的"total"與"111年/ 女/ 機關別總計"那一列中的"total"
#算出男性是女性的幾倍
#男生的犯罪率是女生的X倍，這可能也體現出了男性與女性在個體或個性上的差異


#大部分的刑事案件嫌疑者為高中（職）生，而大學跟國中生的比例是差不多的，
#表示即使普遍的人皆受到了相對高等的教育，但刑事案件的數量依舊沒有減少


