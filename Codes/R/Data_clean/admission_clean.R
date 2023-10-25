# Load admission information =================
read_excel(file.path(dropbox_dir, "Data/Raw/admission_data/2021_08go_G.xlsx"), skip=2)

load_admission_df <- function(file_path, year) {
  
  print(year)
  
  if (year <= 2015) {
    output_df <- read_excel(file_path, skip = 2) %>% 
      rename(
        univ = 大学名,
        department = 学部名,
        gender = 県別入学者数
      )
  } else if (year <= 2020) {
    output_df <- read_excel(file_path, skip = 2) %>% 
      rename(
        univ = 学校名,
        department = 学部名,
        gender = 性別
      )
  } else {
    output_df <- read_excel(file_path, skip = 2) %>% 
      rename(
        univ = 大学名,
        department = 学部名,
        gender = 性別
      )
  }
  
  output_df <- output_df %>% 
    filter(univ == "東京大学") %>% 
    filter(str_detect(department, "文科系|理科系")) %>% 
    dplyr::select(univ, department, gender, 北海道:その他) %>% 
    mutate(
      across(
        .cols = is.numeric,
        .fns = ~ replace_na(., 0)
      )
    ) %>% 
    pivot_longer(cols = 北海道:その他, names_to = "prefecture",  values_to = "admission") %>% 
    mutate(
      department = str_extract(department, "（(.)*）") %>% str_replace_all("[（）]", ""),
      department = ifelse(department == "文科系", "Arts", "Sciences"),
      gender = ifelse(gender == "男", "male", "female")
    ) %>% 
    pivot_wider(names_from = gender, values_from = admission, names_prefix = "admission_") %>% 
    mutate(admission_total = admission_male + admission_female) %>% 
    pivot_wider(names_from = department, values_from = starts_with("admission_")) %>% 
    mutate(
      admission_male_share = (admission_male_Arts + admission_male_Sciences) / (sum(admission_total_Arts) + sum(admission_total_Sciences)),
      admission_female_share = (admission_female_Arts + admission_female_Sciences) / (sum(admission_total_Arts) + sum(admission_total_Sciences)),
      admission_Arts_share = (admission_male_Arts + admission_female_Arts) / sum(admission_total_Arts),
      admission_Sciences_share = (admission_male_Sciences + admission_female_Sciences) / sum(admission_total_Sciences),
      admission_total_share = (admission_total_Arts + admission_total_Sciences) / (sum(admission_total_Arts) + sum(admission_total_Sciences))
    ) %>% 
    filter(prefecture != "その他") %>% 
    mutate(
      across(
        .cols = (starts_with("admission_") & ends_with("_share")),
        .fns = ~ . * 100
      )
    ) %>% 
    mutate(year = year)
   
}
    
admission_df_all <- bind_rows(
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2012_08go_G.xls"),
    2012
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2013_08go_G.xls"),
    2013
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2014_08go_G.xls"),
    2014
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2015_08go_G.xls"),
    2015
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2016_08go_G.xls"),
    2016
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2017_08go_G.xlsx"),
    2017
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2018_08go_G.xlsx"),
    2018
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2019_08go_G.xlsx"),
    2019
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2020_08go_G.xlsx"),
    2020
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2021_08go_G.xlsx"),
    2021
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/Raw/admission_data/2022_08go_G.xlsx"),
    2022
  )
)

admission_df_all <- admission_df_all %>%
  mutate(prefecture_eng = case_when(
    prefecture == "北海道" ~ "Hokkaido",
    prefecture == "青森" ~ "Aomori",
    prefecture == "岩手" ~ "Iwate",
    prefecture == "宮城" ~ "Miyagi",
    prefecture == "秋田" ~ "Akita",
    prefecture == "山形" ~ "Yamagata",
    prefecture == "福島" ~ "Fukushima",
    prefecture == "茨城" ~ "Ibaraki",
    prefecture == "栃木" ~ "Tochigi",
    prefecture == "群馬" ~ "Gunma",
    prefecture == "埼玉" ~ "Saitama",
    prefecture == "千葉" ~ "Chiba", 
    prefecture == "東京" ~ "Tokyo",
    prefecture == "神奈川" ~ "Kanagawa",
    prefecture == "新潟" ~ "Niigata",
    prefecture == "富山" ~ "Toyama",
    prefecture == "石川" ~ "Ishikawa",
    prefecture == "福井" ~ "Fukui",
    prefecture == "山梨" ~ "Yamanashi",
    prefecture == "長野" ~ "Nagano",
    prefecture == "岐阜" ~ "Gifu",
    prefecture == "静岡" ~ "Shizuoka",
    prefecture == "愛知" ~ "Aichi",
    prefecture == "三重" ~ "Mie",
    prefecture == "滋賀" ~ "Shiga",
    prefecture == "京都" ~ "Kyoto",
    prefecture == "大阪" ~ "Osaka",
    prefecture == "兵庫" ~ "Hyogo",
    prefecture == "奈良" ~ "Nara",
    prefecture == "和歌山" ~ "Wakayama",
    prefecture == "鳥取" ~ "Tottori",
    prefecture == "島根" ~ "Shimane",
    prefecture == "岡山" ~ "Okayama",
    prefecture == "広島" ~ "Hiroshima",
    prefecture == "山口" ~ "Yamaguchi",
    prefecture == "徳島" ~ "Tokushima",
    prefecture == "香川" ~ "Kagawa",
    prefecture == "愛媛" ~ "Ehime",
    prefecture == "高知" ~ "Kochi",
    prefecture == "福岡" ~ "Fukuoka",
    prefecture == "佐賀" ~ "Saga",
    prefecture == "長崎" ~ "Nagasaki",
    prefecture == "熊本" ~ "Kumamoto",
    prefecture == "大分" ~ "Oita",
    prefecture == "宮崎" ~ "Miyazaki",
    prefecture == "鹿児島" ~ "Kagoshima",
    prefecture == "沖縄" ~ "Okinawa"
  )) 

write_csv(admission_df_all, file.path(dropbox_dir, "Data/Processed/admission_data.csv"))
