packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl"
)
pacman::p_load(packages, character.only = TRUE)

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

# Load admission information =================

load_admission_df <- function(file_path, year) {
  
  print(year)
  
  if (year <= 2015) {
    output_df <- read_excel(file_path, skip = 2) %>% 
      rename(
        univ = 大学名,
        department = 学部名,
        gender = 県別入学者数
      )
  } else {
    output_df <- read_excel(file_path, skip = 2) %>% 
      rename(
        univ = 学校名,
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
    # group_by(department) %>% 
    # mutate(
    #   across(
    #     .cols = starts_with("admission"),
    #     .fns = ~ . / sum(admission_total),
    #     .names = "{col}_share"
    #   )
    # ) %>% 
    # ungroup() %>% 
    pivot_wider(names_from = department, values_from = starts_with("admission_")) %>% 
    mutate(
      admission_male_share = (admission_male_Arts + admission_male_Sciences) / (sum(admission_total_Arts) + sum(admission_total_Sciences)),
      admission_female_share = (admission_female_Arts + admission_female_Sciences) / (sum(admission_total_Arts) + sum(admission_total_Sciences)),
      admission_Arts_share = (admission_male_Arts + admission_female_Arts) / sum(admission_total_Arts),
      admission_Sciences_share = (admission_male_Sciences + admission_female_Sciences) / sum(admission_total_Sciences),
      admission_total_share = (admission_total_Arts + admission_total_Sciences) / (sum(admission_total_Arts) + sum(admission_total_Sciences))
    ) %>% 
    dplyr::select(- c(
      admission_male_Arts, admission_male_Sciences, 
      admission_female_Arts, admission_female_Sciences, 
      admission_total_Arts, admission_total_Sciences
      )) %>% 
    filter(prefecture != "その他") %>% 
    mutate(
      across(
        .cols = starts_with("admission_"),
        .fns = ~ . * 100
      )
    ) %>% 
    mutate(year = year)
   
}
    
admission_df_all <- bind_rows(
  load_admission_df(
    file.path(dropbox_dir, "Data/admission_data/2012_08go_G.xls"),
    2012
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/admission_data/2013_08go_G.xls"),
    2013
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/admission_data/2014_08go_G.xls"),
    2014
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/admission_data/2015_08go_G.xls"),
    2015
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/admission_data/2016_08go_G.xls"),
    2016
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/admission_data/2017_08go_G.xlsx"),
    2017
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/admission_data/2018_08go_G.xlsx"),
    2018
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/admission_data/2019_08go_G.xlsx"),
    2019
  ),
  load_admission_df(
    file.path(dropbox_dir, "Data/admission_data/2020_08go_G.xlsx"),
    2020
  )
)

write_csv(admission_df_all, file.path(dropbox_dir, "Data/admission_data.csv"))

