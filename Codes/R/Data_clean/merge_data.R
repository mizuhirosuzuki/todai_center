# Merge data for regressions

packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl",
  "lfe",
  "stargazer",
  "RStata"
)
pacman::p_load(packages, character.only = TRUE)

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

# Load admission information =================
admission_df <- read_csv(file.path(dropbox_dir, "Data/admission_data.csv"))

# Load weather information on exam days =================
weather_exam_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_on_exam_day.csv"))

# Load weather information on exam days (1 year lag) =================
weather_l1_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_l1.csv"))

# Load weather information on exam days (1 year lead) =================
weather_f1_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_f1.csv"))

# Load weather information (previous 10 days) =================
weather_pre10_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_pre10.csv"))

# Load weather information (Oct, Nov, Dec before the exams) =================
weather_pre_oct_dec_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_pre_oct_dec.csv"))

# Load weather information (Previous summer from May to September) =================
weather_summer_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_summer.csv"))

# Regression with average weather =====================

reg_df <- left_join(
  admission_df,
  weather_exam_df %>% 
    group_by(prefecture, exam_year) %>% 
    summarise_all(mean),
  by = c("prefecture", "year" = "exam_year")
  ) %>% 
  left_join(
    weather_l1_df %>% 
      group_by(prefecture, exam_year) %>% 
      summarise_all(mean),
    by = c("prefecture", "year" = "exam_year")
  ) %>% 
  left_join(
    weather_f1_df %>% 
      group_by(prefecture, exam_year) %>% 
      summarise_all(mean),
    by = c("prefecture", "year" = "exam_year")
  ) %>% 
  left_join(
    weather_summer_df,
    by = c("prefecture", "year")
  ) %>% 
  left_join(weather_pre10_df, by = c("prefecture", "year" = "exam_year")) %>% 
  left_join(weather_pre_oct_dec_df, by = c("prefecture", "year" = "exam_year")) %>% 
  mutate(
    temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    temp_cut = relevel(as.factor(temp_cut), ref = 3),
    daytime_snowfall_m = daytime_snowfall_cm / 100,
    daytime_cum_snow_m = daytime_cum_snow_cm / 100,
    l1_temp_cut = cut(l1_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    l1_temp_cut = relevel(as.factor(l1_temp_cut), ref = 3),
    l1_snowfall_m = l1_snowfall_cm / 100,
    l1_cum_snow_m = l1_cum_snow_cm / 100,
    f1_temp_cut = cut(f1_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    f1_temp_cut = relevel(as.factor(f1_temp_cut), ref = 3),
    f1_snowfall_m = f1_snowfall_cm / 100,
    f1_cum_snow_m = f1_cum_snow_cm / 100,
    temp_cut_pre10 = cut(temperature_pre10_avg, c(-10, 0, 3, 6, 9, 30)),
    temp_cut_pre10 = relevel(as.factor(temp_cut_pre10), ref = 3),
    snowfall_pre10_m = snowfall_pre10 / 100,
    cum_snow_pre10_m = cum_snow_pre10 / 100,
    temp_cut_morning = cut(morning_temperature_degree, c(-10, -3, 0, 3, 6, 30)),
    temp_cut_morning = relevel(temp_cut_morning, ref = 3),
    temp_cut_exam = cut(exam_temperature_degree, c(-10, 0, 3, 6, 9, 30)),
    temp_cut_exam = relevel(temp_cut_exam, ref = 4),
    exam_snowfall_m = exam_snowfall_cm / 100,
    exam_cum_snow_m = exam_cum_snow_cm / 100,
    morning_snowfall_m = morning_snowfall_cm / 100,
    morning_cum_snow_m = morning_cum_snow_cm / 100
    )

saveRDS(
  reg_df, 
  file.path(dropbox_dir, "Data/temp/", "data_regression.rds")
  )
