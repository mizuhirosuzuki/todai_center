# Merge data for regressions

# Load admission information =================
admission_df <- read_csv(file.path(dropbox_dir, "Data/Processed/admission_data.csv"))

# Load weather information on exam days =================
weather_exam_df <- read_csv(file.path(dropbox_dir, "Data/Processed/weather_on_exam_day.csv"))

# Load weather information on exam days (1 year lead) =================
weather_f1_df <- read_csv(file.path(dropbox_dir, "Data/Processed/weather_f1.csv"))

# Load weather information (previous 10 days) =================
weather_pre10_df <- read_csv(file.path(dropbox_dir, "Data/Processed/weather_pre10.csv"))

# Regression with average weather =====================

reg_df <- left_join(
  admission_df,
  weather_exam_df %>% 
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
  left_join(weather_pre10_df, by = c("prefecture", "year" = "exam_year")) %>% 
  mutate(
    temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    temp_cut = relevel(as.factor(temp_cut), ref = 3),
    daytime_snowfall_m = daytime_snowfall_cm / 100,
    daytime_cum_snow_m = daytime_cum_snow_cm / 100,
    f1_temp_cut = cut(f1_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    f1_temp_cut = relevel(as.factor(f1_temp_cut), ref = 3),
    f1_snowfall_m = f1_snowfall_cm / 100,
    f1_cum_snow_m = f1_cum_snow_cm / 100,
    temp_cut_pre10 = cut(temperature_pre10_avg, c(-10, 0, 3, 6, 9, 30)),
    temp_cut_pre10 = relevel(as.factor(temp_cut_pre10), ref = 3),
    snowfall_pre10_m = snowfall_pre10 / 100,
    cum_snow_pre10_m = cum_snow_pre10 / 100,
    )

saveRDS(
  reg_df, 
  file.path(dropbox_dir, "Data/Processed/", "data_regression.rds")
  )