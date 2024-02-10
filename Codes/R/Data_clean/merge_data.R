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
    select(-c(exam_month, exam_day, exam_first_second)) %>% 
    summarise_all(mean),
  by = c("prefecture", "year" = "exam_year")
  ) %>% 
  left_join(
    weather_f1_df %>% 
      group_by(prefecture, exam_year) %>% 
      select(-c(exam_month, exam_day, exam_first_second)) %>% 
      summarise_all(mean),
    by = c("prefecture", "year" = "exam_year")
  ) %>% 
  left_join(weather_pre10_df, by = c("prefecture", "year" = "exam_year")) %>% 
  mutate(
    temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    temp_cut = relevel(as.factor(temp_cut), ref = 3),
    daytime_snowfall_m = daytime_snowfall_cm / 100,
    daytime_cum_snow_m = daytime_cum_snow_cm / 100,
    morning_temp_cut = cut(morning_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    morning_temp_cut = relevel(as.factor(morning_temp_cut), ref = 3),
    morning_snowfall_m = morning_snowfall_cm / 100,
    morning_cum_snow_m = morning_cum_snow_cm / 100,
    exam_temp_cut = cut(exam_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    exam_temp_cut = relevel(as.factor(exam_temp_cut), ref = 3),
    exam_snowfall_m = exam_snowfall_cm / 100,
    exam_cum_snow_m = exam_cum_snow_cm / 100,
    f1_temp_cut = cut(f1_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    f1_temp_cut = relevel(as.factor(f1_temp_cut), ref = 3),
    f1_snowfall_m = f1_snowfall_cm / 100,
    f1_cum_snow_m = f1_cum_snow_cm / 100,
    temp_cut_pre10 = cut(temperature_pre10_avg, c(-10, 0, 3, 6, 9, 30)),
    temp_cut_pre10 = relevel(as.factor(temp_cut_pre10), ref = 3),
    snowfall_pre10_m = snowfall_pre10 / 100,
    cum_snow_pre10_m = cum_snow_pre10 / 100,
    )

reg_df <- reg_df %>% 
  left_join(
    weather_exam_df %>% 
      select(-c(exam_month, exam_day)) %>% 
      pivot_wider(
        id_cols = c(prefecture, exam_year),
        names_from = exam_first_second,
        values_from = c(daytime_temperature_degree, daytime_precipitation_mm, daytime_snowfall_cm, daytime_cum_snow_cm)
        ),
  by = c("prefecture", "year" = "exam_year")
  ) %>% 
  mutate(
    temp_cut_1 = cut(daytime_temperature_degree_1, c(-10, 0, 3, 6, 9, 25)),
    temp_cut_1 = relevel(as.factor(temp_cut_1), ref = 3),
    temp_cut_2 = cut(daytime_temperature_degree_2, c(-10, 0, 3, 6, 9, 25)),
    temp_cut_2 = relevel(as.factor(temp_cut_2), ref = 3),
    daytime_snowfall_m_1 = daytime_snowfall_cm_1 / 100,
    daytime_cum_snow_m_1 = daytime_cum_snow_cm_1 / 100,
    daytime_snowfall_m_2 = daytime_snowfall_cm_2 / 100,
    daytime_cum_snow_m_2 = daytime_cum_snow_cm_2 / 100,
    )

saveRDS(
  reg_df, 
  file.path(dropbox_dir, "Data/Processed/", "data_regression.rds")
  )


