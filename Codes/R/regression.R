packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl",
  "lfe",
  "stargazer"
)
pacman::p_load(packages, character.only = TRUE)

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

# Load admission information =================
admission_df <- read_csv(file.path(dropbox_dir, "Data/admission_data.csv"))

# Load weather information on exam days=================
weather_exam_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_on_exam_day.csv"))

# Load weather information (previous 10 days) =================
weather_pre10_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_pre10.csv"))

# Load weather information (Oct, Nov, Dec before the exams) =================
weather_pre_oct_dec_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_pre_oct_dec.csv"))

# Regression with average weather =====================

reg_df <- left_join(
  admission_df,
  weather_exam_df %>% 
    group_by(prefecture, exam_year) %>% 
    summarise_all(mean),
  by = c("prefecture", "year" = "exam_year")
  ) %>% 
  left_join(weather_pre10_df, by = c("prefecture", "year" = "exam_year")) %>% 
  left_join(weather_pre_oct_dec_df, by = c("prefecture", "year" = "exam_year")) %>% 
  mutate(
    temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    temp_cut = relevel(as.factor(temp_cut), ref = 3),
    daytime_snowfall_m = daytime_snowfall_cm / 100,
    daytime_cum_snow_m = daytime_cum_snow_cm / 100,
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

res_1 <- felm(
  admission_total_share ~ daytime_temperature_degree | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ temp_cut | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_3 <- felm(
  admission_total_share ~ daytime_temperature_degree + daytime_precipitation_mm + daytime_snowfall_m + daytime_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_m + daytime_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_5 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_m + factor(daytime_cum_snow_m > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4, res_5) %>% 
  stargazer(
    dep.var.labels = "Admission share (\\%)",
    covariate.labels = c(
      "Temperature (degree C)",
      "Temperature $\\le$ 0",
      "Temperature $>$ 0, $\\le$ 3",
      "Temperature $>$ 6, $\\le$ 9",
      "Temperature $>$ 9",
      "Rainfall (mm)",
      "Snowfall (m)",
      "Cumulated snow (m)",
      "Cumulated snow $>$ .10 m"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 5)),
      c("Year FE", rep("Yes", 5))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/main_reg.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )


# Regression with weather in the morning and during the exams =====================

res_1 <- felm(
  admission_total_share ~ temp_cut_morning | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ temp_cut_morning + morning_precipitation_mm + morning_snowfall_m + morning_cum_snow_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_total_share ~ temp_cut_exam | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_total_share ~ temp_cut_exam + exam_precipitation_mm + exam_snowfall_m + exam_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_5 <- felm(
  admission_total_share ~ 
    temp_cut_morning + morning_precipitation_mm + morning_snowfall_m + morning_cum_snow_m +
    temp_cut_exam + exam_precipitation_mm + exam_snowfall_m + exam_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4, res_5) %>% 
  stargazer(
    dep.var.labels = "Admission share (\\%)",
    column.labels = c(rep("Morning", 2), rep("During-exam", 2)),
    order = c(seq(1, 4), seq(8, 11), seq(5, 7), seq(12, 14)),
    covariate.labels = c(
      "Temperature (morning) $\\le$ -3",
      "Temperature (morning) $>$ -3, $\\le$ 0",
      "Temperature (morning) $>$ 3, $\\le$ 6",
      "Temperature (morning) $>$ 6",
      "Temperature (during-exam) $\\le$ 0",
      "Temperature (during-exam) $>$ 0, $\\le$ 3",
      "Temperature (during-exam) $>$ 3, $\\le$ 6",
      "Temperature (during-exam) $>$ 9",
      "Rainfall (mm) (morning)",
      "Snowfall (m) (morning)",
      "Cumulated snow (m) (morning)",
      "Rainfall (mm) (during-exam)",
      "Snowfall (m) (during-exam)",
      "Cumulated snow (m) (during-exam)"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 5)),
      c("Year FE", rep("Yes", 5))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/reg_morning_exam.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Regression with weather for the previous 10 days =====================

res_1 <- felm(
  admission_total_share ~ temperature_pre10_avg | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ temp_cut_pre10 | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_3 <- felm(
  admission_total_share ~ temperature_pre10_avg + precipitation_pre10_sum + snowfall_pre10 + cum_snow_pre10 | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_total_share ~ temp_cut_pre10 + precipitation_pre10_sum + snowfall_pre10 + cum_snow_pre10 | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Admission share (\\%)",
    covariate.labels = c(
      "Temperature (degree C)",
      "Temperature $\\le$ 0",
      "Temperature $>$ 0, $\\le$ 3",
      "Temperature $>$ 6, $\\le$ 9",
      "Temperature $>$ 9",
      "Rainfall (mm)",
      "Snowfall (m)",
      "Cumulated snow (m)"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 5)),
      c("Year FE", rep("Yes", 5))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/reg_pre10.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Regression with weather on two days =====================

weather_df %>% 
  dplyr::select(starts_with("daytime_"), exam_first_second, prefecture, exam_year) %>% 
  pivot_wider(names_from = exam_first_second, values_from = starts_with("daytime_"))

reg_df <- left_join(
  admission_df,
  weather_df %>% 
    dplyr::select(starts_with("daytime_"), exam_first_second, prefecture, exam_year) %>% 
    pivot_wider(names_from = exam_first_second, values_from = starts_with("daytime_")),
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
    daytime_cum_snow_m_2 = daytime_cum_snow_cm_2 / 100
    )

res_1 <- felm(
  admission_total_share ~ daytime_temperature_degree_1 + daytime_temperature_degree_2 | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ temp_cut_1 + temp_cut_2 | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_3 <- felm(
  admission_total_share ~ 
    daytime_temperature_degree_1 + daytime_precipitation_mm_1 + daytime_snowfall_m_1 + daytime_cum_snow_m_1 +
    daytime_temperature_degree_2 + daytime_precipitation_mm_2 + daytime_snowfall_m_2 + daytime_cum_snow_m_2
    | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_total_share ~
    temp_cut_1 + daytime_precipitation_mm_1 + daytime_snowfall_m_1 + daytime_cum_snow_m_1 +
    temp_cut_2 + daytime_precipitation_mm_2 + daytime_snowfall_m_2 + daytime_cum_snow_m_2 |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)




felm(
  admission_total_share ~ temp_cut + temperature_daily_average_10 + temperature_daily_average_11 + temperature_daily_average_12 | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  ) %>% 
  summary()

felm(
  admission_total_share ~ temp_cut + daytime_cum_snow_m + snowfall_sum_avg |
    prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    mutate(
      temp_avg_cut = cut(temperature_daily_average_10, c(-2, 14, 17, 20, 30)),
      temp_avgcut = relevel(as.factor(temp_avg_cut), ref = 3)
    )
  ) %>% 
  summary()

felm(
  admission_total_share ~ temp_cut + temp_avg_cut |
    prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    mutate(
      temp_avg_cut = cut(temperature_daily_average_12, c(-5, 3, 6, 9, 30)),
      temp_avgcut = relevel(as.factor(temp_avg_cut), ref = 3)
    )
  ) %>% 
  summary()


felm(
  admission_total_share ~ morning_temperature_degree + exam_temperature_degree | 
    prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    mutate(
      temp_cut_morning = cut(morning_temperature_degree, c(-10, 0, 3, 6, 9, 30)),
      temp_cut_exam = cut(exam_temperature_degree, c(-10, 0, 3, 6, 9, 30))
      )
  ) %>% 
  summary()

felm(
  # admission_total_share ~ relevel(factor(temp_cut_morning), ref = 3) + relevel(factor(temp_cut_exam), ref = 4) |
  admission_total_share ~ relevel(factor(temp_cut_morning), ref = 3) |
  # admission_total_share ~ relevel(factor(temp_cut_exam), ref = 4) |
    prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    mutate(
      temp_cut_morning = cut(morning_temperature_degree, c(-10, -3, 0, 3, 6, 30)),
      temp_cut_exam = cut(exam_temperature_degree, c(-10, 0, 3, 6, 9, 30))
      )
  ) %>% 
  summary()

reg_df %>% 
  ggplot(aes(x = morning_temperature_degree)) +
  geom_histogram()

reg_df %>% 
    mutate(
      temp_cut_morning = cut(morning_temperature_degree, c(-10, -3, 0, 2, 4, 6, 30)),
      temp_cut_exam = cut(exam_temperature_degree, c(-10, 0, 3, 6, 9, 30))
      ) %>% 
  .$temp_cut_exam %>% 
  table()

reg_df %>% 
  select(exam_temperature_degree, morning_temperature_degree) %>% 
  summary()
