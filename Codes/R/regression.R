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

reg_df %>% 
  mutate(
    # temp_cut = cut(daytime_temperature_degree, c(-10, 1, 3, 5, 7, 25)),
    temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    temp_cut_morning = cut(morning_temperature_degree, c(-10, -2, 0, 2, 4, 6, 25)),
    temp_cut_exam = cut(exam_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    l1_temp_cut = cut(l1_temperature_degree, c(-10, 0, 2, 4, 6, 8, 25)),
    f1_temp_cut = cut(f1_temperature_degree, c(-10, 0, 2, 4, 6, 8, 25))
    # temp_cut = relevel(as.factor(temp_cut), ref = 3),
    ) %>%
  # filter(year <= 2018) %>%
  # select(year, temp_cut) %>%
  # select(temp_cut) %>%
  # select(year, f1_temp_cut) %>%
  # select(f1_temp_cut) %>%
  # select(year, l1_temp_cut) %>%
  # select(l1_temp_cut) %>%
  # select(year, temp_cut_morning) %>%
  # select(temp_cut_morning) %>%
  select(year, temp_cut_exam) %>%
  select(temp_cut_exam) %>%
  table()

felm(
  admission_total_share ~ 
    temp_cut |
    prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    mutate(
      temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
      temp_cut = cut(daytime_temperature_degree, c(-10, 0, 2, 4, 6, 8, 25)),
      temp_cut = relevel(temp_cut, ref = 3),
      temp_cut_morning = cut(morning_temperature_degree, c(-10, -2, 0, 2, 4, 6, 25)),
      temp_cut_exam = cut(exam_temperature_degree, c(-10, 0, 2, 4, 6, 25)),
    )
  ) %>% summary

felm(
  admission_total_share ~ 
    temp_cut |
    prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    mutate(
      # temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
      temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
      temp_cut_morning = cut(morning_temperature_degree, c(-10, -3, 0, 3, 6, 25)),
      temp_cut_exam = cut(exam_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    )
  ) %>% 
  car::linearHypothesis(
    c(
      rownames(.$coefficients)[str_detect(rownames(.$coefficients), "temp_cut")]
    ),
    test = "F"
  )

a <- lm(admission_total_share ~ factor(prefecture) + factor(year), data = reg_df)
b <- lm(daytime_temperature_degree ~ factor(prefecture) + factor(year), data = reg_df)
reg_df %>% 
  mutate(
    resid_y = (lm(admission_total_share ~ factor(prefecture) + factor(year), data = reg_df))$residuals, 
    resid_x = (lm(daytime_temperature_degree ~ factor(prefecture) + factor(year), data = reg_df))$residuals
    ) %>% 
  ggplot(aes(x = resid_x, y = resid_y)) +
  geom_smooth()

res

car::linearHypothesis(
  res,
  rownames(res$coefficients)[str_detect(rownames(res$coefficients), "temp_cut")]
)


# Main regression ==========================

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
  admission_total_share ~ daytime_temperature_degree + daytime_precipitation_mm + daytime_snowfall_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_5 <- felm(
  admission_total_share ~ daytime_temperature_degree + daytime_precipitation_mm + daytime_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_6 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm + daytime_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_7 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm + factor(daytime_cum_snow_m > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4, res_5, res_6, res_7) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
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
      c("Prefecture FE", rep("Yes", 7)),
      c("Year FE", rep("Yes", 7))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/main_reg.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )


# Regression with weather in the morning and during the exams =====================

res_1 <- felm(
  admission_total_share ~ temp_cut_morning + morning_precipitation_mm + morning_snowfall_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ temp_cut_morning + morning_precipitation_mm + morning_cum_snow_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_total_share ~ temp_cut_exam + exam_precipitation_mm + exam_snowfall_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_total_share ~ temp_cut_exam + exam_precipitation_mm + exam_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_5 <- felm(
  admission_total_share ~ 
    temp_cut_morning + morning_precipitation_mm + morning_snowfall_m +
    temp_cut_exam + exam_precipitation_mm + exam_snowfall_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_6 <- felm(
  admission_total_share ~ 
    temp_cut_morning + morning_precipitation_mm + morning_cum_snow_m +
    temp_cut_exam + exam_precipitation_mm + exam_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4, res_5, res_6) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    # column.labels = c(rep("Morning", 2), rep("During-exam", 2)),
    order = c(seq(1, 4), seq(8, 11), seq(5, 7), seq(12, 14)),
    covariate.labels = c(
      "Temperature (morning) $\\le$ -3",
      "Temperature (morning) $>$ -3, $\\le$ 0",
      "Temperature (morning) $>$ 3, $\\le$ 6",
      "Temperature (morning) $>$ 6",
      "Temperature (during-exam) $\\le$ 0",
      "Temperature (during-exam) $>$ 0, $\\le$ 3",
      "Temperature (during-exam) $>$ 6, $\\le$ 9",
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
      c("Prefecture FE", rep("Yes", 6)),
      c("Year FE", rep("Yes", 6))
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
  admission_total_share ~ temperature_pre10_avg + precipitation_pre10_sum + snowfall_pre10_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_total_share ~ temp_cut_pre10 + precipitation_pre10_sum + snowfall_pre10_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_5 <- felm(
  admission_total_share ~ temperature_pre10_avg + precipitation_pre10_sum + cum_snow_pre10_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_6 <- felm(
  admission_total_share ~ temp_cut_pre10 + precipitation_pre10_sum + cum_snow_pre10_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

reg_df %>% 
  select(daytime_temperature_degree, temperature_pre10_avg) %>% 
  cor()

list(res_1, res_2, res_3, res_4, res_5, res_6) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
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
      c("Prefecture FE", rep("Yes", 6)),
      c("Year FE", rep("Yes", 6))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/reg_pre10.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

reg_df$cum_snow_pre10_m %>% sd
0.12 * 0.19

# Regression by male and female matriculations =====================

res_1 <- felm(
  admission_female_share ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_female_share ~ temp_cut + daytime_precipitation_mm + daytime_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_male_share ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_male_share ~ temp_cut + daytime_precipitation_mm + daytime_cum_snow_m | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = c("Matriculation share (\\%, female)", "Matriculation share (\\%, male)"),
    covariate.labels = c(
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
      c("Prefecture FE", rep("Yes", 4)),
      c("Year FE", rep("Yes", 4))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/reg_by_gender.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Regression with weather (lag and lead) =====================

res_1 <- felm(
  admission_total_share ~ f1_temp_cut + f1_precipitation_mm + f1_snowfall_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ f1_temp_cut + f1_precipitation_mm + f1_cum_snow_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_total_share ~ 
    temp_cut + daytime_precipitation_mm + daytime_snowfall_m +
    f1_temp_cut + f1_precipitation_mm + f1_snowfall_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_4 <- felm(
  admission_total_share ~ 
    temp_cut + daytime_precipitation_mm + daytime_cum_snow_m +
    f1_temp_cut + f1_precipitation_mm + f1_cum_snow_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    order = c(seq(1, 4), seq(5, 7), seq(8, 11), seq(12, 14)),
    covariate.labels = c(
      "Temperature ($t$) $\\le$ 0",
      "Temperature ($t$) $>$ 0, $\\le$ 3",
      "Temperature ($t$) $>$ 6, $\\le$ 9",
      "Temperature ($t$) $>$ 9",
      "Rainfall (mm) ($t$)",
      "Snowfall (m) ($t$)",
      "Cumulated snow (m) ($t$)",
      "Temperature ($t + 1$) $\\le$ 0",
      "Temperature ($t + 1$) $>$ 0, $\\le$ 3",
      "Temperature ($t + 1$) $>$ 6, $\\le$ 9",
      "Temperature ($t + 1$) $>$ 9",
      "Rainfall (mm) ($t + 1$)",
      "Snowfall (m) ($t + 1$)",
      "Cumulated snow (m) ($t + 1$)"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 5)),
      c("Year FE", rep("Yes", 5))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/reg_placebo_exam.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Regression with weather on two days =====================

reg_df_td <- left_join(
  admission_df,
  weather_exam_df %>% 
    dplyr::select(starts_with("daytime_"), starts_with("morning_"), exam_first_second, prefecture, exam_year) %>% 
    pivot_wider(names_from = exam_first_second, values_from = c(starts_with("daytime_"), starts_with("morning_"))),
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
  data = reg_df_td
  )

res_2 <- felm(
  admission_total_share ~ temp_cut_1 + temp_cut_2 | 
    prefecture + year | 0 | prefecture, 
  data = reg_df_td
  )

res_3 <- felm(
  admission_total_share ~ 
    daytime_temperature_degree_1 + daytime_precipitation_mm_1 + daytime_snowfall_m_1 + daytime_cum_snow_m_1 +
    daytime_temperature_degree_2 + daytime_precipitation_mm_2 + daytime_snowfall_m_2 + daytime_cum_snow_m_2
    | 
    prefecture + year | 0 | prefecture, 
  data = reg_df_td
  )

res_4 <- felm(
  admission_total_share ~
    temp_cut_1 + daytime_precipitation_mm_1 + daytime_snowfall_m_1 + daytime_cum_snow_m_1 +
    temp_cut_2 + daytime_precipitation_mm_2 + daytime_snowfall_m_2 + daytime_cum_snow_m_2 |
    prefecture + year | 0 | prefecture, 
  data = reg_df_td
)



