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

# Load regression data =================
reg_df <- read_csv(file.path(dropbox_dir, "Data/temp/data_regression.csv"))

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

felm(
  admission_total_share ~ 
    temp_cut + daytime_precipitation_mm + daytime_cum_snow_m +
    # l1_temp_cut + l1_precipitation_mm + l1_cum_snow_m +
    # f1_temp_cut + f1_precipitation_mm + f1_cum_snow_m | 
    temp_cut_pre10 + precipitation_pre10_sum + cum_snow_pre10_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
  ) %>% 
  summary()


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



