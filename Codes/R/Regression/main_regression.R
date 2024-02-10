# Main regression
# Table 2 -------------

res_1 <- felm(
  admission_total_share ~ temp_cut |
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm + daytime_cum_snow_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_4 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm + factor(daytime_cum_snow_m > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ 0",
      "Temperature (\\degree C) 0-3",
      "Temperature (\\degree C) 6-9",
      "Temperature (\\degree C) $>$ 9",
      "Rainfall (mm)",
      "Snowfall (cm)",
      "Cumulated snow (cm)",
      "Cumulated snow $>$ 10 cm"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 8)),
      c("Year FE", rep("Yes", 8))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/main_reg.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Regression table for beamer
list(res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    keep = c(
      "^[^temp_cut]"
    ),
    covariate.labels = c(
      "Rainfall (mm)",
      "Snowfall (cm)",
      "Cumulated snow (cm)",
      "Cumulated snow $>$ 10 cm"
    ),
    title = "",
    add.lines = list(
      c("Temperature bins", rep("Yes", 3)),
      c("Prefecture FE", rep("Yes", 3)),
      c("Year FE", rep("Yes", 3))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/main_reg_beamer.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Table 3 -------------
# Regression with weather for the previous 10 days =====================

res_1 <- felm(
  admission_total_share ~ temp_cut_pre10 + precipitation_pre10_sum + snowfall_pre10 |
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ temp_cut_pre10 + precipitation_pre10_sum + cum_snow_pre10 |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_total_share ~ 
    temp_cut + daytime_precipitation_mm + daytime_snowfall_cm +
    temp_cut_pre10 + precipitation_pre10_sum + snowfall_pre10 |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_4 <- felm(
  admission_total_share ~ 
    temp_cut + daytime_precipitation_mm + daytime_cum_snow_cm +
    temp_cut_pre10 + precipitation_pre10_sum + cum_snow_pre10 |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    order = c(seq(1, 4), seq(5, 7), seq(8, 11), seq(12, 14)),
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ 0 (exam dates)",
      "Temperature (\\degree C) 0-3 (exam dates)",
      "Temperature (\\degree C) 6-9 (exam dates)",
      "Temperature (\\degree C) $>$ 9 (exam dates)",
      "Rainfall (mm) (exam dates)",
      "Snowfall (cm) (exam dates)",
      "Cumulated snow (cm) (exam dates)",
      "Temperature (\\degree C) $\\le$ 0 (previous 10 days)",
      "Temperature (\\degree C) 0-3 (previous 10 days)",
      "Temperature (\\degree C) 6-9 (previous 10 days)",
      "Temperature (\\degree C) $>$ 9 (previous 10 days)",
      "Rainfall (mm) (previous 10 days)",
      "Snowfall (cm) (previous 10 days)",
      "Cumulated snow (cm) (previous 10 days)"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 4)),
      c("Year FE", rep("Yes", 4))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/reg_pre10.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Regression table for beamer
list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    omit = c(
      "^(temp_cut)"
    ),
    covariate.labels = c(
      "Rainfall (mm) (exam dates)",
      "Snowfall (cm) (exam dates)",
      "Cumulated snow (cm) (exam dates)",
      "Rainfall (mm) (previous 10 days)",
      "Snowfall (cm) (previous 10 days)",
      "Cumulated snow (cm) (previous 10 days)"
    ),
    title = "",
    add.lines = list(
      c("Temperature bins", rep("Yes", 4)),
      c("Prefecture FE", rep("Yes", 4)),
      c("Year FE", rep("Yes", 4))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/reg_pre10_beamer.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# ===========================

# day1 vs day2
res1 <- felm(
  admission_total_share ~  temp_cut_1 + daytime_precipitation_mm_1 + factor(daytime_cum_snow_m_1 > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res2 <- felm(
  admission_total_share ~  temp_cut_2 + daytime_precipitation_mm_2 + factor(daytime_cum_snow_m_2 > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res3 <- felm(
  admission_total_share ~ 
    temp_cut_1 + temp_cut_2 + 
    daytime_precipitation_mm_1 + daytime_precipitation_mm_2 + 
    factor(daytime_cum_snow_m_1 > .1) + factor(daytime_cum_snow_m_2 > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res1, res2, res3) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ 0 (1st day)",
      "Temperature (\\degree C) 0-3 (1st day)",
      "Temperature (\\degree C) 6-9 (1st day)",
      "Temperature (\\degree C) $>$ 9 (1st day)",
      "Rainfall (mm) (1st day)",
      "Cumulated snow $>$ 10 cm (1st day)",
      "Temperature (\\degree C) $\\le$ 0 (2nd day)",
      "Temperature (\\degree C) 0-3 (2nd day)",
      "Temperature (\\degree C) 6-9 (2nd day)",
      "Temperature (\\degree C) $>$ 9 (2nd day)",
      "Rainfall (mm) (2nd day)",
      "Cumulated snow $>$ 10 cm (2nd day)"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 8)),
      c("Year FE", rep("Yes", 8))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/first_second_day_reg.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

reg_df %>%
  filter(year <= 2020) %>% 
  select(daytime_cum_snow_m_1, daytime_cum_snow_m_2) %>% 
  cor


# morning vs test time

res1 <- felm(
  admission_total_share ~ morning_temp_cut + morning_precipitation_mm + factor(morning_cum_snow_m > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res2 <- felm(
  admission_total_share ~ exam_temp_cut + exam_precipitation_mm + factor(exam_cum_snow_m > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res3 <- felm(
  admission_total_share ~ 
    morning_temp_cut + morning_precipitation_mm + factor(morning_cum_snow_m > .1) +
    exam_temp_cut + exam_precipitation_mm + factor(exam_cum_snow_m > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

reg_df %>% 
  filter(year <= 2020) %>% 
  select(morning_cum_snow_m, exam_cum_snow_m) %>% 
  cor

list(res1, res2, res3) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ 0 (morning)",
      "Temperature (\\degree C) 0-3 (morning)",
      "Temperature (\\degree C) 6-9 (morning)",
      "Temperature (\\degree C) $>$ 9 (morning)",
      "Rainfall (mm) (morning)",
      "Cumulated snow $>$ 10 cm (morning)",
      "Temperature (\\degree C) $\\le$ 0 (during exam)",
      "Temperature (\\degree C) 0-3 (during exam)",
      "Temperature (\\degree C) 6-9 (during exam)",
      "Temperature (\\degree C) $>$ 9 (during exam)",
      "Rainfall (mm) (during exam)",
      "Cumulated snow $>$ 10 cm (during exam)"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 8)),
      c("Year FE", rep("Yes", 8))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/morning_exam_reg.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

reg_df %>%
  filter(year <= 2020) %>% 
  select(morning_temperature_degree, exam_temperature_degree) %>% 
  cor

# heterogeneity

res1 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    filter(daytime_cum_snow_m > .1)
)

res2 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df %>% 
    filter(daytime_cum_snow_m <= .1)
)

res3 <- felm(
  admission_total_share ~ temp_cut*factor(daytime_cum_snow_m > .1) + daytime_precipitation_mm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res3 %>% summary

list(res1, res2, res3) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    # keep = c(
    #   "temp_cut\\(-10,0\\]",
    #   "temp_cut\\(0,3\\]",
    #   "temp_cut\\(6,9\\]",
    #   "temp_cut\\(9,25\\]",
    #   "factor\\(daytime_cum_snow_m > 0.1\\)TRUE",
    #   "daytime_precipitation_mm",
    #   "temp_cut\\(0,3\\]:factor(daytime_cum_snow_m > 0.1)TRUE"
    # ),
    keep = c(
      1, 2, 3, 4, 5, 6, 8
    ),
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ 0",
      "Temperature (\\degree C) 0-3",
      "Temperature (\\degree C) 6-9",
      "Temperature (\\degree C) $>$ 9",
      "Cumulated snow $>$ 10 cm",
      "Rainfall (mm)",
      "Temperature (\\degree C) 0-3 $\\times$ Cum. snow $>$ 10 cm"
    ),
    title = "",
    add.lines = list(
      c("Prefecture FE", rep("Yes", 8)),
      c("Year FE", rep("Yes", 8))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/hetero_exam_reg.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )
