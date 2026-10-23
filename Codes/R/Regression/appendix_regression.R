# Appendix ==============
# Table B.1
# Regression with linear specification ==========================

res_1 <- felm(
  admission_total_share ~ daytime_temperature_degree | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ daytime_temperature_degree + daytime_precipitation_mm + daytime_snowfall_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_3 <- felm(
  admission_total_share ~ daytime_temperature_degree + daytime_precipitation_mm + daytime_cum_snow_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_total_share ~ daytime_temperature_degree + daytime_precipitation_mm + factor(daytime_cum_snow_m > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    covariate.labels = c(
      "Temperature (\\degree C)",
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
    out = file.path(git_dir, "Output/tex/linear_reg.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Table B.2
# Placebo regression with lead weather =====================

res_1 <- felm(
  admission_total_share ~ f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ f1_temp_cut + f1_precipitation_mm + f1_cum_snow_cm |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_total_share ~ 
    temp_cut + daytime_precipitation_mm + daytime_snowfall_cm +
    f1_temp_cut + f1_precipitation_mm + f1_snowfall_cm |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_4 <- felm(
  admission_total_share ~ 
    temp_cut + daytime_precipitation_mm + daytime_cum_snow_cm +
    f1_temp_cut + f1_precipitation_mm + f1_cum_snow_cm |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    order = c(seq(1, 4), seq(5, 7), seq(8, 11), seq(12, 14)),
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ 0 ($t$)",
      "Temperature (\\degree C) 0-3 ($t$)",
      "Temperature (\\degree C) 6-9 ($t$)",
      "Temperature (\\degree C) $>$ 9 ($t$)",
      "Rainfall (mm) ($t$)",
      "Snowfall (cm) ($t$)",
      "Cumulated snow (cm) ($t$)",
      "Temperature (\\degree C) $\\le$ 0 ($t + 1$)",
      "Temperature (\\degree C) 0-3 ($t + 1$)",
      "Temperature (\\degree C) 6-9 ($t + 1$)",
      "Temperature (\\degree C) $>$ 9 ($t + 1$)",
      "Rainfall (mm) ($t + 1$)",
      "Snowfall (cm) ($t + 1$)",
      "Cumulated snow (cm) ($t + 1$)"
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

# Table B.3
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

# Table B.4
# Regression with weather in the morning vs during exam =====================

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

# Table B.5
# Regression with weather on the 1st day vs 2nd day =====================

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

# Table B.6
# Regression with interaction between temperature and snow =====================

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

