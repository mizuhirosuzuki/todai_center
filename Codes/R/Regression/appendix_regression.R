# Appendix ==============
# Table A.1
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

# Table A.2
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
