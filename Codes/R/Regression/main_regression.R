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

