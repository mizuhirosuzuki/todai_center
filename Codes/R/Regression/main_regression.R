# Main regression ==========================

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

# Non-linear regression figures ============
walk2(
  list(res_1, res_2, res_3, res_4),
  c(1, 2, 3, 4), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/main_reg_${.y}.pdf")),
    plot = make_coef_figure(.x),
    height = 6,
    width = 6
    )
  )

