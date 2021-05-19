# Regression by male and female matriculations =====================

res_1 <- felm(
  admission_female_share ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_female_share ~ temp_cut + daytime_precipitation_mm + daytime_cum_snow_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_male_share ~ temp_cut + daytime_precipitation_mm + daytime_snowfall_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_4 <- felm(
  admission_male_share ~ temp_cut + daytime_precipitation_mm + daytime_cum_snow_cm | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = c("Matriculation share (\\%, female)", "Matriculation share (\\%, male)"),
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ 0",
      "Temperature (\\degree C) $>$ 0, $\\le$ 3",
      "Temperature (\\degree C) $>$ 6, $\\le$ 9",
      "Temperature (\\degree C) $>$ 9",
      "Rainfall (mm)",
      "Snowfall (cm)",
      "Cumulated snow (cm)",
      "Cumulated snow $>$ 10 cm"
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

# Non-linear regression figures ============

walk2(
  list(res_1, res_2),
  seq(1, 2), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/reg_gender_${.y}.pdf")),
    plot = make_coef_figure(.x, temp_cut_str = "^temp_cut"),
    height = 6,
    width = 6
    )
  )

walk2(
  list(res_3, res_4),
  seq(3, 4), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/reg_gender_${.y}.pdf")),
    plot = make_coef_figure(.x, temp_cut_str = "^temp_cut"),
    height = 6,
    width = 6
    )
  )


