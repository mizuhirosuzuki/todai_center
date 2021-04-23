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
reg_df <- readRDS(file.path(dropbox_dir, "Data/temp/data_regression.rds"))

# Import function to make estimate figures
source(file.path(git_dir, "Codes/R/Regression/make_coef_figure.R"))

# Regression with weather for the previous 10 days =====================

res_1 <- felm(
  admission_total_share ~ temp_cut_pre10 + precipitation_pre10_sum + snowfall_pre10_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
  )

res_2 <- felm(
  admission_total_share ~ temp_cut_pre10 + precipitation_pre10_sum + cum_snow_pre10_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_3 <- felm(
  admission_total_share ~ 
    temp_cut + daytime_precipitation_mm + daytime_snowfall_m +
    temp_cut_pre10 + precipitation_pre10_sum + snowfall_pre10_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_4 <- felm(
  admission_total_share ~ 
    temp_cut + daytime_precipitation_mm + daytime_cum_snow_m +
    temp_cut_pre10 + precipitation_pre10_sum + cum_snow_pre10_m |
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    order = c(seq(1, 4), seq(5, 7), seq(8, 11), seq(12, 14)),
    covariate.labels = c(
      "Temperature (\\degree C) $\\le$ 0 (exam dates)",
      "Temperature (\\degree C) $>$ 0, $\\le$ 3 (exam dates)",
      "Temperature (\\degree C) $>$ 6, $\\le$ 9 (exam dates)",
      "Temperature (\\degree C) $>$ 9 (exam dates)",
      "Rainfall (mm) (exam dates)",
      "Snowfall (m) (exam dates)",
      "Cumulated snow (m) (exam dates)",
      "Temperature (\\degree C) $\\le$ 0 (previous 10 days)",
      "Temperature (\\degree C) $>$ 0, $\\le$ 3 (previous 10 days)",
      "Temperature (\\degree C) $>$ 6, $\\le$ 9 (previous 10 days)",
      "Temperature (\\degree C) $>$ 9 (previous 10 days)",
      "Rainfall (mm) (previous 10 days)",
      "Snowfall (m) (previous 10 days)",
      "Cumulated snow (m) (previous 10 days)"
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
      "Snowfall (m) (exam dates)",
      "Cumulated snow (m) (exam dates)",
      "Rainfall (mm) (previous 10 days)",
      "Snowfall (m) (previous 10 days)",
      "Cumulated snow (m) (previous 10 days)"
    ),
    title = "",
    add.lines = list(
      c("Temperature bins", rep("Yes", 3)),
      c("Prefecture FE", rep("Yes", 3)),
      c("Year FE", rep("Yes", 3))
    ),
    type = "latex",
    out = file.path(git_dir, "Output/tex/res_pre10_beamer.tex"),
    omit.stat = c("adj.rsq", "ser", "rsq"),
    digits = 2,
    float = FALSE
  )

# Non-linear regression figures ============

walk2(
  list(res_1, res_2, res_3, res_4),
  seq(1, 4), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/reg_pre10_pre10_${.y}.pdf")),
    plot = make_coef_figure(.x, temp_cut_str = "^temp_cut_pre10"),
    height = 6,
    width = 6
    )
  )

walk2(
  list(res_3, res_4),
  seq(3, 4), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/reg_pre10_exam_${.y}.pdf")),
    plot = make_coef_figure(.x, temp_cut_str = "^temp_cut\\("),
    height = 6,
    width = 6
    )
  )

