packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl",
  "lfe",
  "stargazer",
  "latex2exp"
)
pacman::p_load(packages, character.only = TRUE)

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

# Load regression data =================
reg_df <- readRDS(file.path(dropbox_dir, "Data/temp/data_regression.rds"))

# Import function to make estimate figures
source(file.path(git_dir, "Codes/R/Regression/make_coef_figure.R"))

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
  admission_total_share ~ daytime_temperature_degree + daytime_precipitation_mm + factor(daytime_cum_snow_m > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

res_8 <- felm(
  admission_total_share ~ temp_cut + daytime_precipitation_mm + factor(daytime_cum_snow_m > .1) | 
    prefecture + year | 0 | prefecture, 
  data = reg_df
)

list(res_1, res_2, res_3, res_4, res_5, res_6, res_7, res_8) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    covariate.labels = c(
      "Temperature (\\degree C)",
      "Temperature (\\degree C) $\\le$ 0",
      "Temperature (\\degree C) $>$ 0, $\\le$ 3",
      "Temperature (\\degree C) $>$ 6, $\\le$ 9",
      "Temperature (\\degree C) $>$ 9",
      "Rainfall (mm)",
      "Snowfall (m)",
      "Cumulated snow (m)",
      "Cumulated snow $>$ .10 m"
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
list(res_4, res_6, res_8) %>% 
  stargazer(
    dep.var.labels = "Matriculation share (\\%)",
    keep = c(
      "^[^temp_cut]"
    ),
    covariate.labels = c(
      "Rainfall (mm)",
      "Snowfall (m)",
      "Cumulated snow (m)",
      "Cumulated snow $>$ .10 m"
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
  list(res_2, res_4, res_6, res_8),
  c(2, 4, 6, 8), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/main_reg_${.y}.pdf")),
    plot = make_coef_figure(.x),
    height = 6,
    width = 6
    )
  )

