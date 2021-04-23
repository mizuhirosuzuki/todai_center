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

# Placebo regression with lead weather =====================

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
      "Temperature (\\degree C) $\\le$ 0 ($t$)",
      "Temperature (\\degree C) $>$ 0, $\\le$ 3 ($t$)",
      "Temperature (\\degree C) $>$ 6, $\\le$ 9 ($t$)",
      "Temperature (\\degree C) $>$ 9 ($t$)",
      "Rainfall (mm) ($t$)",
      "Snowfall (m) ($t$)",
      "Cumulated snow (m) ($t$)",
      "Temperature (\\degree C) $\\le$ 0 ($t + 1$)",
      "Temperature (\\degree C) $>$ 0, $\\le$ 3 ($t + 1$)",
      "Temperature (\\degree C) $>$ 6, $\\le$ 9 ($t + 1$)",
      "Temperature (\\degree C) $>$ 9 ($t + 1$)",
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

# Non-linear regression figures ============

walk2(
  list(res_1, res_2, res_3, res_4),
  seq(1, 4), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/reg_placebo_f1_${.y}.pdf")),
    plot = make_coef_figure(.x, temp_cut_str = "^f1_temp_cut"),
    height = 6,
    width = 6
    )
  )

walk2(
  list(res_3, res_4),
  seq(3, 4), 
  ~ ggsave(
    filename = file.path(git_dir, str_interp("Output/images/reg_placebo_exam_${.y}.pdf")),
    plot = make_coef_figure(.x, temp_cut_str = "^temp_cut"),
    height = 6,
    width = 6
    )
  )


