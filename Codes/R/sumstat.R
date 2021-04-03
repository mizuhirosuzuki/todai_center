packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl",
  "lfe",
  "stargazer",
  "kableExtra"
)
pacman::p_load(packages, character.only = TRUE)

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

# Load admission information =================
admission_df <- read_csv(file.path(dropbox_dir, "Data/admission_data.csv"))

# Load weather information =================
weather_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_on_exam_day.csv"))

# Regression with average weather =====================

reg_df <- left_join(
  admission_df,
  weather_df %>% 
    group_by(prefecture, exam_year) %>% 
    summarise_all(mean),
  by = c("prefecture", "year" = "exam_year")
  ) %>% 
  mutate(
    temp_cut = cut(daytime_temperature_degree, c(-10, 0, 3, 6, 9, 25)),
    temp_cut = relevel(as.factor(temp_cut), ref = 3),
    daytime_snowfall_m = daytime_snowfall_cm / 100,
    daytime_cum_snow_m = daytime_cum_snow_cm / 100
    )

list(
  function(x) sum(!is.na(x)), 
  mean, sd, median, min, max
) %>% 
map(
  function(x) 
    reg_df %>% 
    dplyr::select(
      admission_total_share,
      daytime_temperature_degree,
      daytime_precipitation_mm,
      daytime_snowfall_m,
      daytime_cum_snow_m
      ) %>% 
    summarise_all(x)
  ) %>% 
  bind_rows() %>% 
  as.matrix() %>% 
  t() %>% 
  as_tibble() %>% 
  mutate(
    V1 = formatC(V1),
    across(
      .cols = V2:V6,
      .fns = ~ formatC(., digits = 2, format = "f")
    )
  ) %>% 
  as.matrix() %>% 
  set_colnames(NULL) %>% 
  set_rownames(c(
    "Matriculation share (%)",
    "Temperature (degree C)",
    "Hourly precipitation (mm)",
    "Hourly snowfall (m)",
    "Cumulated snow (m)"
  )) %>% 
  kbl("latex", booktabs = TRUE) %>% 
  add_header_above(
    c(" ", "N", "Mean", "SD", "Median", "Min", "Max")
  ) %>% 
  save_kable(file.path(git_dir, "Output/tex/sum_stat.tex"))


