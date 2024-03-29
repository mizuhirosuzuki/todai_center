# Load admission information =================
admission_df <- read_csv(file.path(dropbox_dir, "Data/Processed/admission_data.csv"))

# Load weather information =================
weather_df <- read_csv(file.path(dropbox_dir, "Data/Processed/weather_on_exam_day.csv"))

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
  function(x) mean(x, na.rm = TRUE),
  function(x) sd(x, na.rm = TRUE),
  function(x) median(x, na.rm = TRUE),
  function(x) min(x, na.rm = TRUE),
  function(x) max(x, na.rm = TRUE)
  ) %>% 
  map(
  function(x) 
    reg_df %>% 
    dplyr::select(
      admission_total_share,
      daytime_temperature_degree,
      daytime_precipitation_mm,
      daytime_snowfall_cm,
      daytime_cum_snow_cm
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
    "Matriculation share (\\%)",
    "Temperature (\\degree C)",
    "Hourly precipitation (mm)",
    "Hourly snowfall (cm)",
    "Cumulated snow (cm)"
  )) %>% 
  kbl("latex", booktabs = TRUE, escape = FALSE) %>% 
  add_header_above(
    c(" ", "N", "Mean", "SD", "Median", "Min", "Max")
  ) %>% 
  save_kable(file.path(git_dir, "Output/tex/sum_stat.tex"))
