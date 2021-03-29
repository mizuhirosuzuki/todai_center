packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate"
)
pacman::p_load(packages, character.only = TRUE)

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

# Hourly weather ====================
# Load data
weather_hour_df <- read_csv(file.path(dropbox_dir, "Data/weather_hourly/all_weather_hour.csv"))

weather_hour_output <- weather_hour_df %>% 
  group_by(prefecture, year, month, day) %>% 
  summarise_at(
      vars(temperature_degree, precipitation_mm, snowfall_cm, cum_snow_cm),
      mean, na.rm = TRUE
  ) %>% 
  left_join(
    weather_hour_df %>% 
      filter(hour >= 6, hour <= 18) %>% 
      group_by(prefecture, year, month, day) %>% 
      summarise_at(
          vars(temperature_degree, precipitation_mm, snowfall_cm, cum_snow_cm),
          mean, na.rm = TRUE
      ) %>% 
      rename(
        daytime_temperature_degree = temperature_degree,
        daytime_precipitation_mm = precipitation_mm,
        daytime_snowfall_cm = snowfall_cm,
        daytime_cum_snow_cm = cum_snow_cm
      ),
    by = c("prefecture", "year", "month", "day")
  ) %>% 
  left_join(
    weather_hour_df %>% 
      filter(hour >= 6, hour <= 9) %>% 
      group_by(prefecture, year, month, day) %>% 
      summarise_at(
          vars(temperature_degree, precipitation_mm, snowfall_cm, cum_snow_cm),
          mean, na.rm = TRUE
      ) %>% 
      rename(
        morning_temperature_degree = temperature_degree,
        morning_precipitation_mm = precipitation_mm,
        morning_snowfall_cm = snowfall_cm,
        morning_cum_snow_cm = cum_snow_cm
      ),
    by = c("prefecture", "year", "month", "day")
  ) %>% 
  left_join(
    weather_hour_df %>% 
      filter(hour >= 9, hour <= 18) %>% 
      group_by(prefecture, year, month, day) %>% 
      summarise_at(
          vars(temperature_degree, precipitation_mm, snowfall_cm, cum_snow_cm),
          mean, na.rm = TRUE
      ) %>% 
      rename(
        exam_temperature_degree = temperature_degree,
        exam_precipitation_mm = precipitation_mm,
        exam_snowfall_cm = snowfall_cm,
        exam_cum_snow_cm = cum_snow_cm
      ),
    by = c("prefecture", "year", "month", "day")
  ) %>% 
  group_by(prefecture, year, month, day) %>% 
  arrange(day, .by_group = TRUE) %>% 
  mutate(exam_first_second = row_number()) %>% 
  ungroup() %>% 
  rename(exam_year = year, exam_month = month, exam_day = day)

write_csv(
  weather_hour_output,
  file.path(dropbox_dir, "Data/temp/weather_on_exam_day.csv")
)

# Daily weather ====================
# Load data
weather_day_df <- read_csv(file.path(dropbox_dir, "Data/weather_day/all_weather_day.csv"))

exam_date <- c(
  "2012/1/14", "2012/1/15",
  "2013/1/19", "2013/1/20",
  "2014/1/18", "2014/1/19",
  "2015/1/17", "2015/1/18",
  "2016/1/16", "2016/1/17",
  "2017/1/14", "2017/1/15",
  "2018/1/13", "2018/1/14",
  "2019/1/19", "2019/1/20",
  "2020/1/18", "2020/1/19"
)

exam_first_day <- exam_date[seq_along(exam_date) %% 2 == 1] %>% 
  as_date() %>% 
  day()
exam_year <- seq(2012, 2020)

weather_day_output <- map2(
  exam_first_day, exam_year, 
  function(x, y) weather_day_df %>% filter(day >= x - 10, day <= x - 1, year == y)
  ) %>% 
  bind_rows() %>% 
  group_by(prefecture, year) %>% 
  summarise_at(
      vars(temperature_avg_degree, precipitation_sum_mm, snowfall_cm, cum_snow_cm),
      mean, na.rm = TRUE
  ) %>% 
  rename(
    exam_year = year,
    temperature_pre10_avg = temperature_avg_degree,
    precipitation_pre10_sum = precipitation_sum_mm,
    snowfall_pre10 = snowfall_cm,
    cum_snow_pre10 = cum_snow_cm
  )
  
write_csv(
  weather_day_output,
  file.path(dropbox_dir, "Data/temp/weather_pre10.csv")
)

# Monthly weather ====================
# Load data
weather_month_df <- read_csv(file.path(dropbox_dir, "Data/weather_month/all_weather_month.csv"))

weather_month_output <- weather_month_df %>% 
  filter(month >= 10, month <= 12) %>% 
  pivot_wider(names_from = month, values_from = c(precipitation_sum, temperature_daily_average, snowfall_sum, cum_snow_max))

write_csv(
  weather_month_output,
  file.path(dropbox_dir, "Data/temp/weather_pre_oct_dec.csv")
)













