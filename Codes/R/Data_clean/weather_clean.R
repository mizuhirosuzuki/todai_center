# Hourly weather ====================
# Load data
weather_hour_df <- read_csv(
  file.path(
    dropbox_dir, 
    "Data/Raw/weather_hourly/all_weather_hour.csv"
  )
)

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
      filter(hour >= 10, hour <= 18) %>% 
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
  group_by(prefecture, year, month) %>% 
  arrange(day, .by_group = TRUE) %>% 
  mutate(exam_first_second = row_number()) %>% 
  ungroup() %>% 
  rename(exam_year = year, exam_month = month, exam_day = day)

write_csv(
  weather_hour_output,
  file.path(dropbox_dir, "Data/Processed/weather_on_exam_day.csv")
)

# Hourly weather: 1 year lead  ====================
# Load data
weather_hour_df <- read_csv(file.path(dropbox_dir, "Data/Raw/weather_hourly_f1/all_weather_hour_f1.csv"))

weather_hour_output <- weather_hour_df %>% 
  filter(hour >= 6, hour <= 18) %>% 
  group_by(prefecture, year, month, day) %>% 
  summarise_at(
      vars(temperature_degree, precipitation_mm, snowfall_cm, cum_snow_cm),
      mean, na.rm = TRUE
  ) %>% 
  rename(
    f1_temperature_degree = temperature_degree,
    f1_precipitation_mm = precipitation_mm,
    f1_snowfall_cm = snowfall_cm,
    f1_cum_snow_cm = cum_snow_cm
  ) %>% 
  group_by(prefecture, year, month) %>% 
  arrange(day, .by_group = TRUE) %>% 
  mutate(exam_first_second = row_number()) %>% 
  ungroup() %>% 
  rename(exam_year = year, exam_month = month, exam_day = day)

write_csv(
  weather_hour_output,
  file.path(dropbox_dir, "Data/Processed/weather_f1.csv")
)

# Daily weather ====================
# Load data
weather_day_df <- read_csv(file.path(dropbox_dir, "Data/Raw/weather_day/all_weather_day.csv"))

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
  file.path(dropbox_dir, "Data/Processed/weather_pre10.csv")
)

