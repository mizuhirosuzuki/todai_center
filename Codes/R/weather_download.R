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

# Load location information
loc_df <- read_delim(file.path(dropbox_dir, "Data/weather_station_id.txt"), delim = ",")

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

# Hourly weather ===========================
# Download exam day (hourly) weather -----------------------

get_hourly_weather <- function(loc_i, j) {
  
  exam_date_lubridate <- exam_date[j] %>% as_date()
  exam_year <- exam_date_lubridate %>% year()
  exam_month <- exam_date_lubridate %>% month()
  exam_day <- exam_date_lubridate %>% day()

  if (i %in% c(11)) {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/hourly_a1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(exam_year), 
        "&month=", as.character(exam_month), 
        "&day=", as.character(exam_day), 
        "&view="
        )
    )
    # Sys.sleep(runif(1, max = 10))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[5] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "hour", "precipitation_mm", "temperature_degree",
          "wind_speed", "wind_direction",
          "sunny_time", "snowfall_cm", "cum_snow_cm"
          )
        ) %>% 
      as_tibble() %>% 
      slice(-1) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
    
  } else {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/hourly_s1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(exam_year), 
        "&month=", as.character(exam_month), 
        "&day=", as.character(exam_day), 
        "&view="
        )
    )
    # Sys.sleep(runif(1, min = 20, max = 40))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[5] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "hour", "airpressure_local", "airpressure_sea",
          "precipitation_mm", "temperature_degree",
          "dewpoint_degree", "vapor_pressure",
          "humidity", "wind_speed", "wind_direction",
          "sunny_time", "all_sun_amount",
          "snowfall_cm", "cum_snow_cm", "weather", 
          "cloud", "visuality"
          )
        ) %>% 
      as_tibble() %>% 
      slice(-1) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
  }
  
  html_output <- html_table %>% 
    select(hour, precipitation_mm, temperature_degree, snowfall_cm, cum_snow_cm) %>% 
    mutate(year = exam_year, month = exam_month, day = exam_day, prefecture = loc_i$prefecture)

  return(html_output)
}

for (i in seq(nrow(loc_df))) {
  
  print(i)
  
  loc_i <- loc_df %>% slice(i)
  
  weather_hourly_output <- map(seq_along(exam_date), ~ get_hourly_weather(loc_i, .)) %>% 
    bind_rows()
  
  write_csv(
    weather_hourly_output, 
    file.path(dropbox_dir, paste0("Data/weather_hourly/", i, "_weather_hour.csv"))
    )
  
  Sys.sleep(runif(1, min = 20, max = 40))

}

weather_hour_all <- map(
  seq(nrow(loc_df)),
  ~ read_csv(
    file.path(dropbox_dir, paste0("Data/weather_hourly/", ., "_weather_hour.csv"))
    )
  ) %>% 
  bind_rows() %>% 
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ replace_na(., 0)
    )
  )

write_csv(
  weather_hour_all, 
  file.path(dropbox_dir, "Data/weather_hourly", "all_weather_hour.csv")
  )

# Download exam day (hourly) weather: 1 year lag -----------------------

get_hourly_weather_l1 <- function(loc_i, j) {
  
  exam_date_lubridate <- exam_date[j] %>% as_date()
  exam_year <- exam_date_lubridate %>% year()
  exam_month <- exam_date_lubridate %>% month()
  exam_day <- exam_date_lubridate %>% day()

  if (i %in% c(11)) {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/hourly_a1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(exam_year - 1), 
        "&month=", as.character(exam_month), 
        "&day=", as.character(exam_day), 
        "&view="
        )
    )
    # Sys.sleep(runif(1, max = 10))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[5] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "hour", "precipitation_mm", "temperature_degree",
          "wind_speed", "wind_direction",
          "sunny_time", "snowfall_cm", "cum_snow_cm"
          )
        ) %>% 
      as_tibble() %>% 
      slice(-1) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
    
  } else {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/hourly_s1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(exam_year - 1), 
        "&month=", as.character(exam_month), 
        "&day=", as.character(exam_day), 
        "&view="
        )
    )
    # Sys.sleep(runif(1, min = 20, max = 40))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[5] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "hour", "airpressure_local", "airpressure_sea",
          "precipitation_mm", "temperature_degree",
          "dewpoint_degree", "vapor_pressure",
          "humidity", "wind_speed", "wind_direction",
          "sunny_time", "all_sun_amount",
          "snowfall_cm", "cum_snow_cm", "weather", 
          "cloud", "visuality"
          )
        ) %>% 
      as_tibble() %>% 
      slice(-1) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
  }
  
  html_output <- html_table %>% 
    select(hour, precipitation_mm, temperature_degree, snowfall_cm, cum_snow_cm) %>% 
    mutate(year = exam_year, month = exam_month, day = exam_day, prefecture = loc_i$prefecture)

  return(html_output)
}

for (i in seq(nrow(loc_df))) {
  
  print(i)
  
  loc_i <- loc_df %>% slice(i)
  
  weather_hourly_output <- map(seq_along(exam_date), ~ get_hourly_weather_l1(loc_i, .)) %>% 
    bind_rows()
  
  write_csv(
    weather_hourly_output, 
    file.path(dropbox_dir, paste0("Data/weather_hourly_l1/", i, "_weather_hour_l1.csv"))
    )
  
  Sys.sleep(runif(1, min = 20, max = 40))

}

weather_hour_all <- map(
  seq(nrow(loc_df)),
  ~ read_csv(
    file.path(dropbox_dir, paste0("Data/weather_hourly_l1/", ., "_weather_hour_l1.csv"))
    )
  ) %>% 
  bind_rows() %>% 
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ replace_na(., 0)
    )
  )

write_csv(
  weather_hour_all, 
  file.path(dropbox_dir, "Data/weather_hourly_l1", "all_weather_hour_l1.csv")
  )

# Download exam day (hourly) weather: 1 year lead -----------------------

get_hourly_weather_f1 <- function(loc_i, j) {
  
  exam_date_lubridate <- exam_date[j] %>% as_date()
  exam_year <- exam_date_lubridate %>% year()
  exam_month <- exam_date_lubridate %>% month()
  exam_day <- exam_date_lubridate %>% day()

  if (i %in% c(11)) {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/hourly_a1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(exam_year + 1), 
        "&month=", as.character(exam_month), 
        "&day=", as.character(exam_day), 
        "&view="
        )
    )
    # Sys.sleep(runif(1, max = 10))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[5] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "hour", "precipitation_mm", "temperature_degree",
          "wind_speed", "wind_direction",
          "sunny_time", "snowfall_cm", "cum_snow_cm"
          )
        ) %>% 
      as_tibble() %>% 
      slice(-1) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
    
  } else {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/hourly_s1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(exam_year + 1), 
        "&month=", as.character(exam_month), 
        "&day=", as.character(exam_day), 
        "&view="
        )
    )
    # Sys.sleep(runif(1, min = 20, max = 40))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[5] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "hour", "airpressure_local", "airpressure_sea",
          "precipitation_mm", "temperature_degree",
          "dewpoint_degree", "vapor_pressure",
          "humidity", "wind_speed", "wind_direction",
          "sunny_time", "all_sun_amount",
          "snowfall_cm", "cum_snow_cm", "weather", 
          "cloud", "visuality"
          )
        ) %>% 
      as_tibble() %>% 
      slice(-1) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
  }
  
  html_output <- html_table %>% 
    select(hour, precipitation_mm, temperature_degree, snowfall_cm, cum_snow_cm) %>% 
    mutate(year = exam_year, month = exam_month, day = exam_day, prefecture = loc_i$prefecture)

  return(html_output)
}

for (i in seq(nrow(loc_df))) {
  
  print(i)
  
  loc_i <- loc_df %>% slice(i)
  
  weather_hourly_output <- map(seq_along(exam_date), ~ get_hourly_weather_f1(loc_i, .)) %>% 
    bind_rows()
  
  write_csv(
    weather_hourly_output, 
    file.path(dropbox_dir, paste0("Data/weather_hourly_f1/", i, "_weather_hour_f1.csv"))
    )
  
  Sys.sleep(runif(1, min = 20, max = 40))

}

weather_hour_all_f1 <- map(
  seq(nrow(loc_df)),
  ~ read_csv(
    file.path(dropbox_dir, paste0("Data/weather_hourly_f1/", ., "_weather_hour_f1.csv"))
    )
  ) %>% 
  bind_rows() %>% 
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ replace_na(., 0)
    )
  )

write_csv(
  weather_hour_all_f1, 
  file.path(dropbox_dir, "Data/weather_hourly_f1", "all_weather_hour_f1.csv")
  )

# Daily weather ===========================
# Download daily weather on January -----------------------

get_day_weather <- function(loc_i, year) {

  if (i %in% c(11)) {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/daily_a1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(year), 
        "&month=1", "&day=1", "&view="
        )
    )
    # Sys.sleep(runif(1, max = 10))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[6] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "day", 
          "precipitation_sum_mm",  "precipitation_max_hour_mm",  "precipitation_max_10min_mm", 
          "temperature_avg_degree", "temperature_max_degree", "temperature_min_degree",
          "wind_speed_avg", "wind_speed_max", "wind_direction_max", 
          "wind_speed_max_max", "wind_direction_max_max", "wind_direction_most_frequent",
          "sunny_time", "snowfall_cm", "cum_snow_cm"
          )
        ) %>% 
      as_tibble() %>% 
      slice(- c(1, 2)) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
    
  } else {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/daily_s1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(year), 
        "&month=1", "&day=1", "&view="
        )
    )
    # Sys.sleep(runif(1, min = 20, max = 40))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[6] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "day", "airpressure_local", "airpressure_sea",
          "precipitation_sum_mm",  "precipitation_max_hour_mm",  "precipitation_max_10min_mm", 
          "temperature_avg_degree", "temperature_max_degree", "temperature_min_degree",
          "humidity_avg", "humidity_min",
          "wind_speed_avg", "wind_speed_max", "wind_direction_max", 
          "wind_speed_max_max", "wind_direction_max_max", 
          "sunny_time", "snowfall_cm", "cum_snow_cm",
          "weather_daytime",  "weather_night"
          )
        ) %>% 
      as_tibble() %>% 
      slice(-c(1, 2, 3)) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
  }
  
  html_output <- html_table %>% 
    select(day, precipitation_sum_mm, temperature_avg_degree, snowfall_cm, cum_snow_cm) %>% 
    mutate(year = year, prefecture = loc_i$prefecture)

  return(html_output)
}

for (i in seq(nrow(loc_df))) {
  
  print(i)
  
  loc_i <- loc_df %>% slice(i)
  
  weather_day_output <- map(seq(2012, 2020), ~ get_day_weather(loc_i, .)) %>% 
    bind_rows()
  
  write_csv(
    weather_day_output, 
    file.path(dropbox_dir, paste0("Data/weather_day/", i, "_weather_day.csv"))
    )
  
  Sys.sleep(runif(1, min = 20, max = 40))

}

weather_day_all <- map(
  seq(nrow(loc_df)),
  ~ read_csv(
    file.path(dropbox_dir, paste0("Data/weather_day/", ., "_weather_day.csv"))
    )
  ) %>% 
  bind_rows() %>% 
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ replace_na(., 0)
    )
  )

write_csv(
  weather_day_all, 
  file.path(dropbox_dir, "Data/weather_day/", "all_weather_day.csv")
  )

# Monthly weather ===========================
# Download monthly weather on previous October, November, and December -----------------------

get_month_weather <- function(loc_i, year) {

  if (i %in% c(11)) {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/monthly_a1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(year - 1), 
        "&month=1", "&day=1", "&view="
        )
    )
    # Sys.sleep(runif(1, max = 10))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[6] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "month", "precipitation_sum", "precipitation_daily_max",
          "precipitation_max_hour", "precipitation_hour_10min", 
          "temperature_daily_average", "temperature_daily_max", "temperature_daily_min", 
          "temperature_monthly_max", "temperature_monthly_min",
          "wind_speed_average",  "wind_speed_max",  "wind_direction_max",
          "wind_speed_max_max",  "wind_direction_max_max", 
          "sunny_time", "snowfall_sum", "snowfall_snow_max", "cum_snow_max"
          )
        ) %>% 
      as_tibble() %>% 
      slice(- c(1, 2)) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
    
  } else {
    
    html_data <- read_html(
      paste0(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/monthly_s1.php?prec_no=",
        loc_i$prec_no, "&block_no=", loc_i$block_no,
        "&year=", as.character(year - 1), 
        "&month=1", "&day=1", "&view="
        )
    )
    # Sys.sleep(runif(1, min = 20, max = 40))
  
    html_table <- html_data %>% 
      html_nodes("table") %>% 
      .[6] %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      set_colnames(
        c(
          "month", "airpressure_local", "airpressure_sea",
          "precipitation_sum", "precipitation_daily_max",
          "precipitation_max_hour", "precipitation_hour_10min", 
          "temperature_daily_average", "temperature_daily_max", "temperature_daily_min", 
          "temperature_monthly_max", "temperature_monthly_min",
          "hunidity_average", "humidity_min",
          "wind_speed_average",  "wind_speed_max",  "wind_direction_max",
          "wind_speed_max_max",  "wind_direction_max_max", 
          "sunny_time", "all_sun_amount",
          "snowfall_sum", "snowfall_snow_max", "cum_snow_max",
          "cloud", "snow_day", "mist_day", "lightning_day"
          )
        ) %>% 
      as_tibble() %>% 
      slice(-c(1, 2)) %>% 
      mutate(
        across(
          .fns = as.numeric
        )
      )
  }
  
  html_output <- html_table %>% 
    select(month, precipitation_sum, temperature_daily_average, snowfall_sum, cum_snow_max) %>% 
    mutate(exam_year = year, prefecture = loc_i$prefecture)

  return(html_output)
}

for (i in seq(nrow(loc_df))) {
  
  print(i)
  
  loc_i <- loc_df %>% slice(i)
  
  weather_month_output <- map(seq(2012, 2020), ~ get_month_weather(loc_i, .)) %>% 
    bind_rows()
  
  write_csv(
    weather_month_output, 
    file.path(dropbox_dir, paste0("Data/weather_month/", i, "_weather_month.csv"))
    )
  
  Sys.sleep(runif(1, min = 20, max = 40))

}

weather_month_all <- map(
  seq(nrow(loc_df)),
  ~ read_csv(
    file.path(dropbox_dir, paste0("Data/weather_month/", ., "_weather_month.csv"))
    )
  ) %>% 
  bind_rows() %>% 
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ replace_na(., 0)
    )
  )

write_csv(
  weather_month_all, 
  file.path(dropbox_dir, "Data/weather_month/", "all_weather_month.csv")
  )

