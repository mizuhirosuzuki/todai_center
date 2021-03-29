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

# Load weather information =================
weather_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_on_exam_day.csv"))

# Load location information
loc_df <- read_delim(file.path(dropbox_dir, "Data/weather_station_id.txt"), delim = ",")
prefecture_order <- loc_df$prefecture

weather_df_avg <- weather_df %>% 
  mutate(
    daytime_cum_snow_m = daytime_cum_snow_cm / 100,
    daytime_snowfall_m = daytime_snowfall_cm / 100,
    ) %>% 
  group_by(prefecture, exam_year, exam_month) %>% 
  summarise_at(
    vars(daytime_temperature_degree, daytime_cum_snow_m, daytime_snowfall_m, daytime_precipitation_mm), 
    mean
    ) %>% 
  mutate(prefecture = rev(factor(prefecture, levels = prefecture_order)))

# Daytime temperature
p <- weather_df_avg %>% 
  ggplot(aes(x = prefecture, y = daytime_temperature_degree, color = factor(exam_year))) +
  geom_point() +
  scale_x_discrete(limits = prefecture_order) +
  scale_color_brewer(
    palette = "YlOrRd",
    name = "Year"
  ) +
  ylab("Temperature (degree Celsius)") +
  xlab("Prefecture") +
  theme_minimal() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(git_dir, "Output/images/temperature_diff.pdf"),
  plot = p,
  width = 10,
  height = 7
)

# Daytime cumulated snow
p <- weather_df_avg %>% 
  ggplot(aes(x = prefecture, y = daytime_cum_snow_m, color = factor(exam_year))) +
  geom_point() +
  scale_x_discrete(limits = prefecture_order) +
  scale_color_brewer(
    palette = "YlOrRd",
    name = "Year"
  ) +
  ylab("Cumulated snow (m)") +
  xlab("Prefecture") +
  theme_minimal() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(git_dir, "Output/images/cum_snow_diff.pdf"),
  plot = p,
  width = 10,
  height = 7
)

# Daytime snowfall
p <- weather_df_avg %>% 
  ggplot(aes(x = prefecture, y = daytime_snowfall_m, color = factor(exam_year))) +
  geom_point() +
  scale_x_discrete(limits = prefecture_order) +
  scale_color_brewer(
    palette = "YlOrRd",
    name = "Year"
  ) +
  ylab("Snowfall (m)") +
  xlab("Prefecture") +
  theme_minimal() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(git_dir, "Output/images/snowfall_diff.pdf"),
  plot = p,
  width = 10,
  height = 7
)

# Daytime rainfall
p <- weather_df_avg %>% 
  ggplot(aes(x = prefecture, y = daytime_precipitation_mm, color = factor(exam_year))) +
  geom_point() +
  scale_x_discrete(limits = prefecture_order) +
  scale_color_brewer(
    palette = "YlOrRd",
    name = "Year"
  ) +
  ylab("Rainfall (mm)") +
  xlab("Prefecture") +
  theme_minimal() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(git_dir, "Output/images/rainfall_diff.pdf"),
  plot = p,
  width = 10,
  height = 7
)



