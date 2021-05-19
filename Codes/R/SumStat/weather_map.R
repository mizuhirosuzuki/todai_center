packages <- c(
  "rgdal",
  "sp",
  "raster",
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl",
  "lfe",
  "stargazer",
  "RColorBrewer",
  "kableExtra"
)
pacman::p_load(packages, character.only = TRUE)

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

# Load weather information =================
weather_df <- read_csv(file.path(dropbox_dir, "Data/temp/weather_on_exam_day.csv"))

# Draw map of average temperature  ===============

map_Japan <- shapefile(
  file.path(
    dropbox_dir,
    "Data/shapefile",
    "jpn_admbnda_adm0_2019.shp"
  )
)
map <- shapefile(
  file.path(
    dropbox_dir,
    "Data/shapefile",
    "jpn_admbnda_adm1_2019.shp"
  )
)

weather_df_avg <- weather_df %>% 
  mutate(daytime_cum_snow_cm = daytime_cum_snow_cm / 100) %>% 
  group_by(prefecture) %>% 
  summarise_at(vars(daytime_temperature_degree, daytime_cum_snow_cm), mean)

map@data$ADM1_JA <- str_replace(map@data$ADM1_JA, "県|府", "")
map@data$ADM1_JA <- str_replace(map@data$ADM1_JA, "東京都", "東京")

map_avg <- merge(map, weather_df_avg, by.x = "ADM1_JA", by.y = "prefecture")

# Average temperature -------------------

filepath = file.path(git_dir, 'Output/images/temperature_map.png')
png(file = filepath)
print(
  spplot(
    map_avg, 
    "daytime_temperature_degree", 
    sp.layout = list(map_Japan, lwd = 0.1), 
    col.regions = brewer.pal(n = 7, name = "OrRd"),
    # cuts = 6,
    at = seq(-5, 20, 5),
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey = list(
      labels = list(
        at = seq(-5, 20, 5),
        labels = as.character(seq(-5, 20, 5)),
        cex = 1.0
      )
    ),
    col = "transparent"
  )
)
dev.off()

# Average cumulative snow -------------------

filepath = file.path(git_dir, 'Output/images/cum_snow_map.png')
png(file = filepath)
print(
  spplot(
    map_avg, 
    "daytime_cum_snow_cm", 
    sp.layout = list(map_Japan, lwd = 0.1), 
    col.regions = brewer.pal(n = 7, name = "OrRd"),
    # cuts = 6,
    at = seq(0, 0.7, 0.1),
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey = list(
      labels = list(
        at = seq(0, 0.7, 0.1),
        labels = as.character(seq(0, 0.7, 0.1)),
        cex = 1.0
      )
    ),
    col = "transparent"
  )
)
dev.off()
