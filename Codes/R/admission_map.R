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
  "kableExtra",
  "classInt"
)
pacman::p_load(packages, character.only = TRUE)

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

# Load admission information =================
admission_df <- read_csv(file.path(dropbox_dir, "Data/admission_data.csv"))

# Draw map of average admission shares  ===============

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

admission_df_avg <- admission_df %>% 
  group_by(prefecture) %>% 
  summarise_at(vars(admission_total_share), mean)

map@data$ADM1_JA <- str_replace(map@data$ADM1_JA, "県|府", "")
map@data$ADM1_JA <- str_replace(map@data$ADM1_JA, "東京都", "東京")

map_avg <- merge(map, admission_df_avg, by.x = "ADM1_JA", by.y = "prefecture")

filepath = file.path(git_dir, 'Output/images/admission_map.png')
png(file = filepath)
print(
  spplot(
    map_avg, 
    "admission_total_share", 
    sp.layout = list(map_Japan, lwd = 0.1), 
    # col.regions = brewer.pal(n = 10, name = "OrRd"),
    # at = classIntervals(map_avg$admission_total_share, n = 9, style = "quantile", intervalClosure = "right")$brks,
    col.regions = brewer.pal(n = 9, name = "OrRd"),
    # cuts = 8,
    at = c(0, 0.5, 1.0, 2.0, 3.0, 5.0, 10.0, 40.0),
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey = list(
      labels = list(
        at = c(0, 1.0, 2.0, 3.0, 5.0, 10.0, 40.0),
        labels = as.character(c(0, 1.0, 2.0, 3.0, 5.0, 10.0, 40.0)),
        cex = 1.0
      )
    ),
    col = "transparent"
  )
)
dev.off()

admission_df_avg %>% 
  arrange(desc(admission_total_share)) %>% 
  mutate(cum_admission = cumsum(admission_total_share))

