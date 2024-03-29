# Load admission information =================
admission_df <- read_csv(file.path(dropbox_dir, "Data/Processed/admission_data.csv"))

# Draw map of average admission shares  ===============
map_Japan <- shapefile(
  file.path(
    dropbox_dir,
    "Data/Raw/shapefile",
    "jpn_admbnda_adm0_2019.shp"
  )
)
map <- shapefile(
  file.path(
    dropbox_dir,
    "Data/Raw/shapefile",
    "jpn_admbnda_adm1_2019.shp"
  )
)

admission_df_avg <- admission_df %>% 
  group_by(prefecture) %>% 
  summarise_at(vars(admission_total_share), mean)

map@data$ADM1_JA <- str_replace(map@data$ADM1_JA, "県|府", "")
map@data$ADM1_JA <- str_replace(map@data$ADM1_JA, "東京都", "東京")

map_avg <- merge(map, admission_df_avg, by.x = "ADM1_JA", by.y = "prefecture")

filepath = file.path(git_dir, 'Output/images/admission_map.pdf')
pdf(file = filepath)
print(
  spplot(
    map_avg, 
    "admission_total_share", 
    sp.layout = list(map_Japan, lwd = 0.1), 
    col.regions = brewer.pal(n = 9, name = "OrRd"),
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

