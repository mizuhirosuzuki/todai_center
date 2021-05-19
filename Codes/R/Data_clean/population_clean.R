packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl"
)
pacman::p_load(packages, character.only = TRUE)

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

population_clean_func <- function(year) {
  
  print(year)
   
  if (year < 2020) {
    file_path <- file.path(
      dropbox_dir,
      str_interp("Data/population_data/${year}_population.xls")
      )
  } else {
    file_path <- file.path(
      dropbox_dir,
      str_interp("Data/population_data/2019_population.xls")
      )
  }

  if (year != 2015) {
    output_df <- read_excel(file_path, skip = 20, col_names = FALSE) %>% 
      head(47) %>% 
      select(10, 12, 13, 17) %>% 
      set_colnames(c("prefecture", "prefecture_eng", "total_pop", "pop_15_19")) %>% 
      mutate(
        across(
          c(total_pop, pop_15_19),
          ~ as.numeric(.)
        ),
        year = year
      )
  } else {
    output_df <- read_excel(file_path, skip = 18, col_names = FALSE) %>% 
      head(47) %>% 
      select(3, 5, 6, 10) %>% 
      set_colnames(c("prefecture", "prefecture_eng", "total_pop", "pop_15_19")) %>% 
      mutate(
        across(
          c(total_pop, pop_15_19),
          ~ as.numeric(.)
        ),
        year = year 
      )
  }
  
  return(output_df)
}

pop_df <- map(
  seq(2012, 2020),
  population_clean_func
  ) %>% 
  bind_rows() %>% 
  mutate(
    prefecture = str_replace(prefecture, "県", ""),
    prefecture = str_replace(prefecture, "府", ""),
    prefecture = str_replace(prefecture, "東京都", "東京"),
    prefecture = str_trim(prefecture, side = "both")
  ) %>% 
  mutate(
    total_pop = total_pop / 1000,
    pop_15_19 = pop_15_19 / 1000
  )
     
# file_path <- file.path(
#   dropbox_dir,
#   str_interp("Data/population_data/${2012}_population.xls")
#   )
# pop_df <- read_excel(file_path, skip = 20, col_names = FALSE) %>% 
#   head(47) %>% 
#   select(10, 13, 17) %>% 
#   set_colnames(c("prefecture", "total_pop", "pop_15_19")) %>% 
#   mutate(
#     across(
#       c(total_pop, pop_15_19),
#       ~ as.numeric(.)
#     )
#   ) %>% 
#   mutate(
#     prefecture = str_replace(prefecture, "県", ""),
#     prefecture = str_replace(prefecture, "府", ""),
#     prefecture = str_replace(prefecture, "東京都", "東京"),
#     prefecture = str_trim(prefecture, side = "both")
#   )

write_csv(pop_df, file.path(dropbox_dir, "Data/population_data.csv"))

pop_df %>% 
  group_by(prefecture_eng) %>% 
  # mutate(avg_total_pop = mean(total_pop)) %>%
  # mutate(dev_total_pop = total_pop - avg_total_pop) %>%
  summarise_at(vars(total_pop), mean, na.rm = TRUE) %>%
  ggplot(aes(x = prefecture_eng, y = total_pop)) +
  geom_point() +
  coord_flip()

pop_df %>% 
  group_by(prefecture_eng) %>% 
  mutate(avg_total_pop = mean(total_pop)) %>%
  mutate(dev_total_pop = (total_pop - avg_total_pop) / avg_total_pop) %>%
  # summarise_at(vars(total_pop), mean, na.rm = TRUE) %>%
  # ggplot(aes(x = prefecture_eng, y = total_pop)) +
  ggplot(aes(x = prefecture_eng, y = dev_total_pop, color = year)) +
  geom_point() +
  coord_flip()

