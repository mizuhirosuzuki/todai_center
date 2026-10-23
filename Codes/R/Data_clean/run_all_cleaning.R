packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl",
  "lfe",
  "stargazer",
  "RStata"
)
pacman::p_load(packages, character.only = TRUE)

args <- commandArgs(trailingOnly = TRUE)
dropbox_dir <- args[1]
git_dir <- args[2]

# Download weather data
# source(file.path(git_dir, "Codes/R/Data_clean/weather_download.R"))

# Cleaning weather data
source(file.path(git_dir, "Codes/R/Data_clean/weather_clean.R"))

# Cleaning admission data
source(file.path(git_dir, "Codes/R/Data_clean/admission_clean.R"))

# Merge data
source(file.path(git_dir, "Codes/R/Data_clean/merge_data.R"))
