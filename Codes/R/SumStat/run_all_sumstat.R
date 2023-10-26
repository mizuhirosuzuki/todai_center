packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl",
  "lfe",
  "kableExtra",
  "raster",
  "RColorBrewer",
  "gridExtra",
  "grid",
  "stargazer"
)
pacman::p_load(packages, character.only = TRUE)

args <- commandArgs(trailingOnly = TRUE)
dropbox_dir <- args[1]
git_dir <- args[2]

# Summary statistics table
source(file.path(git_dir, "Codes/R/SumStat/sumstat.R"))

# Admission map
source(file.path(git_dir, "Codes/R/SumStat/admission_map.R"))

# Weather difference
source(file.path(git_dir, "Codes/R/SumStat/weather_diff.R"))
