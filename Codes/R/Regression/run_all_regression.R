packages <- c(
  "tidyverse",
  "curl",
  "rvest",
  "magrittr",
  "lubridate",
  "readxl",
  "lfe",
  "stargazer"
)
pacman::p_load(packages, character.only = TRUE)

args <- commandArgs(trailingOnly = TRUE)
dropbox_dir <- args[1]
git_dir <- args[2]

# Load regression data =================
reg_df <- readRDS(file.path(dropbox_dir, "Data/Processed/data_regression.rds"))

# Import function to make estimate figures
source(file.path(git_dir, "Codes/R/Regression/make_coef_figure.R"))

# Main regression
source(file.path(git_dir, "Codes/R/Regression/main_regression.R"))

# Appendix regression
source(file.path(git_dir, "Codes/R/Regression/appendix_regression.R"))
