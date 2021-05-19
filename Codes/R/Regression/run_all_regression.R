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

dropbox_dir <- "~/Dropbox/todai_center/"
git_dir <- "~/Documents/GitHub/todai_center/"

# Load regression data =================
reg_df <- readRDS(file.path(dropbox_dir, "Data/temp/data_regression.rds"))

# Import function to make estimate figures
source(file.path(git_dir, "Codes/R/Regression/make_coef_figure.R"))

# Main regression
source(file.path(git_dir, "Codes/R/Regression/main_regression.R"))

# Placebo regression
source(file.path(git_dir, "Codes/R/Regression/placebo_regression.R"))

# Regression with weather for the previous 10 days
source(file.path(git_dir, "Codes/R/Regression/pre10_regression.R"))

# Linear regression
source(file.path(git_dir, "Codes/R/Regression/linear_regression.R"))

# Regression by gender
source(file.path(git_dir, "Codes/R/Regression/gender_regression.R"))

# Regression by major
source(file.path(git_dir, "Codes/R/Regression/art_science_regression.R"))

# Regression (morning vs. during exam)
source(file.path(git_dir, "Codes/R/Regression/morning_exam_regression.R"))



