# GitHub repo for "Winter weather on exam dates and matriculation for a prestigious university in Japan"

This repo stores codes for reproducing the result in my paper "Winter weather on exam dates and matriculation for a prestigious university in Japan".

The R scripts for downloading the data, cleaning the data, and generating analysis results are in the `Codes/R` folder.

(I don't have an `renv` setup in this repo... sorry!)

To reproduce the results, you can use the `Makefile` of this repo.
For this, you need to change the paths in `Makefile`:
  - `dropbox_dir`: path for the folder containing the raw data (you can download the raw data I used for the analyses [here](https://drive.google.com/drive/folders/1X9IyNp56RrmChqoOXeK91CxH8l13vC44?usp=drive_link)).
  - `git_dir`: path for the folder to this git repo.

The `Makefile` contains several commands and you can run them in the following way:
  - `make analysis`: run R scripts to clean data and generate analysis results;
  - `make all`: run the R scripts above and also compile the main `.tex` file for the paper.

Please feel free to contact me if you find there is any issue.
You can leave a message on the issue page or send an email to me (mizuhiro.suzuki@gmail.com).
