#### Preamble ####
# Purpose: Downloads and saves the data from Open Government 
# Author: Sirui Tan 
# Date: 21 March 2024 
# Contact: sirui.tanr@utoronto.ca 
# License: MIT
# Pre-requisites: NO
# Any other information needed? NO


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)


#### Download data ####
# Load necessary library
library(httr)

library(httr)

# Define the URL of the ZIP file
zip_url <- "https://www150.statcan.gc.ca/n1/tbl/csv/11100192-eng.zip"

# Define the folder where you want to save the downloaded file
download_folder <- "data/raw_data/"

# Create the directory if it doesn't exist
if (!file.exists(download_folder)) {
  dir.create(download_folder, recursive = TRUE)
}

# Define the file path for the downloaded ZIP file
zip_file <- paste0(download_folder, "11100192-eng.zip")

# Download the ZIP file to the specified folder
download.file(zip_url, destfile = zip_file, mode = "wb")

# Unzip the contents of the ZIP file
unzip(zip_file, exdir = download_folder)

list.files(download_folder)

raw_data <- read_csv(here::here("data/raw_data/11100192.csv"),show_col_types = FALSE)

write_csv(raw_data, "data/raw_data/raw_data_income.csv")

         
