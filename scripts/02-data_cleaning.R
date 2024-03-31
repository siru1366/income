#### Preamble ####
# Purpose: Cleans the raw data and rename the variables
# Author: Sirui Tan
# Date: 30 March 2024
# Contact: sirui.tan@utoronto.ca 
# License: MIT
# Pre-requisites: No
# Any other information needed? No

#### Workspace setup ####
library(arrow)
library(tidyverse)
library(dplyr)

#### Clean data ####
raw_data <- read_csv(file = here::here("data/raw_data/raw_data_income.csv"))

# Just keep some variables that may be of interest
raw_data <- raw_data %>%
  select(REF_DATE, GEO, `Economic family type`, `Income concept`, Statistics, `Income decile`,  VALUE)

# Rename some variables
raw_data <- raw_data %>% 
  rename(Year = REF_DATE, `Geographical Location` = GEO, Income = VALUE)

raw1_data <- raw_data %>%
  filter(`Income concept`%in% c("After-tax income"),
         Statistics == "Average income",
         `Economic family type` == "Economic families and persons not in an economic family") %>%
  select(!`Income concept`) %>%
  select(!Statistics)%>%
  select(!`Economic family type`)

raw2_data <- raw1_data %>%
  mutate(`Highest-to-Lowest Average Income Ratio` = ifelse(`Income decile` == "Total deciles", 0, NA))

for(i in 1:nrow(raw2_data)) {
  if (raw2_data[i,3] == "Total deciles") {
    if (raw2_data[i+1,3] == "Lowest decile" && raw2_data[i+10,3] == "Highest decile") {
      raw2_data[i, 5] <- ((raw2_data[i + 10, 4]  / raw2_data[i + 1, 4]) )
    
    } else{
      raw2_data[i,6] = NA
    }
  }
}

raw3_data <- raw2_data %>%
  mutate(` Income Range` = ifelse(`Income decile` == "Total deciles", 0, NA))

for(i in 1:nrow(raw3_data)) {
  if (raw3_data[i,3] == "Total deciles") {
    if (raw3_data[i+1,3] == "Lowest decile" && raw3_data[i+10,3] == "Highest decile") {
      raw3_data[i, 6] <- ((raw3_data[i + 10, 4]  - raw3_data[i + 1, 4]) )
      
    } else{
      raw3_data[i,6] = NA
    }
  }
}

cleaned_data <- raw3_data %>%
  drop_na(Year, `Geographical Location`, `Income decile`, Income)      

 

#### Save data ####
write_parquet(cleaned_data, "data/analysis_data/cleaned_income_data.parquet")

