#### Preamble ####
# Purpose: Models the income range.
# Author: Sirui Tan
# Date: 23 March 2024 
# Contact: sirui.tan@utoronto.ca 
# License: MIT
# Pre-requisites: No
# Any other information needed? No


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(tidymodels)
library(dplyr)
library(arrow)

income_data <- read_parquet(file = here::here("data/analysis_data/cleaned_income_data.parquet"))
income_model <- income_data %>%
  filter(
    `Income decile` == "Total deciles",
    `Geographical_Location` != "Canada")
income_model <- income_model %>% 
  rename(Ratio= `Highest-to-Lowest_Average_Income_Ratio`)

# Assuming income_mode is your dataset

### Model data ####
# Assuming income_mode is your dataset
income_model$Atlantic_provinces <- ifelse(income_model$Geographical_Location == "Atlantic provinces", 1, 0)
income_model$Newfoundland_and_Labrador <- ifelse(income_model$Geographical_Location == "Newfoundland and Labrador", 1, 0)
income_model$Prince_Edward_Island <- ifelse(income_model$Geographical_Location == "Prince Edward Island", 1, 0)
income_model$Nova_Scotia <- ifelse(income_model$Geographical_Location == "Nova Scotia", 1, 0)
income_model$New_Brunswick <- ifelse(income_model$Geographical_Location == "New Brunswick", 1, 0)
income_model$Quebec <- ifelse(income_model$Geographical_Location == "Quebec", 1, 0)
income_model$Ontario <- ifelse(income_model$Geographical_Location == "Ontario", 1, 0)
income_model$Prairie_provinces <- ifelse(income_model$Geographical_Location == "Prairie provinces", 1, 0)
income_model$Manitoba <- ifelse(income_model$Geographical_Location == "Manitoba", 1, 0)
income_model$Alberta<- ifelse(income_model$Geographical_Location == "Alberta", 1, 0)
income_model$Saskatchewan<- ifelse(income_model$Geographical_Location == "Saskatchewan", 1, 0)
income_model$British_Columbia<- ifelse(income_model$Geographical_Location == "British Columbia", 1, 0)








income_range_model <- 
  stan_glm(
  formula = `Income_Range` ~ Year + Geographical_Location,
  data = income_model,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_aux = exponential(rate = 1, autoscale = TRUE),
  seed = 853
)



#### Save model ####
saveRDS(
  income_range_model,
  file = "models/income_range_model.rds"
)

model <- 
  stan_glm(
    formula = `Income_Range` ~ Year + Atlantic_provinces + Newfoundland_and_Labrador+Prince_Edward_Island +
      Nova_Scotia+ New_Brunswick+Ontario+Quebec+Prairie_provinces+Manitoba+Saskatchewan+Alberta+British_Columbia,
    data = income_model,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )



#### Save model ####
saveRDS(
  model,
  file = "models/model.rds"
)

model2 <- 
  stan_glm(
    formula = Ratio ~ Year + Atlantic_provinces + Newfoundland_and_Labrador+Prince_Edward_Island +
      Nova_Scotia+ New_Brunswick+Ontario+Quebec+Prairie_provinces+Manitoba+Saskatchewan+Alberta+British_Columbia,
    data = income_model,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )



#### Save model ####
saveRDS(
  model2,
  file = "models/model2.rds"
)





