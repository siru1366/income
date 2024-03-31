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
set.seed(10666)

income <- read_parquet(file = here::here("data/analysis_data/cleaned_income_data.parquet"))

### Model data ####
income_model <- income %>%
  filter(
    `Income decile` == "Total deciles",
    `Geographical Location` != "Canada")

income_model_split <- 
  initial_split(data = income_model, 
                prop = 0.80)
income_model_train <- training(income_model_split)
model1 <- lm(`Highest-to-Lowest Average Income Ratio`~Year+`Geographical Location`+Year*`Geographical Location`, data = income_model_train)



#### Save model ####
saveRDS(
  model1,
  file = "models/income_model.rds"
)

set.seed(10667)


sim_run_data_first_model_rstanarm <- stan_glm(
  formula = `Highest-to-Lowest Average Income Ratio` ~ Year + `Geographical Location`,
  data = income_model_train,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5),
  prior_intercept = normal(location = 0, scale = 2.5),
  prior_aux = exponential(rate = 1),
  seed = 853
)


saveRDS(
  sim_run_data_first_model_rstanarm,
  file = "models/sim_run_data_first_model_rstanarm.rds"
)

set.seed(10665)

#### Read data ####
income <- read_csv("data/analysis_data/cleaned_income_data.csv")

### Model data ####
income_model <- income %>%
  filter(
    `Income decile` == "Total deciles",
    `Geographical Location` != "Canada")

income_model_split <- 
  initial_split(data = income_model, 
                prop = 0.80)
income_model_train <- training(income_model_split)
model2 <- lm(`Income Range`~Year+`Geographical Location`+Year*`Geographical Location`, data = income_model_train)



#### Save model ####
saveRDS(
  model2,
  file = "models/income_range_model.rds"
)


set.seed(10663)

#### Read data ####
income <- read_csv("data/analysis_data/cleaned_income_data.csv")

### Model data ####
income_model <- income %>%
  filter(
    `Income decile` == "Total deciles",
    `Geographical Location` != "Canada")

income_model_split <- 
  initial_split(data = income_model, 
                prop = 0.80)
income_model_train <- training(income_model_split)
model2 <- lm(`Highest-to-Lowest Average Income Ratio`~Year+`Geographical Location`, data = income_model_train)



#### Save model ####
saveRDS(
  model2,
  file = "models/income_model2.rds"
)
