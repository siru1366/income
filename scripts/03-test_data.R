#### Preamble ####
# Purpose: Tests clean data
# Author: Sirui Tan
# Date:23 March 2024 
# Contact: sirui.tan@utoronto.ca 
# License: MIT
# Pre-requisites: No
# Any other information needed? No


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)


# Load income data from Parquet file
income_data <- read_parquet(file = here::here("data/analysis_data/cleaned_income_data.parquet"))

# Define test cases
test_that("Non-negative income values", {
  expect_true(all(income_data$Income >= 0), "Some income values are negative")
})

test_that("Non-negative income values", {
  expect_true(all(income_data$`Income Range` >= 0), "Some income ranges are negative")
})

test_that("Exactly 11 unique types of Income decile", {
  unique_income_deciles <- unique(income_data$`Income decile`)
  expect_true(length(unique_income_deciles) == 11, "Number of unique types of Income deciles is not 11")
})


test_that("Correct years range", {
  expect_true(all(income_data$Year >= 1976), "Minimum year is incorrect")
  expect_true(all(income_data$Year <= 2021), "Maximum year is incorrect")
})

test_that("Exactly 13 unique types of geographical locations", {
  unique_location_types <- unique(income_data$`Geographical Location`)
  expect_true(length(unique_location_types) == 13, "Number of unique types of geographical locations is not 13")
})


