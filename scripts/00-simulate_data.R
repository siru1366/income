#### Preamble ####
# Purpose: Simulates Upper income limit, income share and average income by economic family type and income decile
# Author: Sirui Tan
# Date: 20 March 2024  
# Contact: sirui.tan@utoronto.ca 
# License: MIT
# Pre-requisites: NO
# Any other information needed? NO


#### Workspace setup ####
library(tidyverse)
library(testthat)


#### Simulate data ####
set.seed(853)


# Create an empty tibble to store simulated data
simulated_data <- tibble(
  Year = rep(1971:2021, each = 143),
  `Geographical Location` = rep(c(
    rep("Canada", 11),
    rep("Atlantic provinces", 11),
    rep("Newfoundland and Labrador", 11),
    rep("Prince Edward Island", 11),
    rep("Nova Scotia", 11),
    rep("New Brunswick", 11),
    rep("Quebec", 11),
    rep("Ontario", 11),
    rep("Prairie provinces", 11),
    rep("Manitoba", 11),
    rep("Saskatchewan", 11),
    rep("Alberta", 11),
    rep("British Columbia", 11)
  ), 51),
  `Income decile` = rep(
    c(
      "Total deciles", "Lowest decile", "Second decile", "Third decile", "Fourth decile",
      "Fifth decile", "Sixth decile", "Seventh decile", "Eighth decile", "Ninth decile",
      "Highest decile"
    ), 663
  ),
  Income = rep(0, 7293),
  `Income range` = rep(0, 7293),
  `Income ration` = rep(0, 7293)
)


for (i in 1:nrow(simulated_data)) {
  if (simulated_data[i, "Income decile"] == "Total deciles") {
    simulated_data[i, "Income"] <- runif(1, 5000, 150000)
  } else {
    # Calculate simulated income for each decile based on the previous decile's income
    previous_income <- simulated_data[i - 1, "Income"]
    simulated_data[i, "Income"] <- previous_income * runif(1, 1.05, 1.1)  # Simulate some growth
  }
}

# Calculate income range for each total decile
for (i in 1:nrow(simulated_data)) {
  if (simulated_data[i, "Income decile"] == "Total deciles") {
    lowest_decile_income <- simulated_data[i + 1, "Income"]
    highest_decile_income <- simulated_data[i + 10, "Income"]
    simulated_data[i, "Income range"] <- highest_decile_income - lowest_decile_income
    simulated_data[i, "Income ration"] <- highest_decile_income /lowest_decile_income
  }
}



# Test 1: Check if all income values are non-negative
test_that("Non-negative income values", {
  expect_true(all(simulated_data$Income >= 0), "Some income values are negative")
})
# Test 2: Check if all rows have the correct geographical location
test_that("Correct geographical location representation", {
  expected_locations <- rep(c(
    rep("Canada", 11),
    rep("Atlantic provinces", 11),
    rep("Newfoundland and Labrador", 11),
    rep("Prince Edward Island", 11),
    rep("Nova Scotia", 11),
    rep("New Brunswick", 11),
    rep("Quebec", 11),
    rep("Ontario", 11),
    rep("Prairie provinces", 11),
    rep("Manitoba", 11),
    rep("Saskatchewan", 11),
    rep("Alberta", 11),
    rep("British Columbia", 11)
  ), 51)
  expect_equal(simulated_data$`Geographical Location`, expected_locations, "Geographical location representation is incorrect")
})
# Test 3: Check if the Year column contains the correct range of years
test_that("Correct years range", {
  expect_true(all(simulated_data$Year > 1970), "Minimum year is incorrect")
  expect_true(all(simulated_data$Year < 2022), "Maximum year is incorrect")
})

# Test 4: Check if there are exactly 13 unique types of geographical locations
test_that("Exactly 13 unique types of geographical locations", {
  unique_location_types <- unique(simulated_data$`Geographical Location`)
  expect_true(length(unique_location_types) == 13, "Number of unique types of geographical locations is not 13")
})

# Test 5: Check if there are exactly 11 unique types of Income deciles
test_that("Exactly 11 unique types of Income decile", {
  unique_location_types <- unique(simulated_data$`Income decile`)
  expect_true(length(unique_location_types) == 11, "Number of unique types of Income deciles is not 11")
})





