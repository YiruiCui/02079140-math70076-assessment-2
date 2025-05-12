# This R script cleans World Mortality Dataset (weekly death counts by country)
# and saves the cleaned version for modeling excess mortality.

# Load required packages
library(magrittr)  # Load {magrittr} package for piping operations (%>%)
library(here)  # Load {here} package for file path management
library(tidyverse)  
# Load {tidyverse} package for data manipulation and missing value handling

# Identify project location
here::i_am("03-analyses/01-covid-excess-mortality-replication/01-clean-world-mortality-data.R")

# ==============================================================================
# Obtain cleaned data
# ==============================================================================

# ------------------------------------------------------------------------------
## Load Data ----
# ------------------------------------------------------------------------------

# Read the world mortality dataset from the raw folder
uncleaned_world_mortality <- readr::read_csv(
  here("02-data", "01-covid-excess-mortality-replication", 
       "raw", "2025-04-16-release-world-mortality.csv"),
)

# ------------------------------------------------------------------------------
## Extract and clean weekly data ----
# ------------------------------------------------------------------------------

cleaned_world_mortality <- 
  uncleaned_world_mortality %>% 
  
  # Select only weekly data
  filter(time_unit == "weekly") %>%
  
  # Select country, year, time(week number) and deaths columns
  select(country_name, year, time, deaths) %>% 
  # Convert all columns to numeric type
  mutate(
    year = as.integer(year),
    time = as.integer(time),
    deaths = as.numeric(deaths)
  ) %>%
  
  # Filter years of interest (from 2015 onward)
  filter(year >= 2015) %>%
  
  # Remove rows with missing values (NA)
  drop_na(country_name, year, time, deaths)


# ------------------------------------------------------------------------------
## Save Cleaned Data ----
# ------------------------------------------------------------------------------

# Write the cleaned dataset to a new CSV file
readr::write_csv(
  cleaned_world_mortality,
  here("02-data", "01-covid-excess-mortality-replication",
       "derived", "2025-04-16-world-mortality-cleaned.csv")
)
