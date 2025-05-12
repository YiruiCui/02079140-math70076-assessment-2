# This R script cleans OWID COVID-19 global data (cases, deaths, testing etc.)
# and saves a simplified version with selected key variables.

# Load required packages
library(magrittr)  # Load {magrittr} package for piping operations (%>%)
library(here)  # Load {here} package for file path management
library(tidyverse)  
# Load {tidyverse} package for data manipulation and missing value handling

# Identify project location
here::i_am("03-analyses/01-covid-excess-mortality-replication/02-clean-owid-covid-data.R")

# ==============================================================================
# Obtain cleaned data
# ==============================================================================

# ------------------------------------------------------------------------------
## Load Data ----
# ------------------------------------------------------------------------------

# Read OWID COVID-19 dataset from raw folder
uncleaned_owid_data <- readr::read_csv(
  here("02-data", "01-covid-excess-mortality-replication", 
       "raw", "2024-08-19-release-owid-covid-data.csv")
)

# ------------------------------------------------------------------------------
## Extract and clean selected data ----
# ------------------------------------------------------------------------------

cleaned_owid_data <- 
  uncleaned_owid_data %>% 
  
  # Select required columns
  select(location, date, new_deaths
  ) %>%
  
  # Convert date column to proper date format
  mutate(
    date = lubridate::ymd(date),
    year = lubridate::year(date),
    time = lubridate::isoweek(date),
    
    # Convert new_deaths columns to numeric type
    new_deaths = as.numeric(new_deaths)
  ) %>%
  
  # Remove rows with missing values (NA) in location or date
  drop_na(location, date) %>% 

  # Replace missing values (NA) in new_deaths by 0
  mutate(
    new_deaths = replace_na(new_deaths, 0)
  )

# ------------------------------------------------------------------------------
## Save Cleaned Data ----
# ------------------------------------------------------------------------------

# Write the cleaned dataset to a new CSV file
readr::write_csv(
  cleaned_owid_data,
  here("02-data", "01-covid-excess-mortality-replication", 
       "derived", "2024-08-19-owid-covid-cleaned.csv")
)
