# This R script loads, cleans, and saves the mushroom edibility dataset 
# for analysis using Random Forest model.

# Load required packages
library(magrittr)  # Load {magrittr} package for piping operations (%>%)
library(here)  # Load {here} package for file path management
library(tidyverse)  
# Load {tidyverse} package for data manipulation and missing value handling

# Identify project location
here::i_am("03-analyses/02-mushroom-edibility-analysis-replication/01-clean-mushrooms-data.R")

# ==============================================================================
# Obtain cleaned data
# ==============================================================================

# ------------------------------------------------------------------------------
## Load Data ----
# ------------------------------------------------------------------------------

# Read mushrooms data with semicolon-delimited format from the raw folder
uncleaned_mushrooms <- readr::read_delim(
  here("02-data", "02-mushroom-edibility-analysis-replication", 
       "raw", "1987-04-26-release-mushrooms-data.csv"),
  delim = ";"
)

# ------------------------------------------------------------------------------
## Extract and clean weekly data ----
# ------------------------------------------------------------------------------

cleaned_mushrooms <- 
  uncleaned_mushrooms %>% 
  
  # Replace hyphens with underscores in column names for compatibility
  rename_with(~ gsub("-", "_", .x)) %>%
  
  # Convert numerical feature columns to numeric
  mutate(
    cap_diameter = as.numeric(cap_diameter),
    stem_height = as.numeric(stem_height),
    stem_width = as.numeric(stem_width),
  ) %>%
  
  # Remove entries with missing class labels (target variable)
  drop_na(class)  %>%
  
  # Convert all NA values in all columns to "unknown"
  mutate(across(everything(), ~ ifelse(is.na(.), "unknown", .)))

# ------------------------------------------------------------------------------
## Save Cleaned Data ----
# ------------------------------------------------------------------------------

# Write the cleaned dataset to a new CSV file
readr::write_csv(
  cleaned_mushrooms,
  here("02-data", "02-mushroom-edibility-analysis-replication",
       "derived", "1987-04-26-release-mushrooms-data-cleaned.csv")
)
