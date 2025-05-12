# This R script loads, cleans, and saves the Spotify music dataset 

# Load required packages
library(magrittr)  # Load {magrittr} package for piping operations (%>%)
library(here)  # Load {here} package for file path management
library(tidyverse)  
# Load {tidyverse} package for data manipulation and missing value handling

# Identify project location
here::i_am("03-analyses/03-spotify-valence-replication/01-clean-spotify-data.R")

# ==============================================================================
# Obtain cleaned data
# ==============================================================================

# ------------------------------------------------------------------------------
## Load Data ----
# ------------------------------------------------------------------------------

# Read mushrooms data with semicolon-delimited format from the raw folder
uncleaned_spotify <- readr::read_csv(
  here("02-data", "03-spotify-valence-replication", 
       "raw", "2021-04-release-spotify-data.csv")
)

# ------------------------------------------------------------------------------
# Clean Data ----
# ------------------------------------------------------------------------------

set.seed(7914) # for reproducibility

cleaned_spotify <- uncleaned_spotify %>%
  # Drop unnecessary columns
  select(-c(artists, name, id, explicit, 
            popularity, release_date, duration_ms)) %>%
  
  # Remove duplicate entries
  distinct() %>%
  
  # Drop NA rows in key features
  drop_na(valence, year, acousticness, danceability, energy,
          instrumentalness, key, liveness, loudness, mode,
          speechiness, tempo) %>%
  
  # Convert year, genre, key and mode to factors
  mutate(
    year = as.integer(year),
    key = as.factor(key),            
    mode = as.factor(mode)           
  )%>%
  
  # Randomly sample 10,000 observations to reduce memory footprint
  sample_n(10000)

# ------------------------------------------------------------------------------
# Save Cleaned Dataset ----
# ------------------------------------------------------------------------------

# Write the cleaned dataset to a new CSV file
readr::write_csv(
  cleaned_spotify,
  here("02-data", "03-spotify-valence-replication",
       "derived", "2021-04-release-spotify-data-cleaned.csv")
)