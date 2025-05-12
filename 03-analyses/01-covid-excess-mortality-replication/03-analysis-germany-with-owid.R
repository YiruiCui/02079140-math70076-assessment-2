# This R script performs analysis of excess mortality for Germany using
# World Mortality Dataset (weekly data) and OWID COVID-19 reported deaths.


# Load required packages
library(magrittr)  # Load {magrittr} package for piping operations (%>%)
library(here)  # Load {here} package for file path management
library(tidyverse)  # Load {tidyverse} for data manipulation and plotting

# Identify project location
here::i_am("03-analyses/01-covid-excess-mortality-replication/03-analysis-germany-with-owid.R")

#===============================================================================
# Load Data ----
# ==============================================================================

# ------------------------------------------------------------------------------
## Load cleaned world mortality and owid covid data ----
# ------------------------------------------------------------------------------

# Read the world mortality data from the derived data folder
world_mortality <- readr::read_csv(
  here("02-data", "01-covid-excess-mortality-replication", 
       "derived", "2025-04-16-world-mortality-cleaned.csv")
)

# Read the owid covid-19 data from the derived data folder
owid_covid <- readr::read_csv(
  here("02-data", "01-covid-excess-mortality-replication", 
       "derived", "2024-08-19-owid-covid-cleaned.csv")
)

# Select world mortality data in Germany from 2015 to 2019 ----
model_data <- world_mortality %>%
  filter(country_name == "Germany", year >= 2015, year <= 2019)

# Select owid covid-19 data in Germany from 2020 to 2023
owid_germany <- owid_covid %>%
  filter(location == "Germany", year >= 2020, year <= 2023) %>%
  group_by(year, time) %>%
  summarise(owid_covid_deaths = sum(new_deaths, na.rm = TRUE), .groups = "drop") %>%
  distinct()

#===============================================================================
# Modelling ----
# ==============================================================================

# ------------------------------------------------------------------------------
## Fit seasonal linear model ----
# ------------------------------------------------------------------------------

model <- lm(deaths ~ year + factor(time), data = model_data)

# ------------------------------------------------------------------------------
## Predict expected deaths for 2020–2023 ----
# ------------------------------------------------------------------------------

# Select world mortality data in Germany from 2020 to 2023
data_actual <- world_mortality %>%
  filter(country_name == "Germany", year >= 2020, year <= 2023)

# Predict expected deaths using trained model and calculate excess death
data_expected <- data_actual %>%
  mutate(
    predict_deaths = predict(model, newdata = data_actual),
    excess_deaths = deaths - predict_deaths,
    p_score = 100 * (excess_deaths / predict_deaths)
  )

# ------------------------------------------------------------------------------
## Merge with OWID COVID Data ----
# ------------------------------------------------------------------------------

data_merged <- data_expected %>%
  left_join(owid_germany, by = c("year", "time")) %>%
  mutate(owid_covid_deaths = replace_na(owid_covid_deaths, 0))

# ------------------------------------------------------------------------------
## Summary statistics per year ----
# ------------------------------------------------------------------------------

# Obtain summary statistics per year
summary_stats <- data_merged %>%
  group_by(year) %>%
  summarise(
    total_excess_deaths = round(sum(excess_deaths, na.rm = TRUE)),
    peak_p_score = round(max(p_score, na.rm = TRUE), 1),
    total_covid_deaths = round(sum(owid_covid_deaths, na.rm = TRUE))
  )

# Write the summary statistics to a CSV file
readr::write_csv(
  summary_stats,
  here("04-outputs/01-covid-excess-mortality-replication", 
       "Table1-germany-summary-statistics.csv"),
)

# Compute total excess deaths across all years and print it out
total_excess_deaths <- data_merged %>%
  filter(year >= 2020, year <= 2023) %>%
  summarise(total = round(sum(excess_deaths, na.rm = TRUE))) %>%
  pull(total)
print(total_excess_deaths)

#===============================================================================
# Make plots ----
# ==============================================================================

# ------------------------------------------------------------------------------
## Make plot of observed vs expected deaths
# ------------------------------------------------------------------------------
p1 <- ggplot(data_merged, aes(x = time + (year - 2020) * 52)) +
  geom_line(aes(y = deaths), color = "black", linewidth = 1) +
  geom_line(aes(y = predict_deaths), color = "blue", linetype = "dashed") +
  labs(
    title = "Germany: Weekly Deaths vs. Expected Deaths (2020–2023)",
    x = "Week Index (from 2020)",
    y = "Number of Deaths"
  )

# ------------------------------------------------------------------------------
## Make plot of excess deaths
# ------------------------------------------------------------------------------
p2 <- ggplot(data_merged, aes(x = time + (year - 2020) * 52, y = excess_deaths)) +
  geom_col(fill = "firebrick") +
  labs(
    title = "Germany: Weekly Excess Deaths",
    x = "Week Index (from 2020)",
    y = "Excess Deaths"
  )

# ------------------------------------------------------------------------------
## Make plot of P-score
# ------------------------------------------------------------------------------
p3 <- ggplot(data_merged, aes(x = time + (year - 2020) * 52, y = p_score)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Germany: Weekly P-score (Excess Deaths %)",
    x = "Week Index (from 2020)",
    y = "P-score (%)"
  )

# ------------------------------------------------------------------------------
## Make plot of excess vs reported covid deaths
# ------------------------------------------------------------------------------
p4 <- ggplot(data_merged, aes(x = time + (year - 2020) * 52)) +
  geom_line(aes(y = excess_deaths), color = "firebrick", linewidth = 1) +
  geom_line(aes(y = owid_covid_deaths), color = "darkblue", linewidth = 1, linetype = "dotted") +
  labs(
    title = "Germany: Excess Deaths vs. Reported COVID Deaths",
    x = "Week Index (from 2020)",
    y = "Number of Deaths"
  )

# ==============================================================================
# Save all plots ----
# ==============================================================================
ggsave(
  filename = here("04-outputs/01-covid-excess-mortality-replication", 
                  "Figure1-germany-deaths-vs-expected.png"), 
  plot = p1, 
  width = 10, 
  height = 5
)
ggsave(
  filename = here("04-outputs/01-covid-excess-mortality-replication", 
                  "Figure2-germany-excess-deaths.png"), 
  plot = p2, 
  width = 10, 
  height = 5
)
ggsave(
  filename = here("04-outputs/01-covid-excess-mortality-replication", 
                  "Figure3-germany-p-score.png"), 
  plot = p3, 
  width = 10, 
  height = 5
)
ggsave(
  filename = here("04-outputs/01-covid-excess-mortality-replication", 
                  "Figure4-germany-excess-vs-covid.png"), 
  plot = p4, 
  width = 10, 
  height = 5
)