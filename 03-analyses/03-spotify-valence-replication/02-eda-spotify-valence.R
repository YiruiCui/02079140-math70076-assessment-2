# This R script performs exploratory data analysis (EDA) on the Spotify dataset.
# It reproduces visualizations from the replication study: valence distribution,
# trends over time, feature correlations, and scatter relationships.

# Load required packages
library(magrittr)  # Load {magrittr} package for piping operations (%>%)
library(here)  # Load {here} package for file path management
library(tidyverse)  # Load {tidyverse} for data manipulation and plotting
library(GGally)     # Load {GGally} for pair plots
library(corrplot)   # Load {corrplot} For correlation heatmaps

# Identify project location
here::i_am("03-analyses/03-spotify-valence-replication/02-eda-spotify-valence.R")

#===============================================================================
# Load Data ----
# ==============================================================================

# ------------------------------------------------------------------------------
## Load cleaned Spotify dataset ----
# ------------------------------------------------------------------------------

# Read the spotify dataset from the derived data folder
spotify <- readr::read_csv(
  here("02-data", "03-spotify-valence-replication", 
       "derived", "2021-04-release-spotify-data-cleaned.csv")
)

# ==============================================================================
# Make plots ----
# ==============================================================================

# ------------------------------------------------------------------------------
## Plot 1: Histogram of Valence ----
# ------------------------------------------------------------------------------

valence_plot <- ggplot(spotify, aes(x = valence)) +
  geom_histogram(binwidth = 0.05, fill = "#1DB954", color = "white", alpha = 0.9) +
  labs(x = "Valence", y = "Count") +
  theme_bw(base_size = 13)

# ------------------------------------------------------------------------------
## Plot 2: Average Valence by Year ----
# ------------------------------------------------------------------------------

valence_year_plot <- spotify %>%
  filter(year >= 1921, year <= 2020) %>%
  group_by(year) %>%
  summarise(avg_valence = mean(valence, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_valence)) +
  geom_line(color = "#1DB954", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  labs(x = "Year", y = "Average Valence") +
  theme_bw(base_size = 13)

# ------------------------------------------------------------------------------
## Plot 3: Scatter Plot of Danceability vs Valence ----
# ------------------------------------------------------------------------------

scatter_plot <- ggplot(spotify, aes(x = danceability, y = valence)) +
  geom_point(alpha = 0.3, size = 1, color = "#1DB954") +
  labs(x = "Danceability", y = "Valence") +
  theme_bw(base_size = 13)

# ------------------------------------------------------------------------------
## Plot 4: Pair Plot – Audio Features ----
# ------------------------------------------------------------------------------

selected_features <- spotify %>%
  select(valence, danceability, energy, acousticness)

pair_plot <- ggpairs(
  selected_features,
  upper = list(continuous = wrap("points", alpha = 0.2, size = 0.5, color = "#1DB954")),
  diag  = list(continuous = wrap("barDiag", binwidth = 0.05, fill = "#1DB954")),
  lower = list(continuous = wrap("smooth", alpha = 0.6, size = 0.6, color = "gray40"))
) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15)  
  )

# ------------------------------------------------------------------------------
## Plot 5: Correlation Heatmap (Upper Triangle) ----
# ------------------------------------------------------------------------------

cor_matrix <- spotify %>%
  select(valence, danceability, energy, loudness, speechiness,
         acousticness, instrumentalness, liveness, tempo) %>%
  cor(use = "complete.obs")

png(
  filename = here("04-outputs", "03-spotify-valence-replication", 
                  "Figure5-correlation-matrix.png"),
  width = 800, height = 800
)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", cl.cex = 1.5, tl.cex = 1.5, addCoef.col = "black")
dev.off()

# ==============================================================================
# Save all plots ----
# ==============================================================================

ggsave(
  filename = here("04-outputs", "03-spotify-valence-replication", 
                  "Figure1-valence-distribution.png"),
  plot = valence_plot, 
  width = 8, 
  height = 4
)

ggsave(
  filename = here("04-outputs", "03-spotify-valence-replication", 
                  "Figure2-valence-trend-year.png"),
  plot = valence_year_plot, 
  width = 8, 
  height = 4
)

ggsave(
  filename = here("04-outputs", "03-spotify-valence-replication", 
                  "Figure3-scatter-danceability-valence.png"),
  plot = scatter_plot, 
  width = 6, 
  height = 5
)

ggsave(
  filename = here("04-outputs", "03-spotify-valence-replication", 
                  "Figure4-pairplot-valence.png"),
  plot = pair_plot, 
  width = 8, 
  height = 8
)
