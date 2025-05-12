# This R script performs modeling on the Spotify dataset to predict valence.
# It includes both linear regression and random forest, evaluating performance
# and visualizing feature importance.

# Load required packages
library(magrittr)  # Load {magrittr} package for piping operations (%>%)
library(here)  # Load {here} package for file path management
library(tidyverse)  # Load {tidyverse} for data manipulation and plotting
library(randomForest) # Load {randomForest} for random Forest model
library(caret) # Load {caret} for train/test splitting and evaluation

# Identify project location
here::i_am("03-analyses/03-spotify-valence-replication/03-model-valence.R")

#===============================================================================
# Load Data ----
# ==============================================================================

# ------------------------------------------------------------------------------
# Load cleaned Spotify dataset ----
# ------------------------------------------------------------------------------

# Read the spotify dataset from the derived data folder
spotify <- readr::read_csv(
  here("02-data", "03-spotify-valence-replication", "derived", 
       "2021-04-release-spotify-data-cleaned.csv")
)

#===============================================================================
# Modelling ----
# ==============================================================================

# ------------------------------------------------------------------------------
# Prepare features and train/test split ----
# ------------------------------------------------------------------------------

set.seed(7914)
spotify_model <- spotify %>%
  select(valence, danceability, energy, acousticness, instrumentalness,
         loudness, speechiness, liveness, tempo, key, mode)

# Create train/test split
train_index <- createDataPartition(spotify_model$valence, p = 0.7, list = FALSE)
train_data <- spotify_model[train_index, ]
test_data <- spotify_model[-train_index, ]

# ------------------------------------------------------------------------------
# Linear Regression Model ----
# ------------------------------------------------------------------------------

lm_model <- lm(valence ~ ., data = train_data)
summary(lm_model)

# Predict on test data
lm_predictions <- predict(lm_model, newdata = test_data)

# Evaluate
lm_mae <- MAE(lm_predictions, test_data$valence)
lm_mse <- mean((lm_predictions - test_data$valence)^2)
lm_rmse <- RMSE(lm_predictions, test_data$valence)
lm_r2   <- R2(lm_predictions, test_data$valence)

# ------------------------------------------------------------------------------
# Random Forest Model ----
# ------------------------------------------------------------------------------

set.seed(7914)
rf_model <- randomForest(valence ~ ., data = train_data, ntree = 200, importance = TRUE)

# Predict on test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate
rf_mae <- MAE(rf_predictions, test_data$valence)
rf_mse <- mean((rf_predictions - test_data$valence)^2)
rf_rmse <- RMSE(rf_predictions, test_data$valence)
rf_r2   <- R2(rf_predictions, test_data$valence)

# ------------------------------------------------------------------------------
# Save Model Performance Metrics for LM and RF ----
# ------------------------------------------------------------------------------

# Create tidy metrics table
model_metrics <- tibble::tibble(
  Model = c("Linear Regression", "Random Forest"),
  MAE = c(lm_mae, rf_mae),
  MSE = c(lm_mse, rf_mse),
  RMSE = c(lm_rmse, rf_rmse),
  R_squared = c(lm_r2, rf_r2)
)

# Save as CSV
write_csv(
  model_metrics,
  here("04-outputs", "03-spotify-valence-replication", 
       "Table1-model-performance-metrics.csv")
)

# ------------------------------------------------------------------------------
# Feature Importance Plot ----
# ------------------------------------------------------------------------------

rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(Feature = rownames(rf_importance),
                               Importance = rf_importance[, "%IncMSE"]) %>%
  arrange(desc(Importance))

# Save feature importance plot
importance_plot <- ggplot(rf_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "#1DB954") +
  coord_flip() +
  labs(x = "Feature", y = "% Increase in MSE") +
  theme_minimal(base_size = 14)

ggsave(
  filename = here("04-outputs", "03-spotify-valence-replication", 
                  "Figure6-rf-feature-importance.png"),
  plot = importance_plot, 
  width = 7, 
  height = 5
)