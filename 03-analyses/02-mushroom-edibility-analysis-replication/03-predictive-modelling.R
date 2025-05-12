# This R script trains Random Forest models on the mushroom edibility dataset
# using different train/test splits and tuned hyperparameters. It also outputs
# accuracy results and visualizes feature importance using Gini index.

# Load required packages
library(magrittr)  # Load {magrittr} package for piping operations (%>%)
library(here)  # Load {here} package for file path management
library(tidyverse)  # Load {tidyverse} for data manipulation and plotting
library(randomForest) # Load {randomForest} for random Forest model
library(caret) # Load {caret} for train/test splitting and evaluation
library(reshape2)  # Load {reshape2} for for melt()

# Identify project location
here::i_am("03-analyses/02-mushroom-edibility-analysis-replication/02-exploratory-data-analysis.R")

#===============================================================================
# Load Data ----
# ==============================================================================

# ------------------------------------------------------------------------------
## Load and prepare cleaned mushrooms data ----
# ------------------------------------------------------------------------------

# Read the mushrooms data from the derived data folder
mushrooms <- readr::read_csv(
  here("02-data", "02-mushroom-edibility-analysis-replication",
       "derived", "1987-04-26-release-mushrooms-data-cleaned.csv"))

# Identify numeric and categorical features
num_features <- c("cap_diameter", "stem_height", "stem_width")
cat_features <- setdiff(names(mushrooms), c(num_features, "class"))

# Convert class to factor
mushrooms$class <- as.factor(mushrooms$class)

# Encode categorical features as factors
mushrooms[cat_features] <- lapply(mushrooms[cat_features], as.factor)

# Standardize numeric features
mushrooms[num_features] <- scale(mushrooms[num_features])

# Combine into final dataset for modeling
rf_data <- mushrooms %>%
  select(class, all_of(cat_features), all_of(num_features))

#===============================================================================
# Modelling ----
# ==============================================================================

# ------------------------------------------------------------------------------
## Define function to train and evaluate random forest ----
# ------------------------------------------------------------------------------

evaluate_rf_model <- function(data, p = 0.7, ntree = 100, seed = 7914) {
  set.seed(seed)  # Set seed for reproducibility
  
  # Split data into training and testing sets
  train_idx <- createDataPartition(data$class, p = p, list = FALSE)
  train_data <- data[train_idx, ] %>% na.omit()
  test_data  <- data[-train_idx, ] %>% na.omit()
  
  # Train Random Forest model
  rf_model <- randomForest(class ~ ., data = train_data, ntree = ntree)
  
  # Make predictions and compute accuracy
  predictions <- predict(rf_model, test_data)
  conf_matrix <- confusionMatrix(predictions, test_data$class)
  accuracy <- conf_matrix$overall["Accuracy"]
  
  # Return results as a list
  return(list(
    accuracy = accuracy,
    model = rf_model,
    confusion_matrix = conf_matrix
  ))
}

# ------------------------------------------------------------------------------
# Train and evaluate models with varying train/test Splits and tuned hyperparameters
# ------------------------------------------------------------------------------

# Run with 1% dataset for training and 99% for testing
result_1 <- evaluate_rf_model(rf_data, p = 0.01, ntree = 750)

# Run with 10% dataset for training and 90% for testing
result_2 <- evaluate_rf_model(rf_data, p = 0.1, ntree = 330)

# Run with 30% dataset for training and 70% for testing
result_3 <- evaluate_rf_model(rf_data, p = 0.3, ntree = 200)

# Run with 70% dataset for training and 30% for testing
result_4 <- evaluate_rf_model(rf_data, p = 0.7, ntree = 100)

# ------------------------------------------------------------------------------
# Save accuracy results as a csv file
# ------------------------------------------------------------------------------

# Collect accuracy results into a data frame
accuracy_df <- tibble::tibble(
  TrainSplit = c("1%", "10%", "30%", "70%"),
  TestSplit  = c("99%", "90%", "70%", "30%"),
  ntree      = c(750, 330, 200, 100),
  Accuracy   = c(result_1$accuracy,
                 result_2$accuracy,
                 result_3$accuracy,
                 result_4$accuracy) %>% round(6)
)

# Write the accuracy results to a CSV file
readr::write_csv(
  accuracy_df,
  here("04-outputs/02-mushroom-edibility-analysis-replication", 
       "Table1-accuracy-results.csv"),
)

# ------------------------------------------------------------------------------
# Grid Search for fit-tuning hyperparameters
# ------------------------------------------------------------------------------

## Define ntree values to test
#ntree_grid <- seq(100, 500, by = 100)
#
## Initialize tracking variables
#best_acc <- 0
#best_model <- NULL
#best_params <- list()
#
## Loop over ntree values
#for (ntree_val in ntree_grid) {
#  result <- evaluate_rf_model(rf_data, p = 0.7, ntree = ntree_val)
#  
#  acc <- result$accuracy
#  cat("ntree =", ntree_val, "-> Accuracy:", round(acc, 7), "\n")
#  
#  if (acc > best_acc) {
#    best_acc <- acc
#    best_model <- result$model
#    best_params <- list(ntree = ntree_val)
#  }
#}
#
## Output best results
#cat("Best Accuracy:", round(best_acc, 7), "\n")
#print(best_params)

# ------------------------------------------------------------------------------
# Feature importance from best model (result 4)
# ------------------------------------------------------------------------------

importance_df <- importance(result_4$model, type = 2) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  arrange(desc(MeanDecreaseGini))

# ------------------------------------------------------------------------------
# Plot Gini-based feature importance
# ------------------------------------------------------------------------------

# Plot feature importance from Random Forest (Gini importance)
bar_chart_gini <- importance_df %>%
  arrange(MeanDecreaseGini) %>%
  ggplot(aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "#F28E2B", width = 0.7) +
  geom_text(aes(label = round(MeanDecreaseGini, 3)),
            hjust = -0.1, size = 3.5, color = "black") +
  coord_flip() +
  labs(x = NULL, y = "Mean Decrease in Gini") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(size = 15),
    axis.title.x = element_text(size = 12),
    panel.grid.major.y = element_blank()
  )

# Save plot
ggsave(
  filename = here("04-outputs/02-mushroom-edibility-analysis-replication", 
                  "Figure4-bar-chart-gini.png"), 
  plot = bar_chart_gini, 
  width = 10, 
  height = 8
)

# ------------------------------------------------------------------------------
# Confusion_matrix from best model (result 4)
# ------------------------------------------------------------------------------

# Extract confusion matrix table
cm <- result_4$confusion_matrix$table

# Convert to data frame
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Reference", "Prediction", "Freq")

# ------------------------------------------------------------------------------
# Plot confusion_matrix as heatmap
# ------------------------------------------------------------------------------

# Plot as heatmap
heatmap <- cm_df %>%
  ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6, color = "black") +
  scale_fill_gradient(low = "#fefefe", high = "#0073C2") +
  labs(x = "Actual", y = "Predicted") +
  theme_minimal(base_size = 14)

# Save plot
ggsave(here("04-outputs/02-mushroom-edibility-analysis-replication",
            "Figure5-confusion-matrix.png"),
       plot = heatmap, 
       width = 6, 
       height = 5)

