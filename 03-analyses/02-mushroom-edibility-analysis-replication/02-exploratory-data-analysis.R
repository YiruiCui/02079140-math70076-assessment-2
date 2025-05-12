# This R script performs exploratory analysis for the mushroom edibility dataset,
# including visualizations and chi-squared feature importance analysis.

# Load required packages
library(magrittr)  # Load {magrittr} package for piping operations (%>%)
library(here)  # Load {here} package for file path management
library(tidyverse)  # Load {tidyverse} for data manipulation and plotting
library(FSelector) 
# Load {FSelector} for feature selection (e.g., chi-squared scoring)

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

# Identify all numerical features
num_features <- c("cap_diameter", "stem_height", "stem_width")

# Identify all categorical features (excluding numeric + target)
cat_features <- setdiff(names(mushrooms), c(num_features, "class"))

# Convert target and categorical features to factors
mushrooms <- mushrooms %>%
  mutate(across(all_of(cat_features), ~ as.factor(.))) %>%
  mutate(class = as.factor(class))

# ==============================================================================
# Make plots ----
# ==============================================================================

# ------------------------------------------------------------------------------
# Create Violin plots for numerical features (with boxplots overlay)
# ------------------------------------------------------------------------------

# Plot violin plots for numerical features
violin_plot <- mushrooms %>%
  pivot_longer(cols = all_of(num_features), names_to = "feature", values_to = "value") %>%
  ggplot(aes(x = class, y = value, fill = class)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.2) +
  facet_wrap(~ feature, scales = "free_y", ncol = 3) +
  labs(x = "Class (e = edible, p = poisonous)", y = NULL) +
  theme_bw() +
  theme(text = element_text(size = 15),
        legend.position = "none")

# ------------------------------------------------------------------------------
# Create Count plots for selected categorical features
# ------------------------------------------------------------------------------

# Select categorical features with more informative count plot
cat_features_use <- c("stem_root", "stem_surface", "veil_color", "spore_print_color",
                      "cap_shape", "cap_surface", "cap_color", "gill_attachment")

# Plot count plots
count_plot <- mushrooms %>%
  pivot_longer(cols = all_of(cat_features_use), names_to = "feature", values_to = "value") %>%
  filter(value != "unknown") %>%  # Drop unknown values only for the active feature column
  ggplot(aes(x = value, fill = class)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ feature, scales = "free", ncol = 4) +
  labs(x = NULL, y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 15))

# ------------------------------------------------------------------------------
# Create Count plots for all categorical features
# ------------------------------------------------------------------------------

count_plot_full <- mushrooms %>%
  pivot_longer(cols = all_of(cat_features), names_to = "feature", values_to = "value") %>%
  filter(value != "unknown") %>%  # Drop unknown values only for the active feature column
  ggplot(aes(x = value, fill = class)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ feature, scales = "free", ncol = 4) +
  labs(x = NULL, y = "Count") +
  theme_bw() +
  theme(text = element_text(size = 15))

# ==============================================================================
# Display categorical feature importance ----
# ==============================================================================

# ------------------------------------------------------------------------------
# Chi-squared feature importance for categorical features
# ------------------------------------------------------------------------------

# Ensure all categorical features are factors
mushrooms[cat_features] <- lapply(mushrooms[cat_features], as.factor)

# Fit chi-squared feature selector
formula <- as.formula(paste("class ~", paste(cat_features, collapse = " + ")))
chi_scores <- chi.squared(formula, data = mushrooms)

# ------------------------------------------------------------------------------
# Plot chi-squared feature importance
# ------------------------------------------------------------------------------

# Create new column 'feature'
chi_scores_clean <- chi_scores %>%
  tibble::rownames_to_column(var = "feature")  

# Plot as horizontal bar chart
bar_chart <- chi_scores_clean %>%
  arrange(attr_importance) %>%
  ggplot(aes(x = reorder(feature, attr_importance), y = attr_importance)) +
  geom_bar(stat = "identity", fill = "#4E79A7", width = 0.7) +
  geom_text(aes(label = round(attr_importance, 3)),
            hjust = -0.1, size = 3.5, color = "black") +
  coord_flip() +
  labs(x = NULL, y = "Chi-squared Score"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(text = element_text(size = 15),
    axis.title.x = element_text(size = 12),
    panel.grid.major.y = element_blank()
  )

# ==============================================================================
# Save all plots ----
# ==============================================================================
ggsave(
  filename = here("04-outputs/02-mushroom-edibility-analysis-replication", 
                  "Figure1-violin-plots.png"), 
  plot = violin_plot, 
  width = 10, 
  height = 5
)

ggsave(
  filename = here("04-outputs/02-mushroom-edibility-analysis-replication", 
                  "Figure2-count-plots.png"), 
  plot = count_plot, 
  width = 10, 
  height = 5
)

ggsave(
  filename = here("04-outputs/02-mushroom-edibility-analysis-replication", 
                  "Figure2.1-count-plots-full.png"), 
  plot = count_plot_full, 
  width = 10, 
  height = 10
)

ggsave(
  filename = here("04-outputs/02-mushroom-edibility-analysis-replication", 
                  "Figure3-bar-chart.png"), 
  plot = bar_chart, 
  width = 10, 
  height = 8
)