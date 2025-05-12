# 02079140-math70076-assessment-2 Project

This repository contains three complete and self-contained replication studies. Each study reproduces and extends published analyses using real-world open-source datasets and R. The focus is on applying statistical modeling, machine learning, and reproducible research workflows.

---

## Replication Studies Overview

### 1. **COVID-19 Excess Mortality (Germany)**  
**Source Paper**: [Karlinsky & Kobak, 2021 – *eLife*](https://doi.org/10.7554/eLife.69336)  
**Goal**: Estimate and visualize excess mortality in Germany during the COVID-19 pandemic using weekly mortality data.  
**Methods**:  
- Linear regression to predict baseline deaths  
- Excess and P-score calculation  
- Comparison with reported COVID-19 deaths  
**Language**: R  
**Folder**: `01-covid-excess-mortality-replication`

### 2. **Mushroom Edibility Classification**  
**Source Paper**: [IJIGSP, 2019](https://www.mecs-press.org/ijigsp/ijigsp-v11-n4/v11n4-5.html)  
**Goal**: Predict whether a mushroom is edible or poisonous based on physical traits.  
**Methods**: 
- Feature selection using Chi-Squared test   
- Random Forest classifier  
- Model tuning and accuracy evaluation  
**Language**: R 
**Folder**: `02-mushroom-edibility-analysis-replication`

### 3. **Spotify Valence and Emotion Trends**  
**Source Paper**: [Dutta & Mookherjee, 2023 – *arXiv*](https://arxiv.org/abs/2310.19052)  
**Goal**: Analyze and model musical emotion (valence) using Spotify audio features.  
**Methods**:  
- Exploratory data analysis (EDA) of valence trends  
- Linear regression and Random Forest  
- Correlation heatmap and feature importance  
**Language**: R  
**Folder**: `03-spotify-valence-replication`

---

## Requirements

- R (version >= 4.4.5), with packages: `magrittr`, `here`, `tidyverse`, `randomForest`, `caret`, `corrplot`, `GGally`, `FSelector`, `reshape2`
- LaTeX (to compile reports)

---

## Project Structure

# 02_data
This directory contains all raw and derived data sets for each replication study

# 03_analysis
This directory contains R scripts for data processing and visualization for each replication study

# 04_outputs
This directory contains all generated figures for each replication study

# 05_reports
This directory contains the final reports in LaTeX format for each replication study


---

## License

MIT License — for academic use only. See `LICENSE`.

