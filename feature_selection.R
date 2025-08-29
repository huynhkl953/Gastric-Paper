# Load necessary libraries
library(readxl)
library(glmnet)
library(dplyr)

# Set working directory and load data
setwd("C:/Users/User/Downloads")
metadata <- read_xlsx("Metadata (1).xlsx")
data_cfdna_meta <- read.csv("gastric0720.csv")

# Split data into train and test
train <- data_cfdna_meta[which(data_cfdna_meta$Cohort == "Train"),]
test <- data_cfdna_meta[which(data_cfdna_meta$Cohort == "Test"),]

# Assuming the target column is named 'Label'
# Replace 'Label' with the actual column name for your target variable
target_col <- "Label"

# 1. Lasso Regression for Feature Selection
X_train <- as.matrix(train[, -which(names(train) == target_col)])  # Features
y_train <- train[[target_col]]  # Labels (binary: 0 and 1)

# Fit Lasso model
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1, family = "binomial")

# Extract non-zero coefficients
lasso_coef <- coef(lasso_model, s = "lambda.min")
selected_features_lasso <- rownames(lasso_coef)[which(lasso_coef != 0)][-1]  # Exclude intercept

# 2. Statistical Tests for Feature Selection
# Function to perform t-test for continuous features
select_features_ttest <- function(data, label_col, alpha = 0.05) {
  features <- names(data)[!names(data) %in% label_col]
  selected_features <- c()
  
  for (feature in features) {
    if (is.numeric(data[[feature]])) {
      t_test_result <- t.test(data[[feature]] ~ data[[label_col]])
      if (t_test_result$p.value < alpha) {
        selected_features <- c(selected_features, feature)
      }
    }
  }
  
  return(selected_features)
}

# Function to perform chi-square test for categorical features
select_features_chisq <- function(data, label_col, alpha = 0.05) {
  features <- names(data)[!names(data) %in% label_col]
  selected_features <- c()
  
  for (feature in features) {
    if (is.factor(data[[feature]]) || is.character(data[[feature]])) {
      chisq_test_result <- chisq.test(table(data[[feature]], data[[label_col]]))
      if (chisq_test_result$p.value < alpha) {
        selected_features <- c(selected_features, feature)
      }
    }
  }
  
  return(selected_features)
}

# Select features using t-test and chi-square test
selected_continuous <- select_features_ttest(train, target_col)
selected_categorical <- select_features_chisq(train, target_col)

# Combine selected features from statistical tests
selected_features_stats <- unique(c(selected_continuous, selected_categorical))

# 3. Combine Lasso and Statistical Test Results
final_selected_features <- unique(c(selected_features_lasso, selected_features_stats))

# Print final selected features
print("Final Selected Features:")
print(final_selected_features)

# 4. Apply Selected Features to Test Data
test_selected <- test[, final_selected_features]

# Print the test data with selected features
print("Test Data with Selected Features:")
print(test_selected)
