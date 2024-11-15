# Required Libraries
library(ggplot2)
library(reshape2)
library(randomForest)
library(rpart)
library(caret)
library(neuralnet)   
library(e1071)  # For Support Vector Machine (SVM)

# Load the training and test datasets
train_path <- file.choose()  # Select the training dataset file
test_path <- file.choose()   # Select the test dataset file

train_data <- read.csv(train_path, header = TRUE, sep = ',')
test_data <- read.csv(test_path, header = TRUE, sep = ',')

# Ensure data types are correct in both training and test data
train_data$grade <- as.factor(train_data$grade)
train_data$has_basement <- as.factor(train_data$has_basement)
train_data$renovated <- as.factor(train_data$renovated)
train_data$nice_view <- as.factor(train_data$nice_view)
train_data$perfect_condition <- as.factor(train_data$perfect_condition)
train_data$has_lavatory <- as.factor(train_data$has_lavatory)
train_data$single_floor <- as.factor(train_data$single_floor)
train_data$quartile_zone <- as.factor(train_data$quartile_zone)
train_data$month <- as.factor(train_data$month)
train_data$year <- as.factor(train_data$year)

test_data$grade <- as.factor(test_data$grade)
test_data$has_basement <- as.factor(test_data$has_basement)
test_data$renovated <- as.factor(test_data$renovated)
test_data$nice_view <- as.factor(test_data$nice_view)
test_data$perfect_condition <- as.factor(test_data$perfect_condition)
test_data$has_lavatory <- as.factor(test_data$has_lavatory)
test_data$single_floor <- as.factor(test_data$single_floor)
test_data$quartile_zone <- as.factor(test_data$quartile_zone)
test_data$month <- as.factor(test_data$month)
test_data$year <- as.factor(test_data$year)

# Helper function to calculate RMSE
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Apply Linear Regression model
LR_model <- lm(price ~ ., data = train_data)
LR_predictions <- predict(LR_model, test_data)
LR_rmse <- calculate_rmse(test_data$price, LR_predictions)

# Apply Decision Tree model
DT_model <- rpart(price ~ ., data = train_data, method = 'anova')
DT_predictions <- predict(DT_model, test_data)
DT_rmse <- calculate_rmse(test_data$price, DT_predictions)

# Apply Random Forest model
RF_model <- randomForest(price ~ ., data = train_data, ntree = 100, mtry = 3)
RF_predictions <- predict(RF_model, test_data)
RF_rmse <- calculate_rmse(test_data$price, RF_predictions)

# Apply Support Vector Machine (SVM) model
SVM_model <- svm(price ~ ., data = train_data)
SVM_predictions <- predict(SVM_model, test_data)
SVM_rmse <- calculate_rmse(test_data$price, SVM_predictions)

# Display R-squared values for each model
LR_R2 <- summary(LR_model)$r.squared * 100
DT_R2 <- 1 - sum((test_data$price - DT_predictions)^2) / sum((test_data$price - mean(test_data$price))^2) * 100
RF_R2 <- 1 - sum((test_data$price - RF_predictions)^2) / sum((test_data$price - mean(test_data$price))^2) * 100
SVM_R2 <- 1 - sum((test_data$price - SVM_predictions)^2) / sum((test_data$price - mean(test_data$price))^2) * 100

# Store RMSE and R-squared results
model_results <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest", "SVM"),
  RMSE = c(LR_rmse, DT_rmse, RF_rmse, SVM_rmse),
  R_Squared = c(LR_R2, DT_R2, RF_R2, SVM_R2)
)
print(model_results)

# Plot RMSE Comparison
ggplot(model_results, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "RMSE Comparison Across Models", x = "Model", y = "RMSE Value") +
  theme(legend.position = 'none')

# Plot R-Squared Comparison
ggplot(model_results, aes(x = Model, y = R_Squared, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "R-Squared Comparison Across Models", x = "Model", y = "R-Squared Value") +
  theme(legend.position = 'none')
