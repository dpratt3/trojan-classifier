require(RPostgreSQL)
require(DBI)
library(caret)
library(mgcv)
library(doParallel)
library(ROSE)
library(corrplot)
library(lubridate)
library(mice)
library(VIM)
library(pROC)

set.seed(123) # reproducibility

pw <- {"password"}

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, 
                 dbname = "trojan",
                 host = "localhost", 
                 port = 5432,
                 user = "postgres", 
                 password = pw)

down_sampled_train = dbGetQuery(con, "SELECT * FROM down_sampled_train") 
testing_data = dbGetQuery(con, "SELECT * FROM down_sampled_test") 

# Train a Random Forest Classifier
down_sampled_train$class <- as.factor(down_sampled_train$class)
testing_data$class <- as.factor(testing_data$class)
colnames(down_sampled_train) <- gsub(" ", "_", colnames(down_sampled_train))
colnames(testing_data) <- gsub(" ", "_", colnames(testing_data))
random_forest_model <- randomForest::randomForest(class ~ ., data = down_sampled_train, type = "response", maxdepth = 50)

# Evaluate Model Performance
# Make predictions on the testing data
predictions <- predict(random_forest_model, testing_data, type = "response")

# Calculate accuracy (adjust as needed for your specific metric)
accuracy <- sum(predictions == testing_data$class) / nrow(testing_data)

# Display the accuracy
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))

# Make predictions on the testing data
rf_predictions <- as.numeric(predict(random_forest_model, testing_data, type = "response"))

# Check data types and structure
str(rf_predictions)
str(testing_data$class)

# Create an ROC object for Random Forest
roc_rf <- roc(response = testing_data$class, predictor = rf_predictions)

# Calculate AUC for Random Forest
auc_rf <- auc(roc_rf)

# Plot the ROC curve for Random Forest
plot(roc_rf, main = "ROC Curve for Random Forest Classifier", print.auc = TRUE)

# Add a diagonal reference line (random classifier)
abline(a = 0, b = 1, lty = 2, col = "red")