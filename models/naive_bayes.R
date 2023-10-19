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

# Model
# Load the e1071 package
library(e1071)

# Create a Naive Bayes classifier
control <- trainControl(method = "cv", number = 10)

naive_bayes_model <- naiveBayes(class ~ ., data = down_sampled_train, laplace = 3, trControl = control)

# Print the model summary
print(naive_bayes_model)

# how close did we get?
sum(abs(as.numeric(as.matrix(predict(naive_bayes_model, testing_data))) - as.numeric(testing_data$class))) / dim(testing_data)[[1]]

# # ROC Curve
library(pROC)

# Make predictions with your Naive Bayes model
nb_predictions <- predict(naive_bayes_model, newdata = testing_data, type = "raw")

# Assuming that nb_predictions is a matrix with two columns, classes 0 and 1
# Extract the predicted probabilities for the positive class (class "1")
nb_predictions <- as.numeric(nb_predictions[, "1"])

# Create the ROC object
roc_obj <- roc(response = testing_data$class, predictor = nb_predictions)

# Calculate AUC 
auc_value <- auc(roc_obj)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve for Naive Bayes Classifier", print.auc = TRUE)

# Add a diagonal reference line (random classifier)
abline(a = 0, b = 1, lty = 2, col = "red")
