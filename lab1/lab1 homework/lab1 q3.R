rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

#a) Here we open the Default dataset in the package ISLR2, we prepare the data for a k nearest neighbors prediction and generate default-stratified 30% test and 70% training samples.
library(ISLR2)
library(dplyr)
library(caret)
library(class)
library(rsample)
library(pROC)
data(Default)
set.seed(321)
trainIndex <- createDataPartition(Default$default, p = 0.7, list = FALSE, times = 1)
train <- Default[trainIndex, ]
test <- Default[-trainIndex, ]

# Check for missing or invalid values in the data
sum(is.na(train))
sum(is.na(test))

# Convert non-numeric variables to numeric
train$student <- as.numeric(train$student == "Yes")
test$student <- as.numeric(test$student == "Yes")

# Make sure default variable is a factor
train$default <- factor(train$default)
test$default <- factor(test$default)

#k value
accuracy <- vector(mode = "numeric", length = 100)
for (k in 1:100) {
  pred_k <- knn(train = train[, -1], test = test[, -1], cl = train$default, k = k)
  accuracy[k] <- mean(pred_k == test$default)
}
optimal_k <- which.max(accuracy)
cat(paste0("The optimal k value is: ", optimal_k, "\n"))

# Fit k-nearest neighbors model using optimal k value and training data
knn_fit <- train(default ~ ., method = "knn", trControl = trainControl(method = "cv"), data = train)
knn_model <- knn(train[, -1], test[, -1], train[, 1], k = knn_fit$bestTune$k)

# Generate confusion matrix for k-nearest neighbors model
knn_pred <- factor(knn_model, levels = c("No", "Yes"))
confusionMatrix(knn_pred, test$default, dnn = c("Predicted", "Actual"))

# Generate ROC curve for k-nearest neighbors model
roc_knn <- roc(test$default, as.numeric(knn_model))
plot(roc_knn)

#d) For the same training and test samples, we estimate a logistic regression model.

logit_reg <- glm(default ~ ., data = train, family = binomial)
summary(logit_reg)

#e) For the logistic regression model, we can report the confusion matrix and the ROC-curve.

pred_reg <- ifelse(predict(logit_reg, test, type = "response") > 0.5, "Yes", "No")
table(test$default, pred_reg)
roc_reg <- roc(test$default, predict(logit_reg, test))
plot(roc_reg, main = "ROC curve", col = "blue", lwd = 2, legacy.axes = TRUE)

