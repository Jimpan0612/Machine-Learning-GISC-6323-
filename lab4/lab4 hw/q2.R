rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

# Load the required packages
library(caret)
library(dplyr)
library(e1071)


#a

# Read the data
credit <- read.csv("/Users/jimpan/Documents/EPPS 6326/week files/Weeks06and07/credit.csv")

# Split the data into training (70%) and test (30%) sets
set.seed(123)
trainIndex <- createDataPartition(credit$default, p = 0.7, list = FALSE, times = 1)
train <- credit[trainIndex, ]
test <- credit[-trainIndex, ]




#b

# Set up the tuning grid
tuneGrid <- expand.grid(.sigma = c(0.01, 0.1, 1, 5, 10), .C = c(0.1, 1, 5, 10, 100))


# Set up the train control
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

# Fit the SVM model with radial kernel using cross-validation
svmFit <- train(default ~ ., data = train, method = "svmRadial", tuneGrid = tuneGrid, trControl = control, preProcess = c("center", "scale"), metric = "ROC")

# Print the optimal cost parameter
svmFit$bestTune


#c

library(pROC)

# Predict on the test data using the optimal model
svmPred <- predict(svmFit, newdata = test)

# Convert predicted class labels to factor with levels "No" and "Yes"
svmPred <- factor(svmPred, levels = c("No", "Yes"))

# Convert true class labels to factor with levels "No" and "Yes"
test$default <- factor(test$default, levels = c("No", "Yes"))

svmPred <- factor(ifelse(svmPred == "No", "No", "Yes"), levels = c("No", "Yes"))

# Confusion matrix
confusionMatrix(data = svmPred, reference = test$default)

# ROC curve and AUC #???
rocObj <- roc(test$default, as.numeric(svmPred))
plot(rocObj)






