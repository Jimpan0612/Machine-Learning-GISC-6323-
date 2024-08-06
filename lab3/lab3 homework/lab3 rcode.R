rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(tree)
library(ISLR2)
library(caret)
library(randomForest)
library(gbm)
library(caret)


# Load the dataset
mushrooms <- read.csv("/Users/jimpan/Documents/EPPS 6326/lab/lab3/mushrooms.csv")

# Remove the variable veil_type
mushrooms$veil_type <- NULL

# Convert non-numeric columns to factors
non_numeric_cols <- sapply(mushrooms, is.character)
mushrooms[, non_numeric_cols] <- lapply(mushrooms[, non_numeric_cols], factor)

table(is.na(mushrooms))
table(mushrooms$type)
# Remove rows with missing values
mushrooms <- na.omit(mushrooms)

# Split the dataset into training and test sets
set.seed(1)
train <- mushrooms[1:round(2/3*nrow(mushrooms)),]
test <- mushrooms[-(1:round(2/3*nrow(mushrooms))),]

summary(train)

train <- na.omit(train)


#Q1
# Build an unpruned tree using the training data
tree_mushrooms <- tree(type ~ ., data = train)
summary(tree_mushrooms)


plot(tree_mushrooms)
text(tree_mushrooms, pretty = 0)
## Deviance: -2 * sum_Classes(sum_Nodes(obs_CN * log(phat_CN)))
tree_mushrooms

## Pruning
set.seed(1)

help("cv.tree")
cv.mushrooms <- cv.tree(tree_mushrooms, FUN = prune.misclass, K=10)
str(cv.mushrooms)
cv.mushrooms

par(mfrow = c(1, 2))
plot(cv.mushrooms$size, cv.mushrooms$dev, type = "b")
plot(cv.mushrooms$k, cv.mushrooms$dev, type = "b")
par(oldpar)

## Evaluate pruned tree
prune.mushrooms <- prune.misclass(tree_mushrooms, best = 4)
plot(prune.mushrooms)
text(prune.mushrooms, pretty = 0)


# Q2
# Build a random forest model using the training data
rf_mushrooms <- randomForest(type ~ ., data = train, ntree = 500)

## Importance plot
varImpPlot(rf_mushrooms)

#Q3
table(is.na(train))
train <- na.omit(train)

# Find the optimal mtry
tune_mushrooms <- tuneRF(train[, -1], train[, 1], ntree = 500)
tune_mushrooms$mtry

#mtry = 4  OOB error = 0% 
#Searching left ...
#mtry = 2 	OOB error = 0% 
#NaN 0.05 
#Error in if (Improve > improve) { : missing value where TRUE/FALSE needed



#Q4

set.seed(1)

## Find the optimal interaction depth
depths <- 1:10
errors <- rep(0, length(depths))

for (i in 1:length(depths)) {
  boost_mushrooms <- gbm(type ~ ., data = train, distribution = "multinomial",
                         n.trees = 5000, interaction.depth = depths[i])
  errors[i] <- boost_mushrooms$cv.error[boost_mushrooms$n.trees == 5000]
}
## Plot the cross-validation error rates
plot(depths, errors, type = "b", xlab = "Interaction Depth", ylab = "CV Error Rate")


#Q5



