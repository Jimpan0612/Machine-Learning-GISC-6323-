rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console
 
#a
library(caret)
data(College)
set.seed(123)
train_index <- sample(nrow(College), nrow(College)*0.7)
train <- College[train_index, ]
test <- College[-train_index, ]


# b
lm.fit <- lm(Apps ~ ., data = train)
summary(lm.fit)

lm.pred <- predict(lm.fit, newdata = test)
lm.mse <- mean((test$Apps - lm.pred)^2)
lm.mse
#The test error obtained using the linear model with least squares on the test set is1260299

#c
library(glmnet)
x <- model.matrix(Apps ~ ., data = train)
y <- train$Apps
set.seed(123)
cv.ridge <- cv.glmnet(x, y, alpha = 0)
ridge.bestlam <- cv.ridge$lambda.min
ridge.fit <- glmnet(x, y, alpha = 0, lambda = ridge.bestlam)
summary(ridge.fit)

ridge.pred <- predict(ridge.fit, newx = model.matrix(Apps ~ ., data = test))
ridge.mse <- mean((test$Apps - ridge.pred)^2)
ridge.mse
#The test error obtained using the linear model with least squares on the test set is 1155057

#d
t.test(Boston$medv)
set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1)
lasso.bestlam <- cv.lasso$lambda.min
lasso.fit <- glmnet(x, y, alpha = 1, lambda = lasso.bestlam)
summary(lasso.fit)

lasso.pred <- predict(lasso.fit, newx = model.matrix(Apps ~ ., data = test))
lasso.mse <- mean((test$Apps - lasso.pred)^2)
lasso.mse
coef(lasso.fit)
#The test error obtained using the linear model with least squares on the test set is 1247233
#There are 15 non-zero coefficient estimates in the output of the coef() function for the Lasso regression model.


#e
library(pls)
set.seed(123)
pcr.fit <- pcr(Apps ~ ., data = College, scale = TRUE, validation = "CV")  # 10-folds
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

pcr.fit <- pcr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, test, ncomp = 5)
mean((test$Apps - pcr_pred)^2)
#3971132

#f
set.seed(123)
pls.fit <- plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls.pred <- predict(pls.fit, test, ncomp = 1)
mean((test$Apps - pcr_pred)^2)
#3971132

#g
#
