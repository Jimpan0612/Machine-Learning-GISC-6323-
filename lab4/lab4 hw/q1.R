rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

#a
set.seed(12345)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)

#b
plot(x1, x2, col = y + 1)


#c
#fits a logistic regression model 
logit <- glm(as.factor(y) ~ x1 + x2, family = "binomial")

#d
# Make predictions using the logistic regression model
y_pred <- predict(logit, type = "response")

# Convert predicted probabilities to class labels
y_pred_class <- as.numeric(y_pred > 0.5)

plot(x1, x2, col = y_pred_class + 1)

#e
# Create new variables based on x1 and x2
x3 <- x1^2
x4 <- x2^2
x5 <- x1*x2

# Fit a logistic regression model with non-linear terms
logit_nl <- glm(y ~ x1 + x2 + x3 + x4 + x5, family = "binomial")

#f

y_pred_nl <- predict(logit_nl, type = "response")
y_pred_class_nl <- as.numeric(y_pred_nl > 0.5)
plot(x1, x2, col = y_pred_class_nl + 1)

#g
??e1071
library(e1071)
svm_linear <- svm(y ~ x1 + x2, data = data.frame(x1, x2, y), kernel = "linear")

y_pred_svm_linear <- predict(svm_linear)
plot(x1, x2, col = y_pred_svm_linear + 1)

#h

svm_nonlinear <- svm(y ~ x1 + x2, data = data.frame(x1, x2, y), kernel = "radial")

y_pred_svm_nonlinear <- predict(svm_nonlinear)
plot(x1, x2, col = y_pred_svm_nonlinear + 1)

#i
#The logistic regression model with non-linear transformations of the predictors resulted in a non-linear decision boundary, as expected. The support vector classifier with linear kernel also resulted in a linear decision boundary, but the support vector machine with a non-linear kernel was able to capture the non-linear pattern in the data and provide a better classification boundary. 

