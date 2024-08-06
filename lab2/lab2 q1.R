rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(leaps)

#a
# Generate predictor X and noise vector eps
set.seed(123)
X <- rnorm(100)
eps <- rnorm(100)

#b
# Generate response Y according to the model y = Bo + B1X + B2X^2 + B3X^3 + eps
Y <- 1 + 2*X + 3*X^2 + 4*X^3 + eps

# Create data frame containing X and Y
data <- data.frame(X, Y)

#c
# Perform best subset selection using regsubsets
library(leaps)
regfit.Eight <- regsubsets(Y ~ poly(X, 3, raw=TRUE), data=data, nvmax=3)
summary(regfit.Eight)
coef(regfit.Eight, 2)

?plot.regsubsets
par(mfrow=c(2,2))
plot(regfit.Eight, scale = "r2")
plot(regfit.Eight, scale = "adjr2")
plot(regfit.Eight, scale = "Cp")
plot(regfit.Eight, scale = "bic")

## Explore all variables
regfit.full <- regsubsets(Y ~ poly(X, 3, raw=TRUE), data=data, nvmax=3)
(reg.summary <- summary(regfit.full))

names(reg.summary)

reg.summary$rsq

par(mfrow = c(2, 3))
plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

kadjr2 <- which.max(reg.summary$adjr2)
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
points(kadjr2, reg.summary$adjr2[kadjr2], col = "red", cex = 2, 
       pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
kcp <- which.min(reg.summary$cp)
points(kcp, reg.summary$cp[kcp], col = "red", cex = 2,
       pch = 20)

kbic <- which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(kbic, reg.summary$bic[kbic], col = "red", cex = 2,
       pch = 20)
par(oldpar)

#d
## Stepwise forward/backward linear regression
regfit.full <- regsubsets(Y ~ poly(X, 3, raw=TRUE), data=data, nvmax=3)
regfit.fwd <- regsubsets(Y ~ poly(X, 10, raw=TRUE), data=data, nvmax=10, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Y ~ poly(X, 10, raw=TRUE), data=data, nvmax=10, method = "backward")
summary(regfit.bwd)

coef(regfit.full, 3)
coef(regfit.fwd, 10)
coef(regfit.bwd, 10)

#e
library(glmnet)

# Generate simulated data
set.seed(123)
n <- 100
X <- matrix(rnorm(n*10), n, 10)
eps <- rnorm(n)
Y <- 1 + 2*X[,1] + 3*X[,2] + 4*X[,3] + eps

grid <- 10^seq(10, -2, length = 100)  # lambda search grid
?glmnet   ## alpha=0 -> ridge; alpha=1 -> lasso
ridge.mod <- glmnet(X, Y, alpha = 0, lambda = grid)

# Fit lasso model using cross-validation
cv.lasso <- cv.glmnet(X, Y, alpha = 0)
plot(cv.lasso)

# Report coefficient estimates for the selected lambda
coef(cv.lasso, s = "lambda.min")

#f
# Generate response vector Y according to the new model
Y <- 1 + 8*X^7 + eps

# Best subset selection
best.subset2 <- regsubsets(Y ~ poly(X, 10, raw = TRUE), data = data, nvmax = 10,lambda = grid)
summary(best.subset2)

# Lasso
length(X)
length(Y)
train <- sample(1:nrow(X), nrow(X) / 2)

cv.out <- cv.glmnet(X[train, ], Y[train], alpha = 0, nfolds = 10)
plot(cv.out)
coef(cv.out, s = "lambda.min")

