rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console
set.seed(321)     # reset the seed to system clock

popFct <- function(x){
  ## Fictitious population relationship
  y <- (max(x)-x)/4*sin((pi*x/2)^2)+0.4*x+0.4
  return(y)
} #end:popFunction

nSim <- 300             # 300 training samples
nLength <- 100          # of length 100 elements in each sample
nFlex <- 60             # max flex degree of spline function
errVar <- 0.05          # variance of irreducible error

## matrix collecting predicted function values
predTrain <- matrix(NA, nrow = nLength, ncol = nSim)

## matrix with results
flexResults <- matrix(NA, nrow = 2, ncol = nFlex)

## Data setup
xPop <- seq(0, 3, length.out = nLength) 
fctPop <- popFct(xPop)
eps <- replicate(nSim, rnorm(nLength,0,sqrt(errVar)))   # matrix of irreducible error
yTrain <- replicate(nSim,fctPop) + eps                  # matrix of training samples
yTest <- fctPop+rnorm(nLength,0,sqrt(errVar))           # vector of test sample

## Plot one sample with iFlex=13
OneSample <- fctPop+rnorm(nLength,0,sqrt(errVar))           # vector of test sample
plot(OneSample~xPop, xlab="x", ylab="f(x)+e",
     main=paste("Test Sample With Irreducible Error Variance=",errVar, "\nand Smoothing Spline Function with df=40"))
lines(xPop, fctPop, col="red", lwd=2)
lines(xPop, predict(smooth.spline(xPop, OneSample, df=13))$y, col="green")

for (iFlex in 1:nFlex){   
  
  for (iSim in 1:nSim){    # 300 function evaluations
    cSpline <- smooth.spline(xPop, yTrain[,iSim], df=iFlex+1)
    predTrain[ ,iSim] <- predict(cSpline, xPop)$y 
  } # end::iSim
 
  ## Extract one sample at iFlex=10
  if (iFlex == 10) predTrain10 <- predTrain
  
  ## Function statistics over samples for each iFlex
  fctVar <- apply(predTrain, 1, FUN = var)             # Variance of predicted functions at x_i
  fctBias <- apply(predTrain, 1, FUN = mean)           # Mean of predicted function at x_i

  flexResults[1, iFlex] <- mean(fctVar)
  flexResults[2, iFlex] <- mean((fctBias - fctPop)^2)
}

## Explore predTrain10
dim(predTrain10)
View(predTrain)
fctVar10 <- apply(predTrain10, 1, FUN = var)
fctVar10    ## variability of the 300 estimates at the 100 prediction locations
mean(fctVar10)

par(mfrow=c(1,3))
mse <- flexResults[1,]+flexResults[2,]+errVar
  plot(flexResults[1,], type = "l", xlab = "Flexibility", ylab = "Variance")
  plot(flexResults[2,], type = "l", xlab = "Flexibility", ylab = "Squared-Bias")
  plot(mse, type = "l", ylim=c(errVar,max(mse)), xlab = "Flexibility", ylab = "Test MSE")
  abline(h=errVar, v=which.min(mse), lty=4)
par(mfrow=c(1,1))

