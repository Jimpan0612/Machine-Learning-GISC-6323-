rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(MASS)
data(Boston)
#(a), (b)
# estimate of population mean of medv
mu_hat <- mean(Boston$medv)

# estimate of standard error of mu_hat
se_mu_hat <- sd(Boston$medv)/sqrt(length(Boston$medv))

# interpreting the result
# The standard error of the mean measures the variability of the sample mean
# and represents the standard deviation of the sampling distribution of the mean.
# A smaller standard error indicates that the sample mean is a more precise estimate of the population mean.

#(c)
set.seed(123)
B <- 1000 # number of bootstrap samples
boot_means <- numeric(B)
n <- length(Boston$medv)
for(i in 1:B) {
  boot_sample <- sample(Boston$medv, n, replace = TRUE)
  boot_means[i] <- mean(boot_sample)
}
se_mu_hat_boot <- sd(boot_means)
se_mu_hat_boot
# 0.4185474

#(d)
mu_hat_lower <- mu_hat - 2 * se_mu_hat_boot
mu_hat_upper <- mu_hat + 2 * se_mu_hat_boot
c(mu_hat_lower, mu_hat_upper)
# 21.69571 23.36990

#Comparing with t.test results
t.test(Boston$medv)$conf.int
# 21.72953 23.33608, 0.95 conf.level

#(e)
mu_hat_med <- median(Boston$medv)
mu_hat_med
# 21.2

#(f)
set.seed(123)
boot_medians <- numeric(B)
for(i in 1:B) {
  boot_sample <- sample(Boston$medv, n, replace = TRUE)
  boot_medians[i] <- median(boot_sample)
}
se_mu_hat_med_boot <- sd(boot_medians)
se_mu_hat_med_boot
# 0.3852428

#There is no simple formula to calculate the standard error of the median, so we have to use the bootstrap method. The standard error of the median is larger than the standard error of the mean, as expected, because the median is a less efficient estimator than the mean.

#(g)
mu0.1 <- quantile(Boston$medv, 0.1)
mu0.1
# 12.75 , 10% of medv in Boston suburbs

#(h)
set.seed(123)
boot_quantiles <- numeric(B)
for(i in 1:B) {
  boot_sample <- sample(Boston$medv, n, replace = TRUE)
  boot_quantiles[i] <- quantile(boot_sample, 0.1)
}
se_mu0.1_boot <- sd(boot_quantiles)
se_mu0.1_boot
# 0.5054028
#The standard error of mu_hat_0.1 is larger than the standard error of mu_hat, but smaller than the standard error of mu_hat_med. This is because the 10th percentile is a less efficient estimator than the mean, but more efficient than the median.
