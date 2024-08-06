rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

# Creating the data frame a
df <- data.frame(
  TrueObserved = c(rep("negative", 10), rep("positive", 10)),
  PredictedPr = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
)

# Calculating sensitivity and specificity for each cutoff value
cutoff_values <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
sens_spec <- data.frame(CutOff = cutoff_values, Sensitivity = rep(NA, length(cutoff_values)), Specificity = rep(NA, length(cutoff_values)))

for (i in 1:length(cutoff_values)) {
  tp <- sum(df$TrueObserved == "positive" & df$PredictedPr >= cutoff_values[i])
  fp <- sum(df$TrueObserved == "negative" & df$PredictedPr >= cutoff_values[i])
  tn <- sum(df$TrueObserved == "negative" & df$PredictedPr < cutoff_values[i])
  fn <- sum(df$TrueObserved == "positive" & df$PredictedPr < cutoff_values[i])
  
  sens_spec$Sensitivity[i] <- tp / (tp + fn)
  sens_spec$Specificity[i] <- tn / (tn + fp)
}

# Plotting the ROC curve
library(pROC)
roc_curve <- roc(df$TrueObserved, df$PredictedPr)
plot(roc_curve)


# Creating the data frame b
df2 <- data.frame(
  TrueObserved2 = c(rep("negative", 10), rep("positive", 10)),
  PredictedPr2 = c(0.55, 0.05, 0.65, 0.15, 0.75, 0.25, 0.85, 0.35, 0.95, 0.45, 0, 0.6, 0.1, 0.7, 0.2, 0.8, 0.3, 0.9, 0.4, 1)
)

# Calculating sensitivity and specificity for each cutoff value
cutoff_values <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
sens_spec2 <- data.frame(CutOff = cutoff_values, Sensitivity = rep(NA, length(cutoff_values)), Specificity = rep(NA, length(cutoff_values)))

for (i in 1:length(cutoff_values)) {
  tp2 <- sum(df2$TrueObserved2 == "positive" & df2$PredictedPr2 >= cutoff_values[i])
  fp2 <- sum(df2$TrueObserved2 == "negative" & df2$PredictedPr2 >= cutoff_values[i])
  tn2 <- sum(df2$TrueObserved2 == "negative" & df2$PredictedPr2 < cutoff_values[i])
  fn2 <- sum(df2$TrueObserved2 == "positive" & df2$PredictedPr2 < cutoff_values[i])
  
  sens_spec2$Sensitivity[i] <- tp2 / (tp2 + fn2)
  sens_spec2$Specificity[i] <- tn2 / (tn2 + fp2)
}

# Plotting the ROC curve
library(pROC)
roc_curve2 <- roc(df2$TrueObserved2, df2$PredictedPr2)
plot(roc_curve2)


