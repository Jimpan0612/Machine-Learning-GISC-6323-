rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

##
## Find normally distributed clusters in a set of random points
## See Boehmke Chapter 22: Model-based Clustering
##
#install.packages("mclust")
#install.packages("mnormt")
library(mclust); library(mnormt)
mclust.options(hcUse="VARS")


## Feature normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

## Find the minimum cluster separation value `offset` in seq(2,3,by=0.1), 
## which identifies 3 clusters
offset <- 3.0 
  
##
## Generate 3 random clusters with centroids `offset` apart
##
set.seed(321)

npts1 <- 100
clust1 <- rmnorm(npts1, mean=c(-offset/2,0), varcov=diag(c(1,1)))

npts2 <- 100
clust2 <- rmnorm(npts2, mean=c(offset/2,0), varcov=diag(c(1,1)))

npts3 <- 100
clust3 <- rmnorm(npts3, mean=c(0,sqrt(offset^2-(offset/2)^2)), varcov=diag(c(1,1)))

## Build data-frame
member <- factor(c(rep(1,npts1),rep(2,npts2), rep(3,npts3)), labels = c("C1","C2","C3"))

pts <- data.frame(member, x=c(clust1[,1],clust2[, 1], clust3[, 1]), 
                          y=c(clust1[,2],clust2[, 2], clust3[, 2]))

range(pts$x)
range(pts$y)

## Optional normalization
#pts$x <- normalize(pts$x)
#pts$y <- normalize(pts$y)


## Visualize Clusters

plot(y~x, data=pts, col=member, pch=20, cex=1, asp=1,
     main=paste("Centroids of clusters with an offset of",offset," units apart"))
points(mean(pts[pts$member=="C1",2]),mean(pts[pts$member=="C1",3]), col=1, pch=3, cex=2)            # add cluster centroids
points(mean(pts[pts$member=="C2",2]),mean(pts[pts$member=="C2",3]),col=2, pch=3, cex=2)
points(mean(pts[pts$member=="C3",2]),mean(pts[pts$member=="C3",3]), col=3, pch=3, cex=2)

##
## Estimate clusters with 1,2, and 3 suggested groups
##
detectClust <- Mclust(pts[,2:3], G=c(1,2,3,4,5), modelNames="EEI")
summary(detectClust)

## Plot of "optimal" model
plot(detectClust, what="BIC", main="Number of Clusters")
# plot(detectClust, asp=1, what="classification", main="Assignment of Points to Clusters")
# plot(detectClust, asp=1, what="uncertainty", main="Assignment Uncertainty")
# plot(detectClust, asp=1, what="density", main="Joint Density")

# detectClust$parameters # Estimated cluster parameters

