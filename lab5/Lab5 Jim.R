rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(TexMix)
library(sp)
library(ggplot2)
library(maptools)
library(spdep)
library(foreign)
library(e1071)
library(VIM)
library(ClustGeo)

setwd("/Users/jimpan/Documents/EPPS 6326/lab/lab5/TXCnty2021") 
# Read in the shapefile and its associated DBASE file

#Q1 Feature Selection and Preparations

TXCnty2021 <- read.dbf("TXCnty2021.DBF")
TXCnty2021.SHP <- readShapePoly("TXCnty2021.SHP", proj4string=CRS("+proj=longlat"))


# Select metric features for potential differences between putative regions
features <- c("LANDAREA", "WATERAREA", "DISTMEX", "LONG", "LAT")

# Subset the TXCnty2021 data frame to include only the selected features
TXCnty2021 <- TXCnty2021[,features]

# Justification for feature selection:
# LANDAREA and WATERAREA may be good indicators of the size of a county, which could potentially impact economic, social, and demographic factors.
# DISTMEX measures the distance from the county to the Mexican border and could potentially be a useful variable for identifying regions with different cultural or economic characteristics.
# LONG and LAT are the longitude and latitude coordinates of the county centroid and could potentially be useful for identifying geographic clusters of counties.

# Check for redundant features
cor(TXCnty2021)

# Based on the correlation matrix, there are no highly correlated features that need to be removed.

# Standardize the features if necessary
TXCnty2021 <- scale(TXCnty2021)
TXCnty2021


#Q2 Selection of Spatial Relationships
library(rgdal)
library(spdep)

# Read shapefile
county.shp <- readOGR(dsn = ".", layer = "TXCnty2021")

# Get spatial structure distance matrices
nb <- poly2nb(county.shp, queen=F)
B <- nb2mat(nb, style="B")

plot(county.shp, col="palegreen3", border=grey(0.9), axes=T) 
plot(nb, coords=coordinates(county.shp), pch=19, cex=0.1, col="blue", add=T)
title("Spatial Neighbors Links among Tracts") 


topoDist <- 1-B
diag(topoDist) <- 0
topoDist <- as.dist(topoDist)

# Generate distance matrices
topoDist <- dnearneigh(coordinates(county.shp), d1=0, d2=50000, row.names=county.shp$NAME)
sphDist <- dnearneigh(coordinates(county.shp), d1=0, d2=Inf, row.names=county.shp$NAME, longlat=TRUE)
graphDist <- nb2listw(nb, style="W")

topoDist
sphDist
graphDist
# chioce topoDist for the task

#Q3 Iterative Cluster Identification

# Perform Iterative Cluster Identification
xVars <- TXCnty2021
xVars <- kNN(xVars, k = 5)
summary(xVars$LANDAREA)

row.names(xVars) <- 1:nrow(xVars)
featDist <- dist(scale(xVars))

# Convert featDist to class dist
featDist <- as.dist(featDist)

str(topoDist)
# Convert topoDist to a matrix
topoMat <- as.matrix(topoDist)

# Convert topoMat to a matrix
topoMat <- do.call(rbind, topoMat)

dim(topoMat)

# Convert topoMat to a matrix
topoMat <- as.matrix(topoDist)
# Transpose topoMat
topoMat <- t(topoMat)

# Convert topoMat to a distance object
topoDist <- as.dist(topoMat)


topoDist <- as.dist(topoDist)

# Evaluate mixture of feature and spatial dissimilarity.
K <- 12
range.alpha <- seq(0, 1, by = 0.1)

cr <- choicealpha(featDist, topoDist, range.alpha, K, graph = TRUE)

# Perform spatially constrained cluster analysis
tree <- hclustgeo(featDist, topoDist, alpha = 0.2)
plot(tree, hang = -1)
rect.hclust(tree, k = K)

# Number of census tracts per market area
neighClus <- as.factor(cutree(tree, K))
table(neighClus)

# Map Results
mapColorQual(neighClus, county.shp, 
             map.title = "Spatially Constrained Cluster Analysis",
             legend.title = "Cluster\nId.", legend.cex = 0.9)

plot(lakesShp, col = "skyblue", border = "skyblue", add = TRUE)
plot(hwyShp, col = "cornsilk2", lwd = 4, add = TRUE)
plotBoxesByFactor(xVars, neighClus, ncol = 2, zTrans = TRUE, varwidth = FALSE)


# k=6
# Evaluate mixture of feature and spatial dissimilarity.
K <- 6
range.alpha <- seq(0, 1, by = 0.1)

cr <- choicealpha(featDist, topoDist, range.alpha, K, graph = TRUE)

# Perform spatially constrained cluster analysis
tree <- hclustgeo(featDist, topoDist, alpha = 0.2)
plot(tree, hang = -1)
rect.hclust(tree, k = K)

# Number of census tracts per market area
neighClus <- as.factor(cutree(tree, K))
table(neighClus)

# Map Results
mapColorQual(neighClus, county.shp, 
             map.title = "Spatially Constrained Cluster Analysis",
             legend.title = "Cluster\nId.", legend.cex = 0.9)

plot(lakesShp, col = "skyblue", border = "skyblue", add = TRUE)
plot(hwyShp, col = "cornsilk2", lwd = 4, add = TRUE)
plotBoxesByFactor(xVars, neighClus, ncol = 2, zTrans = TRUE, varwidth = FALSE)



