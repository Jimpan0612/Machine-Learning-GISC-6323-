rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console


library(TexMix)    ## For mapping functions
library(sp);library(maptools)
library(spdep);library(ClustGeo)

setwd("E:\\Lectures2023\\GISC6323EPPS6326\\Labs\\Lab05")

## Get neighboring state as spatial reference frame
neig.shp <- rgdal::readOGR(dsn="TXCnty2021", layer = "TXNeighbors",
                             integer64 = "allow.loss", stringsAsFactors=T)

## Get interstate layer for spatial orientation
interState.shp <- rgdal::readOGR(dsn="TXCnty2021", layer = "InterStateHwy",
                                 integer64 = "allow.loss", stringsAsFactors=T)

## Get polygons of TX counties
county.shp <- rgdal::readOGR(dsn="TXCnty2021", layer = "TXCnty2021",
                            integer64 = "allow.loss", stringsAsFactors=T)

## Exclude Loving County
county.shp <- county.shp[county.shp$NAME_x!="Loving", ]

Vars <- county.shp@data
Vars <- transform(Vars,
  RELIGADHER = as.numeric(RELIGADHER),
  TRUMPVOT16 = TRUMPVOT16/TOTALVOT16,
  BNEW = B2000PCT+B2010PCT
)

varKeep <- c("RELIGADHER","DISTMEX", "TRUMPVOT16", "TURNOUT16",
             "UNINSURED", "COLLEGEDEG", "POVERTY", "INCOME",
             "MEDAGE", "PARTBLACK", "HISPORG", "BIR15TO50",
             "POPDENSE", "POPCHG", "BNEW", "MEDVALHOME")

xVars <- Vars[, varKeep]
row.names(xVars) <- Vars$NAME_x
row.names(xVars) <- 1:nrow(xVars)


county.bbox <- bbox(county.shp)                # county bounding box for map region
county.centroid <- coordinates(county.shp)     # Get county centroids

##
## Get spatial structure distance matrices
##
nb <- spdep::poly2nb(county.shp, queen=F)      # extract first order neighbors links
B <- spdep::nb2mat(nb, style="B")              # convert neighbor list to binary matrix
plot(county.shp, col="palegreen3", border=grey(0.9), axes=T) # map topology
plot(nb, coords=coordinates(county.shp), pch=19, cex=0.1, col="blue", add=T)
title("Spatial Neighbors Links among TX County") 
topoDist <- 1-B; diag(topoDist) <- 0          # convert into dissimilarity
topoDist <- as.dist(topoDist)                 # convert into distance object

##
## Get steps distance from topology
##
BNa <- ifelse(B==0,NA,B)                      # recode 0's to NA's
allPath <- e1071::allShortestPaths(BNa)       # calculate the shortest path among all nodes
pathDist <- as.dist(allPath$length)           # number of steps from origin to destination node

sphDist <- sp::spDists(county.shp, longlat=T)   # spherical distance matrix among tracts in km
sphDist <- as.dist(sphDist)

K <- 7                             # Number of distinct clusters
range.alpha <- seq(0, 1, by=0.1)   # Evaluation range
cr <- choicealpha(featDist, sphDist, range.alpha, K, graph=TRUE)
tree <- hclustgeo(featDist, sphDist, alpha=0.1)
plot(tree, hang=-1)
rect.hclust(tree, k=K)

neighClus <- as.factor(cutree(tree, K))        # Determine cluster membership
## Label clusters
neighClus <- factor(neighClus, labels=c("Historic Black", "Religious", "Affluent Rural", 
                                        "Conservative", "Hispanic", "Metropolitan", "Suburban") )
table(neighClus)
plot(table(neighClus))                         # number of c in each cluster

plot(neig.shp, axes=T, col=grey(0.9),                  # first background (only for final maps)
     border="white", bg="lightblue",
     xlim=county.bbox[1,], ylim=county.bbox[2,])        

mapColorQual(neighClus, county.shp, 
             map.title ="Spatially constrained Cluster Analysis",
             legend.title="Cluster\nId.", legend.cex=0.9, add=T)

plot(interState.shp, col="tomato4", lwd=1, add=T)      # insert road network for orientation


plotBoxesByFactor(xVars[,1:4], neighClus, ncol=1, zTrans=T, varwidth=F)
plotBoxesByFactor(xVars[,5:8], neighClus, ncol=1, zTrans=T, varwidth=F)
plotBoxesByFactor(xVars[,9:12], neighClus, ncol=1, zTrans=T, varwidth=F)
plotBoxesByFactor(xVars[,13:16], neighClus, ncol=1, zTrans=T, varwidth=F)

options(digits=2)
by(scale(xVars), neighClus, colMeans)

## Generate heat map
hclust_rows <- as.dendrogram(tree)  # Calculate hclust dendrograms
hclust_cols <- as.dendrogram(hclust(dist(t(scale(xVars)))))

heatmap(as.matrix(scale(xVars)),                                     # Draw heatmap with hclust dendrograms
        Rowv = hclust_rows,
        Colv = hclust_cols)
