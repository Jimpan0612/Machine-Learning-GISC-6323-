rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

## Download package TexMix not in CRAN
# install.packages("http://www.spatialfiltering.com//ThinkR/Downloads/TexMix_0.5.3.tar.gz", 
#                  repos=NULL, dependencies=TRUE)   # dependencies not installed
library(TexMix)
library(sp)
library(maptools)
library(spdep)

## Install library 
install.packages("ClustGeo", dependencies=TRUE)
library(ClustGeo)

## Make sure that the plot window is approximately square
## Note: the map files are large. It takes some time to draw the maps
#dev.off()
## Optional: Setup square plot window in inches independent of RStudio
#windows(width=8, height=8, record=TRUE)     

setwd("/Users/jimpan/Documents/EPPS 6326/lab/lab5/TXCnty2021")   # Directory with the map data

##
## Read Poly Shapefiles (readShapePoly in library maptools)
##
getinfo.shape("TXCnty2021.shp")

## rgdal::readOGR function to read shape files. Note: file extension *.shp no required
## the projection file *.prj is properly interpreted (here: long/lat coordinates)
## option  "integer64" required to import integer variables properly

## Get polygons of neighboring States for geographic frame
## Use only for final maps because of slow drawing
neig.shp <- rgdal::readOGR(dsn=getwd(), layer = "TXNeighbors",
                             integer64 = "allow.loss", stringsAsFactors=T)

## Get interstate layer for spatial orientation
interState.shp <- rgdal::readOGR(dsn=getwd(), layer = "InterStateHwy",
                                 integer64 = "allow.loss", stringsAsFactors=T)

## Get polygons of TX counties
county.shp <- rgdal::readOGR(dsn=getwd(), layer = "TXCnty2021",
                            integer64 = "allow.loss", stringsAsFactors=T)


  
county.bbox <- bbox(county.shp)                             # county bounding box for map region
county.centroid <- coordinates(county.shp)                  # Get county centroids

summary(county.shp)

## Convert religious adherence into the ordinal scale
county.shp$RELNUM <- unclass(county.shp$RELIGADHER)

##
## Map categorical variable religious adherence counties
##
plot(neig.shp, axes=T, col=grey(0.9),                       # first background (only for final maps)
     border="white", bg="lightblue",
     xlim=county.bbox[1,], ylim=county.bbox[2,])             
mapColorQual(county.shp$RELIGADHER, county.shp,
             map.title="Religious Adherence TX Counties",
             legend.cex=0.8,
             legend.title = "Adherence",add.to.map=T)
plot(interState.shp, col="tomato4", lwd=1, add=T)             # insert road network for orientation

##
## Color gradient map: population density with additional layers
##
plot(neig.shp, axes=T, col=grey(0.9),                         # first background (axes=T adds lat/long frame)
     border="white",  bg="lightblue",                         # border and background color 
     xlim=county.bbox[1,], ylim=county.bbox[2,])              # within bounding box

mapColorRamp(county.shp$MEDVALHOME, county.shp, breaks=8,     # second add map
             map.title="Median Home Values", 
             legend.cex=0.8,
             legend.title="Value",add.to.map=T)               # add.to.map=T over-plots provinces over neighbors

plot(interState.shp, col="tomato4", lwd=1, add=T)             # insert road network for orientation

##
## Plot bipolar map: variation around median(population change)
##

hist(county.shp$POPCHG)                                     # explore distribution to

plot(neig.shp, axes=T, col=grey(0.9),                       # first background
     border="white", bg="lightblue",
     xlim=county.bbox[1,], ylim=county.bbox[2,])             

mapBiPolar(county.shp$POPCHG, county.shp,                   # map variation of population change
           neg.breaks=3, pos.breaks=6,  break.value=0,
           map.title="Population Change from 2010 to 2018", 
           legend.title="Population\nChange",   # \n escape character for new line
           legend.cex=0.8, add.to.map=T)

plot(interState.shp, col="tomato4", lwd=1.5, add=T)         # insert road network for orientation

##
## Get spatial structure distance matrices
##
nb <- spdep::poly2nb(county.shp, queen=F)                    # extract first order neighbors links (similarity)
B <- spdep::nb2mat(nb, style="B")                            # convert neighbor list to binary matrix
topoDist <- 1-B; diag(topoDist) <- 0                         # convert into dissimilarity
topoDist <- as.dist(topoDist)                                # convert into distance object (dissimilarity)

##
## Plot neighbor's relationship
##
plot(county.shp, col="palegreen3", border=grey(0.9), axes=T) # map topology
plot(nb, coords=coordinates(county.shp), pch=19, cex=0.1, col="blue", add=T)
title("Spatial Neighbors Links among TX County") 
topoDist <- 1-B; diag(topoDist) <- 0                         # convert into dissimilarity
topoDist <- as.dist(topoDist)                                # convert into distance object

##
## Get spherical distance matrix
##
sphDist <- sp::spDists(county.shp, longlat=T)                # spherical distance matrix among tracts in km
sphDist <- as.dist(sphDist)

##
## Get graph step distance from topology
##
BNa <- ifelse(B==0,NA,B)                                     # recode 0's to NA's
allPath <- e1071::allShortestPaths(BNa)                      # calculate the shortest path among all nodes
graphDist <- as.dist(allPath$length)                         # number of steps from origin to destination node
