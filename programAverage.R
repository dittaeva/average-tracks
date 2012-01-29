# version: 12.01.21
# creator: Michiel Faber
# longtitude: east-west; x
# lattitude: north-south; y
rm(list=ls())

# Directory where this file and "functionsGlobal.R" and "functionsAverage.R" are located.
setwd("~/bin/average-tracks/") 

source("functionsGlobal.R")
source("functionsAverage.R")

loadedTracks <- fLoadFiles(
	inputDir="~/input/", # Directory with input files. 
	listOfFiles=NULL,
	fileType="gpx", # Use "gpx" or "csv"
	nTracks=0,
	loadHeader=FALSE, # csv files without a header
	method="rgdal") # Can use "rgdal" or "maptools". Both will autoinstall if you have the necessary libraries, and installation permissions.

fileNames <- loadedTracks[[2]]
tracks <- loadedTracks[[1]]

averageTrack <- fAverageTrack(
	tracks[tracks$routeID==1,], # track 1
	tracks[tracks$routeID==2,], # track 2
	elevation = TRUE,
	method="b") # combine both averaging methods. "p" for only point method, "l" for line method.

# uncomment png() to save plot as png. !! See also dev.off() on line 42. !!
#png(filename = "~/averageTrack.png", width=2048, height=2048, res=200)
 plot(tracks[tracks$routeID==1,2:1], type="l", pch=2, col="green")
 lines(tracks[tracks$routeID==2,2:1], type="l", pch=2, col="green")
 lines(averageTrack[,2:1], type="l", pch=1, col="red")

 legend.text <- c(fileNames[1], fileNames[2], "Average")
 legend.col <- c("green", "blue", "black")
 legend.lty <- "solid"
 legend.pch <- c(2,1,4) 
 legend.position <- "topleft"
 legend(x=legend.position, legend=legend.text, col=legend.col, lty=legend.lty, pch=legend.pch)
#dev.off() # uncomment when saving plot

fSaveTrack(
	track=averageTrack,
	outputDir="~/output", # Directory where to save the output file.
	trk="20120121", # part of the trackname. "track" will be placed in front of "trk".
	roundTo=6)

