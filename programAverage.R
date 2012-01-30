# version: 12.01.21
# creator: Michiel Faber
# longtitude: east-west; x
# lattitude: north-south; y
rm(list=ls())

# Working directory. Directory where programAverage.R and functions are located. Also where relative paths relate to. Uncomment and customise if needed, or current directory will be used.
#setwd("~/bin/average-tracks/") 

source("functionsGlobal.R")
source("functionsAverage.R")

loadedTracks <- fLoadFiles(
	inputDir="input", # Directory with input files.
	listOfFiles=NULL,
	fileType="gpx", # "gpx" or "csv".
	nTracks=0,
	loadHeader=FALSE, # csv files without a header.
	method="rgdal") # "rgdal" or "maptools". Both will autoinstall if necessary libraries are present.

fileNames <- loadedTracks[[2]]
tracks <- loadedTracks[[1]]

averageTrack <- fAverageTrack(
	tracks[tracks$routeID==1,], # track 1
	tracks[tracks$routeID==2,], # track 2
	elevation = TRUE,
	method="b") # Combine both averaging methods. "p" for only point method, "l" for line method.

# Uncomment png() to save plot as png, also uncomment dev.off() on line 42.
#png(filename = "output/averageTrack.png", width=2048, height=2048, res=200)
 plot(tracks[tracks$routeID==1,2:1], type="l", pch=2, col="green")
 lines(tracks[tracks$routeID==2,2:1], type="l", pch=2, col="green")
 lines(averageTrack[,2:1], type="l", pch=1, col="red")

 legend.text <- c(fileNames[1], fileNames[2], "Average")
 legend.col <- c("green", "blue", "black")
 legend.lty <- "solid"
 legend.pch <- c(2,1,4) 
 legend.position <- "topleft"
 legend(x=legend.position, legend=legend.text, col=legend.col, lty=legend.lty, pch=legend.pch)
#dev.off() # Uncomment to save plot.

fSaveTrack(
	track=averageTrack,
	outputDir="output", # Directory where output file will be saved.
	trk="20120121", # Part of the trackname. "track" will be placed in front of "trk" value.
	roundTo=6)

