#-----------------------------


###########################
#   function fLoadFiles   #
###########################
fLoadFiles <- function(inputDir, listOfFiles=NULL, fileType="csv", nTracks=0, loadHeader=FALSE, method="maptools") {

# Description:
# 	Import set of points (representing lines) stored in *.gpx, *.csv files or user specified files from a given directory.
# 	Additional to loading, it sorts the points and add line characteristics to each point.
#
# Usage:
#
#	fLoadFiles(inputDir, listOfFiles=NULL, fileType="csv", nTracks=0, loadHeader=FALSE, method="maptools")
#
# Arguments:
#
# 	      inputDir:	Directory where the files to import are in.
#
#	   listOfFiles:	A vector of files to import.
#
# 	      fileType:	type of files to imported. Currently only "gpx" and "csv" are supported. Only used when listOfFiles is not given.
#
# 	       nTracks:	number to start identifying each set of points. If you want to add to existing set of points increase this value appropiatly.
#
# 	    loadHeader:	logical value indicating if a header is present in the csv's files. 
#
# 	        method:	method to use when importing *.gpx files. Must be "maptools" or "rgdal". Both methods use the load functions from their 
#			respective packages.
#
# Details:
#
#	... ... ...
#
# Value:
#
#	A list with on first position a data frame with all routes and their points. Second is a list of the imported files.
#
# Author(s):
#
#	Michiel Faber
#
#
# References:
#
#	... ... ...
#
# See Also:
#
#	... ... ...
#

 columnNames <- c("lat", "lon", "height", "routeID", "pointID", "slope", "intercept", "pointType", "nJoins")

 if (nTracks == 0) {
  Tracks <- NULL # Define an empty Tracks object
 }
 if (is.null(listOfFiles)) {
  listOfFiles <- list.files(inputDir, pattern = fileType) 
 }
 fileCount <- length(listOfFiles) # number of files

 if (fileCount > 0) {
  for (fileNumber in 1:fileCount) {
  # read files
   fileName <- listOfFiles[fileNumber]
  if (fileType=="csv") {
   Route <- read.csv(file=paste(inputDir,fileName,sep="/"), header=loadHeader)  # dont import a header, because there is none
  }
  if (fileType=="gpx") {
   tmpRoute <- read1GPX(paste(inputDir,fileName,sep="/"), methodToUse=method)
   if (method=="maptools") {
    Route <- as.data.frame(tmpRoute[,3:4])
    Route[,3] <- as.numeric(substring(as.character(tmpRoute[,14]), 1, (nchar(as.character(tmpRoute[,14]))-1)))
   }
   if (method=="rgdal") {
    Route <- as.data.frame(tmpRoute@coords[,2:1])
    Route[,3] <- as.data.frame(tmpRoute$ele)
   }
  }
   if (nrow(Route) > 0) {  # test if route contains points.
    nTracks <- nTracks + 1 # keep count of the number of routes
    Route[,4] <- nTracks  # each route gets it own routeID
    Route[,5] <- c(1:nrow(Route)) # each point gets a pointID
    Route[,6] <- NA 
    Route[,7] <- NA
    Route[,8] <- NA  
    Route[,9] <- NA
    colnames(Route) <- columnNames # set column names
    Route <- fSortOnDirection(Route)
    Route <- fCalculateSlope(Route)
 #   Route <- fSetPointTypes(Route)
    # combine the routes
    if (nTracks == 1) { 
     Tracks <- Route # the first route is added directly
    } else {
     Tracks <- rbind(Tracks,Route) # all later routes are appended
    }
   }
  }
 }
 result <- list(Tracks, listOfFiles)
return(result)
}

read1GPX <- function(x, methodToUse="maptools") {
# http://stackoverflow.com/questions/6397523/read-multiple-gpx-files
 if (methodToUse=="maptools") {
  if(!require("maptools")){
   install.packages("maptools")
   if(!require("maptools")){
    stop("could not install pacakge 'maptools'.")
   }
  }
  dd <- readGPS(i = "gpx", f = x, type = "w")
 }
 if (methodToUse=="rgdal") {
  if(!require("rgdal")){
   install.packages("rgdal")
   if(!require("rgdal")){
    stop("could not install package 'rgdal'.")
   }
  }
  dd <- readOGR(x, layer="track_points")
 }
return(dd)
}

readGPX <- function(listOfFiles=NULL, method="maptools") {
# http://stackoverflow.com/questions/6397523/read-multiple-gpx-files
 if (is.null(listOfFiles)) {
  out <- NULL
  stop("Please provide a vector with filenames")
 }
 if (method=="maptools") {
  out <- lapply(listOfFiles, FUN=read1GPX)
 }
 if (method=="rgdal") {
  out <- lapply(listOfFiles, FUN=read1GPX, methodToUse=method)
 }
 names(out) <- listOfFiles
return(out)
}

fSortOnDirection <- function(track) {
 nPoints <- nrow(track)
 if (track$lon[1] == track$lon[nPoints]) {
  orderValues <- fOrderPoints(track$lat)
 } else {
  orderValues <- fOrderPoints(track$lon)
 }
 track <- track[orderValues,]
 track$pointID <- c(1:nPoints)
return(track)
}

fOrderPoints <- function(values) {
 nPoints <- length(values)
 orderValues <- c(1:nPoints)
 maxValue <- max(values[1], values[nPoints])
 selectMaxPoint  <- maxValue == c(values[1], values[nPoints])
 if (selectMaxPoint[1]) {
  orderValues <- c(nPoints:1)
 }
return(orderValues)
}

#-----------------------------


################################
#   function fCalculateSlope   #
################################

fCalculateSlope <- function(track) {
 # ============
# Description:
#
# 	This function calculates for a set of points (representing a line), for each segment between the points the intersect a slope 
#	of the line coming from the previos point. Therefor point one has no intercept or slope.
#
# Usage:

# 	fCalculateSlope(track)
#
# Arguments:
# 		 track: a data frame of coordinates. Column names should be provided and should include "lon" and "lat". 
#
# Details:
#
#	... ... ...
#
# Value:
#
#	A data frame with the original columns and 2 extra colums with names "slope" and "intercept".
#
# Author(s):
#
#	Michiel Faber
#
#
# References:
#
#	... ... ...
#
# See Also:
#
#	'fCreateTrack.
#
#
# Examples:
#	... ... ...

 comment <- TRUE # only dummy to prevent executing of example
 if (!comment) {

  lat <- c(1,2,3.5,4,5)
  lon <- c(2,3,4,5,6)
  track <- data.frame(lat,lon)
  newTrack <- fCalculateSlope(track)
  plot(newTrack[,2:1],type="b", xlim=c(-1,7),ylim=c(-3,7))
  for (x in 2:5) {
   lines(c(-1:7),newTrack$slope[x]*c(-1:7)+newTrack$intercept[x],col="green")
  }
  lines(newTrack[,2:1],type="b", xlim=c(-1,7),ylim=c(-3,7))

 } # end comment

######
# Begin function
######

 # Define function to be used by sapply()
 f <- function(i) { 
  if ((track$lon[i]-track$lon[i-1]) == 0) { # if line goes vertical...
   # to do: give a and b better values
   slope <- 999999
   intercept <- 999999
   result <- cbind(slope,intercept)
  } else {
   slope <- (track$lat[i]-track$lat[i-1]) / (track$lon[i]-track$lon[i-1])
   intercept <- track$lat[i-1] - slope * track$lon[i-1]
   result <- cbind(slope,intercept)
  }
 return(result)
 }
 slopes <- sapply(2:nrow(track), FUN=f)
 track$slope[2:nrow(track)] <- t(slopes)[,1]
 track$intercept[2:nrow(track)] <- t(slopes)[,2]
return(track)
}

fSetPointTypes <- function(track, beginType="b", midType="l", endType="e") {
 track$pointType[1] <- NA
 track$pointType[2:(nrow(track)-1)] <- midType # This one come first to make sure first and last point get the good values when nrow(track) = 2
 track$pointType[1] <- beginType
 track$pointType[nrow(track)] <- endType
return(track)
}

fSaveTrack <- function(track, outputDir, trk=NULL, roundTo=6) {
# function to save a track to GPX.
# If everything is converted to Spatial-classes rgdal/ogr functions can be used.
# Define gpx header
 lineHeader1 <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>" 
 lineHeader2 <- "<gpx xmlns=\"http://www.topografix.com/GPX/1/1\" version=\"1.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd\" creator=\"M. Faber\">"
 lineTrkStart <- "<trk>"
 lineTrkSegStart <- "<trkseg>"
 # lineTrkptStart <- # see below
 lineTrkptEnd <- "</trkpt>"
 lineTrkSegEnd <- "</trkseg>"
 lineTrkEnd <- "</trk>"
 lineFooter1 <- "</gpx>"

 if (is.null(trk)) {
  trk <- track$routeID[1]
 }
 trackName <- paste("Track:", trk, sep=" ")
 trackDescription <- "These are my first tracks created by R"
 lineTrackName <- paste("<name>", trackName, "</name>", sep="")
 lineTrackDescription <- paste("<desc>",  trackDescription, "</desc>", sep="")
 fileName <- paste("track", trk, ".gpx", sep="")
 outputName <- paste(outputDir, fileName, sep="/")
 cat(paste(lineHeader1, lineHeader2, lineTrkStart, lineTrackName, lineTrackDescription, lineTrkSegStart, "", sep="\n"), file=outputName)
 for (pnt in unique(track$pointID)) { 
  lineTrkptStart <- paste("<trkpt lon=\"", round(track$lon[pnt],roundTo), "\" lat=\"", round(track$lat[pnt],roundTo), "\">", sep="")
  if (!is.null(track$height[pnt])) {
   lineEle <- paste("<ele>", round(track$height[pnt],roundTo), "</ele>", sep="")
  } else { 
   lineEle <- paste("<ele>", NA, "</ele>", sep="") 
  }
  cat(paste(lineTrkptStart, lineEle, lineTrkptEnd, "", sep="\n"), file=outputName,  append=T)
 }
 cat(paste(lineTrkSegEnd, lineTrkEnd, lineFooter1, "", sep="\n"), file=outputName, append=T)
}

fSaveToOneTrack <- function(newTracks, trackName="name", outputDir=outputDir) {
# Define gpx header
 lineHeader1 <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>" 
 lineHeader2 <- "<gpx xmlns=\"http://www.topografix.com/GPX/1/1\" version=\"1.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd\" creator=\"M. Faber\">"
 lineTrkStart <- "<trk>"
 lineTrkSegStart <- "<trkseg>"
 # lineTrkptStart <- # see below
 lineTrkptEnd <- "</trkpt>"
 lineTrkSegEnd <- "</trkseg>"
 lineTrkEnd <- "</trk>"
 lineFooter1 <- "</gpx>"

 trackDescription <- "These are my first tracks created by R"
 lineTrackName <- paste("<name>", trackName, "</name>", sep="")
 lineTrackDescription <- paste("<desc>",  trackDescription, "</desc>", sep="")
 fileName <- paste(trackName, ".gpx", sep="")
 outputName <- paste(outputDir, fileName, sep="/")
 cat(paste(lineHeader1, lineHeader2, lineTrkStart, lineTrackName, lineTrackDescription, "", sep="\n"), file=outputName)
 for (trkID in unique(newTracks$routeID)) {
  selectTrack <- newTracks$routeID==trkID
  workTrack <- newTracks[selectTrack,]
  # function to save a track to GPX.
  cat(paste(lineTrkSegStart, "", sep="\n"), file=outputName,  append=T)
  for (pnt in unique(workTrack$pointID)) { 
   lineTrkptStart <- paste("<trkpt lon=\"", round(workTrack$lon[pnt],roundTo), "\" lat=\"", round(workTrack$lat[pnt],roundTo), "\">", sep="")
   if (!is.null(workTrack$height[pnt])) {
    lineEle <- paste("<ele>", round(workTrack$height[pnt],roundTo), "</ele>", sep="")
   } else { 
    lineEle <- paste("<ele>", NA, "</ele>", sep="") 
   }
   cat(paste(lineTrkptStart, lineEle, lineTrkptEnd, "", sep="\n"), file=outputName,  append=T)
  }
  cat(paste(lineTrkSegEnd, "", sep="\n"), file=outputName, append=T)
 }
 cat(paste(lineTrkEnd, lineFooter1, "", sep="\n"), file=outputName, append=T)
}

