#-----------------------------


##############################
#   function fAverageTrack   #
##############################

fAverageTrack <- function(track1, track2, elevation=FALSE, method="b", routes="s") {
# ============
# Description:
#
# 	This function computes and returns a set of average points between two (unequal) sets of points (representing lines).
# 	If points include height information, it can compute average height too.
#
# Usage:
#
# 	fAverageTrack(track1, track2, elevation=FALSE, method="b", routes="s")
#
# Arguments:
#
# 	track1, track2: a data frame of coordinates. Collumn names should be provided and should include "lon" and "lat". 
#                  	If elevation is set to TRUE, "height" must be present in the column names.
#
#      	     elevation: if set to TRUE, elevation will be taken into account and will be averaged accordingly.
#
# 	 	method: the averaging method to be used. This must be one of "p"(oint), "l"(ine) or "b"(oth). Method "p" calculates 
#                  	the average by looking for the closest points and averages between them. Method "l" calculates the average
#                  	by finding the closest points on a line drawn between the set of points. Methob "b" uses both methods 
#                  	and averages between those methods.
#
#         	routes: Indicates which set of points should be the reference set for the averaging. "s"(ingle) indicates that the set 
#                 	with the most points will be used as reference set. If track1 and track2 have the same amount of points 
#                  	track1 will be used as reference set. To use the set with the least points (or if equal, track2) set routes to "r"(everse).
#                  	One can use both methods by setting routes to "d"(ouble). **Routes "d" is not implemented.** 
# Details:
#
#	For more information on how the actual calculations is done see the help of the functions fAverageTrackCloseToPoint() and fAverageTrackCloseToLine()
#	... ... ...
#
# Value:
#
#	A data frame with three columns; (lat, lon, height)
#
# Author(s):
#
#	Michiel Faber
#
# References:
#
#	... ... ...
#
# See Also:
#
#	'fAverageTrackCloseToPoint', 'fAverageTrackCloseToLine', 'fCreateTrack'.
#
# Examples:

 comment <- TRUE # only dummy to prevent executing of example
 if (!comment) {

# Example 1: 
  lat <- c(52.87233105,52.872332,52.872345,52.872359,52.872361,52.87237056)
  lon <- c(6.392546565,6.392541,6.392537,6.392537,6.392535,6.392534778)
  height <- c(-0.569145, 2.089000,2.443000,1.500000,1.000000,-0.332145)
  route1 <- data.frame(lat, lon, height)
  # route1 <- fCreateTrack(lat,lon,height,1)
  lat <- c(52.87233105,52.87236500,52.87237056)
  lon <- c(6.392546565,6.392537000,6.392534778)
  height <- c(-0.569145,-3.227290,-0.332145)
  route2 <- data.frame(lat, lon, height)
  # route2 <- fCreateTrack(lat,lon,height,2)
  newRoute.Points <- fAverageTrack(route1, route2, elevation = TRUE, method="p")
  newRoute.Lines <- fAverageTrack(route1, route2, elevation = TRUE, method="l")
  newRoute.Both <- fAverageTrack(route1, route2, elevation = TRUE, method="b")

  plot(route1[,2:1], type="b", pch=2, col="green")
  lines(route2[,2:1], type="b", pch=2, col="green")
  lines(newRoute.Points[,2:1], type="b", pch=1, col="red") 
  lines(newRoute.Lines[,2:1], type="b", pch=1, col="blue") 
  lines(newRoute.Both[,2:1], type="b", pch=4, col="black") 
  legend.text <- c("route1, route2", "PointAverage", "LineAverage", "BothAverage")
  legend.col <- c("green", "red", "blue", "black")
  legend.lty <- "solid"
  legend.pch <- c(2,1,1,4) 
  legend(x="topright", legend=legend.text, col=legend.col, lty=legend.lty, pch=legend.pch)

# Example 2:
  lat <- c(1, 1.5, 2.8, 3.7, 4.2, 5.7)
  lon <- c(1, 2, 3, 4, 5, 6)
  height <- c(-0.569145, 2.089000,2.443000,1.500000,1.000000,-0.332145)
  route3 <- fCreateTrack(lat,lon,height,3)
  lat <- c(1, 1, 2.3, 3.5, 5.7)
  lon <- c(1, 2.3, 4, 5.2, 6)
  height <- c(-0.569145, -3.227290, -0.332145, -2, -2.5)
  route4 <- fCreateTrack(lat,lon,height,4)
  newRoute.Points <- fAverageTrack(route3, route4, elevation = TRUE, method="p")
  newRoute.Lines <- fAverageTrack(route3, route4, elevation = TRUE, method="l")
  newRoute.Both <- fAverageTrack(route3, route4, elevation = TRUE, method="b")

  plot(route3[,2:1], type="b", pch=2, col="green")
  lines(route4[,2:1], type="b", pch=2, col="green")
  lines(newRoute.Points[,2:1], type="b", pch=1, col="red") 
  lines(newRoute.Lines[,2:1], type="b", pch=1, col="blue") 
  lines(newRoute.Both[,2:1], type="b", pch=4, col="black") 
  legend.text <- c("route3, route4", "PointAverage", "LineAverage", "BothAverage")
  legend.col <- c("green", "red", "blue", "black")
  legend.lty <- "solid"
  legend.pch <- c(2,1,1,4) 
  legend(x="topleft", legend=legend.text, col=legend.col, lty=legend.lty, pch=legend.pch)

  rm(lat,lon,height)
 } # end comment


######
# Begin function
######
 
 if (nrow(track1) >= nrow(track2)) {
  referenceTrack <- track1
  shortTrack <- track2 
 } else {
  referenceTrack <- track2
  shortTrack <- track1
 }

 if (method == "p") {
  if (routes == "r") {
   averageTrack <- fAverageTrackCloseToPoint(shortTrack, referenceTrack)
  } else if (routes == "s") {
    averageTrack <- fAverageTrackCloseToPoint(referenceTrack, shortTrack) 
  }
 } else if (method == "l") {
  if (routes == "r") {
   averageTrack <- fAverageTrackCloseToLine(shortTrack, referenceTrack)
  } else if (routes == "s") {
   averageTrack <- fAverageTrackCloseToLine(referenceTrack, shortTrack)
  }
 } else if (method == "b") {
  if (routes == "r") {
   averageTrackPoint <- fAverageTrackCloseToPoint(shortTrack, referenceTrack)
   averageTrackLine <- fAverageTrackCloseToLine(shortTrack, referenceTrack)
  } else if (routes == "s") {
   averageTrackPoint <- fAverageTrackCloseToPoint(referenceTrack, shortTrack)
   averageTrackLine <- fAverageTrackCloseToLine(referenceTrack, shortTrack)
  }
  averageTrack <- (averageTrackPoint+averageTrackLine)/2
 }
return(averageTrack)
}
#-----------------------------


###########################################
#   function fAverageTrackCloseToPoint   #
##########################################

fAverageTrackCloseToPoint <- function(referenceTrack, shortTrack, elevation=FALSE) {
# ============
# Description:
#
# 	This function computes and returns a set of average points between two (unequal) sets of points.
# 	If points include height information, it can compute average height too.
#
# Usage:
#
# 	fAverageTrack(referenceTrack, shortTrack, elevation=FALSE)
#
# Arguments:
#
# 	referenceTrack: a data frame of coordinates. Collumn names should be provided and should include "lon" and "lat". 
#                  	If elevation is set to TRUE, "height" must be present in the column names.
#
# 	    shortTrack: same as referenceTrack
#
#      	     elevation: if set to TRUE, elevation will be taken into account and will be averaged accordingly.
#
# Details:
#
#	Each point in referenceTrack is averaged with the closest point in shortTrack. Distance is the Euclidean distance. 
#
# Value:
#
#	A data frame with three columns; (lat, lon, height)
#
# Author(s):
#
#	Michiel Faber
#
# References:
#
#	... ... ...
#
# See Also:
#
#	'fAverageTrack', 'fAverageTrackCloseToLine', 'fCreateTrack'.
#
# Examples:

 comment <- TRUE # only dummy to prevent executing of example
 if (!comment) {

# Example 1: 
  lat <- c(52.87233105,52.872332,52.872345,52.872359,52.872361,52.87237056)
  lon <- c(6.392546565,6.392541,6.392537,6.392537,6.392535,6.392534778)
  height <- c(-0.569145, 2.089000,2.443000,1.500000,1.000000,-0.332145)
  route1 <- fCreateTrack(lat,lon,height,1)
  lat <- c(52.87233105,52.87236500,52.87237056)
  lon <- c(6.392546565,6.392537000,6.392534778)
  height <- c(-0.569145,-3.227290,-0.332145)
  route2 <- fCreateTrack(lat,lon,height,2)
  newRoute <- fAverageTrackCloseToPoint(route1, route2, elevation=TRUE)
 }

######
# Begin function
######

 # Define helper function
 f <- function(x) {
  pTol <- sqrt((shortTrack$lat-referenceTrack$lat[x])^2+(shortTrack$lon-referenceTrack$lon[x])^2) # find all distances to current point 'x'
  selPointShort <- pTol == min(pTol) # search closest point
  d1 <- sum(referenceTrack$lat[x],shortTrack$lat[selPointShort][1])/2 # calculate average Y
  d2 <- sum(referenceTrack$lon[x],shortTrack$lon[selPointShort][1])/2 # calculate average X
  if (elevation) { # if there is height
   d3 <- sum(referenceTrack$height[x],shortTrack$height[selPointShort][1])/2 # calculate average Z
   dd <- rbind(d1,d2,d3)
  } else {
   dd <- rbind(d1,d2)
  }
 return(dd)
 }
 sapplyTrack <- as.data.frame(t(sapply(1:nrow(referenceTrack), FUN=f))) # For all points in reference Track, calculate average with closest point
 if (!elevation) { # if there is no height
  sapplyTrack$height <- NA
 }
 colnames(sapplyTrack) <- columnNames <- c("lat", "lon", "height")
 averageTrack <- sapplyTrack
 averageTrack$pointID <- seq(1:nrow(averageTrack))
return(averageTrack)
}
#-----------------------------


#########################################
#   function fAverageTrackCloseToLine   #
#########################################

fAverageTrackCloseToLine <- function(referenceTrack, shortTrack, elevation=FALSE) {
# ============
# Description:
#
# 	This function computes and returns a set of average points between two (unequal) sets of points.
# 	If points include height information, it can compute average height too.
#
# Usage:
#
# 	fAverageTrack(referenceTrack, shortTrack, elevation=FALSE)
#
# Arguments:
# 	referenceTrack: a data frame of coordinates. Collumn names should be provided and should include "lon" and "lat". 
#                  	If elevation is set to TRUE, "height" must be present in the column names.
#
# 	    shortTrack: same as referenceTrack
#
#      	     elevation: if set to TRUE, elevation will be taken into account and will be averaged accordingly.
#
# Details:
#
#	For each point in referenceTrack the closest point on the line made from shortTrack is computed. 
#	Those new set of points is averaged. Distance is the Euclidean distance. 
#
# Value:
#
#	A data frame with three columns; (lat, lon, height)
#
# Author(s):
#
#	Michiel Faber
#
# References:
#
#	... ... ...
#
# See Also:
#
#	'fAverageTrack', 'fAverageTrackCloseToPoint', 'fCreateTrack', 'fFindPointOnLineClosestToPoint'.
#
# Examples:

 comment <- TRUE # only dummy to prevent executing of example
 if (!comment) {

# Example 1: 
  lat <- c(52.87233105,52.872332,52.872345,52.872359,52.872361,52.87237056)
  lon <- c(6.392546565,6.392541,6.392537,6.392537,6.392535,6.392534778)
  height <- c(-0.569145, 2.089000,2.443000,1.500000,1.000000,-0.332145)
  route1 <- fCreateTrack(lat,lon,height,1)
  lat <- c(52.87233105,52.87236500,52.87237056)
  lon <- c(6.392546565,6.392537000,6.392534778)
  height <- c(-0.569145,-3.227290,-0.332145)
  route2 <- fCreateTrack(lat,lon,height,2)
  newRoute <- fAverageTrackCloseToLine(route1, route2, elevation=TRUE)
 }

######
# Begin function
######

 # prepare the sets of points.
 nPoints <- nrow(referenceTrack)
 referenceTrack$pointID <- seq(1:nPoints)
 shortTrack$pointID <- seq(1:nrow(shortTrack))
 referenceTrack <- fCalculateSlope(referenceTrack)
 shortTrack <- fCalculateSlope(shortTrack)

 newPoint1 <- NA
 newPoint2 <- NA
 averageTrack <- matrix(NA, nPoints, 3)
 averageTrack <- as.data.frame(averageTrack)
 colnames(averageTrack) <- c("lat","lon","height")


 for (x in 1:(nPoints)) {

  referenceTrackPoint <- referenceTrack[x,]
  pTol <- sqrt((shortTrack$lat-referenceTrackPoint$lat)^2+(shortTrack$lon-referenceTrackPoint$lon)^2) # find all distances to current point 'x'
  selectPointShort <- pTol == min(pTol) # search closest point
  shortTrackPoint <- shortTrack[selectPointShort,][1,]

  if (shortTrackPoint$pointID+1 == (max(shortTrack$pointID)+1) ) {
   nextShortTrackPoint <- NA
  } else {
   nextShortTrackPoint <- shortTrack[shortTrack$pointID==shortTrackPoint$pointID+1,]
  } 

  if (shortTrackPoint$pointID-1 == 0) {
   previousShortTrackPoint <- NA
  } else {
   previousShortTrackPoint <- shortTrack[shortTrack$pointID==shortTrackPoint$pointID-1,]
  } 

  if (!is.na(shortTrackPoint$slope)) {
   newPoint1 <- fFindPointOnLineClosestToPoint(referenceTrackPoint, shortTrackPoint)
  }
  if (!is.na(nextShortTrackPoint[1]) & !is.na(nextShortTrackPoint)[1]) {
  newPoint2 <- fFindPointOnLineClosestToPoint(referenceTrackPoint, nextShortTrackPoint)
  }

  isPointIn <- FALSE
  if (!is.na(newPoint1)[1]) {   
   if (!is.na(previousShortTrackPoint)[1]) {
    if (newPoint1$lon >= min(previousShortTrackPoint$lon, shortTrackPoint$lon)) {
     if (newPoint1$lat <= max(previousShortTrackPoint$lat, shortTrackPoint$lat)) {
      isPointIn <- TRUE
     }
    }
   } 
  }
  is.newPoint1 <- isPointIn

  isPointIn <- FALSE
  if (!is.na(newPoint2)[1]) {
   if (!is.na(nextShortTrackPoint)[1]) {
    if (newPoint2$lon >= min(nextShortTrackPoint$lon, shortTrackPoint$lon)) { 
     if (newPoint2$lat <= max(nextShortTrackPoint$lat, shortTrackPoint$lat)) {
      isPointIn <- TRUE
     } 
    }
   }
  }
  is.newPoint2 <- isPointIn

  testResult <- which(c(is.newPoint1,is.newPoint2))
  if (length(testResult) == 0) {
   targetPoint <- shortTrackPoint
  } else {
   if (length(testResult) == 2) {
    distance1 <- sqrt((newPoint1$lat-referenceTrackPoint$lat)^2+(newPoint1$lon-referenceTrackPoint$lon)^2)
    distance2 <- sqrt((newPoint2$lat-referenceTrackPoint$lat)^2+(newPoint2$lon-referenceTrackPoint$lon)^2)
    if (which.min(c(distance1,distance2)) == 1) {
     targetPoint <- newPoint1
    } else {
     targetPoint <- newPoint2
    }
   } else {
    if (testResult == 1) {
     targetPoint <- newPoint1
    } else {
     targetPoint <- newPoint2
    }  
   }
  }
  averageTrack[x,c("lat","lon")] <- (referenceTrackPoint[c("lat","lon")]+targetPoint[c("lat","lon")])/2
 }
 averageTrack$pointID <- seq(1:nrow(averageTrack))
return(averageTrack)
}

#-----------------------------


###############################################
#   function fFindPointOnLineClosestToPoint   #
###############################################

fFindPointOnLineClosestToPoint <- function(refTrackPoint, shortTrackPoint) {
# ============
# Description:
#
# 	This function computes and returns a point on the line with slope and intercept of shortTrackPoint. 
#	The point found is closest to refTrackpoint.The line from refTrackPoint to the newly found point is orthogonal to the line of shortTrackPoint
#
# Usage:
#
# 	fFindPointOnLineClosestToPoint(refTrackPoint, shortTrackPoint)
#
# Arguments:
# 	refTrackPoint: a data frame with one set of coordinates. Column names should be provided and should include "lon" and "lat". 
#
#     shortTrackPoint: same as refTrackPoint. shortTrackPoint also needs columns with "slope" and "intercept", with appriorate values.
#
# Details:
#
#	... ... ...
#
# Value:
#
#	A data frame with three columns; (lat, lon, height)
#
# Author(s):
#
#	Michiel Faber
#
# References:
#
#	... ... ...
#
# See Also:
#
#
# Examples:

 comment <- TRUE # only dummy to prevent executing of example
 if (!comment) {

# Example 1: 
  lat <- c(52.87233105,52.872332,52.872345,52.872359,52.872361,52.87237056)
  lon <- c(6.392546565,6.392541,6.392537,6.392537,6.392535,6.392534778)
  height <- c(-0.569145, 2.089000,2.443000,1.500000,1.000000,-0.332145)
  route1 <- fCreateTrack(lat,lon,height,1)
  lat <- c(52.87233105,52.87236500,52.87237056)
  lon <- c(6.392546565,6.392537000,6.392534778)
  height <- c(-0.569145,-3.227290,-0.332145)
  route2 <- fCreateTrack(lat,lon,height,2)
  newRoute <- fAverageTrackCloseToLine(route1, route2, elevation=TRUE)
 }

######
# Begin function
######

 newAB <- fFindOrthogonalLine(refTrackPoint, shortTrackPoint)
 newCoord <- fFindIntersection(shortTrackPoint, newAB$slope, newAB$intercept)
 newPointWithLine <- cbind(newCoord,newAB)
return(newPointWithLine)
}
#-----------------------------


###############################################
#   function fFindOrthogonalLine   #
###############################################

fFindOrthogonalLine <- function(refTrackPoint, shortTrackPoint) {
# ============
# Description:
#
# 	This function computes and returns a point on the line with slope and intercept of shortTrackPoint. 
#	The point found is closest to refTrackpoint.The line from refTrackPoint to the newly found point is orthogonal to the line of shortTrackPoint
#
# Usage:
#
# 	fFindOrthogonalLine(refTrackPoint, shortTrackPoint)
#
# Arguments:
#
# 	refTrackPoint: a data frame with one set of coordinates. Column names should be provided and should include "lon" and "lat". 
#
#     shortTrackPoint: same as refTrackPoint. shortTrackPoint also needs columns with "slope" and "intercept", with appriorate values.
#
# Details:
#
#	... ... ...
#
# Value:
#
#	... ... ...
#
# Author(s):
#
#	Michiel Faber
#
# References:
#
#	... ... ...
#
# See Also:
#
#	... ... ...
#
# Examples:
#
#	... ... ...
#

 newSlope <- fCalculateOthogonalSlope(shortTrackPoint$slope)
 newIntercept <- fCalculateIntercept(refTrackPoint,newSlope)
 newAB <- as.data.frame(t(c(newSlope,newIntercept)))
 colnames(newAB) <- c("slope","intercept")
return(newAB)
}
#-----------------------------


###############################################
#   function fFindIntersection   #
###############################################

fFindIntersection <- function(TrackPoint, newSlope, newIntercept) {
# ============
# Description:
#
#	... ... ...
#
# Usage:
#
# 	fFindIntersection(TrackPoint, newSlope, newIntercept)
#
# Arguments:
#
#	... ... ...
#
# Details:
#
#	... ... ...
#
# Value:
#
#	... ... ...
#
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
# Examples:
#
#	... ... ...
#

 newX <- fFindXOfIntersection(TrackPoint$slope, TrackPoint$intercept, newSlope, newIntercept)
 newY <- fCalculateY(newX, newSlope, newIntercept)
 newCoord <- as.data.frame(t(c(newY,newX)))
 colnames(newCoord) <-c("lat","lon") 
return(newCoord)
}

fCalculateOthogonalSlope <- function(slope) {
 if (slope==0) {
  otherSlope <- 999999
 } else {
  otherSlope <- -1/slope
 }
return(otherSlope)
}

fCalculateIntercept <- function(TrackPoint,slope) {
 intercept <- TrackPoint$lat - (slope * TrackPoint$lon)
return(intercept)
}

fFindXOfIntersection <- function(slope1, intercept1, slope2, intercept2) {
 x <- (intercept2 - intercept1) / (slope1 - slope2)
return(x)
}

fCalculateY <- function(x, slope, intercept) {
 y <- slope * x + intercept
return(y)
}

