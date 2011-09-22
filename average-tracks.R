# average tracks is an R script to make an "average" track of several GPS
# tracks by combining/merging them into a "line of best fit". Made by
# Michiel Faber <http://www.openstreetmap.org/user/Michiel%20Faber>
# This software is dedicated to the public domain through CC0:
# http://creativecommons.org/publicdomain/zero/1.0/
#
# See http://wiki.openstreetmap.org/wiki/average_tracks for usage instructions.
# Home: https://github.com/dittaeva/average-tracks
# version: 0.4
# date: 31-aug-2011

# Save the start time
beginTime <- proc.time()


# Import all *.csv files found in the working dir
listOfFiles <- list.files(pattern = ".csv")
fileCount <- length(listOfFiles)
ntracks <- 0
for (n in 1:fileCount) {
# read files
  fileName <- listOfFiles[n]
  route <- read.csv(file=fileName, header=FALSE)  # dont import the header

# test if route contains points. We could have clipped the complete route 
  if (nrow(route) > 0) {
# overwrite the inputfile with the clipped route
#    write.table(route, file = fileName, quote = FALSE, row.names=FALSE, col.names=FALSE, sep=",", dec=".")

    ntracks <- ntracks + 1 # count the number of routes
    colnames(route) <- c("lon", "lat", "routeID") # set column names
    route[,3] <- rep(ntracks,each=nrow(route)) # each route gets it own routeID
#combine the routes
    if (ntracks == 1) { 
      tracks <- route # the first route is added directly
    } else {
      tracks <- rbind(tracks,route) # all later routes are appended
    }
  }
}

# Find the track with the highest number of points
# count per id
# find the highest count
max_points=0
for (i in 1:ntracks) { # search in all routeID's
  tmpdf <- tracks[tracks$routeID==i, ] # [,3] is the column with the routeID
  if (max_points < nrow(tmpdf)) {
    max_points <- nrow(tmpdf)
    refRouteID <- i # refRoute is the route with the moist gps-points.
    refRouteID
  }
}
refRouteID

# Find distance between two points
refRouteXcor <- tracks[tracks$routeID==refRouteID, 1]
refRouteYcor <- tracks[tracks$routeID==refRouteID, 2]
newTracks <- tracks[tracks$routeID==refRouteID, ] # store refRoute in a new list of routes
averageTrack <- newTracks
averageTrack$lon <- 0 # set all x coordinates to 0
averageTrack$lat <- 0 # set all y coordinates to 0
averageTrack <- averageTrack[,-3] # remove the routeID
#averageTrack[, 3] <- ntracks+1 # set correct routeID
 
# for all points in refRoute -> k
# for all tracks in tracks -> l
# for all points in one track -> m
for (l in 1:ntracks) {
  if (l != refRouteID) {
    for (k in 1:max_points) {
      RouteXcor <- tracks[tracks$routeID==l, 1]
      RouteYcor <- tracks[tracks$routeID==l, 2]
      shortest_distance=999999
      for (m in 1:length(RouteXcor)) {
        distance <- sqrt((RouteXcor[m]-refRouteXcor[k])^2+(RouteYcor[m]-refRouteYcor[k])^2)
        if (distance < shortest_distance) {
          shortest_distance <- distance
          pointID <- m
        }
      }
      newTracks <- rbind(newTracks, c(RouteXcor[pointID], RouteYcor[pointID], l)) # add closest point to refRoute-point for each route
    }
  }
}

# Average all tracks into one single track
for (n in 1:(max_points)) {
  for (m in 1:(ntracks)) {
    if (m == 1) {
      valueX <- newTracks[n, 1]
      valueY <- newTracks[n, 2]
    } else {
      valueX <- valueX + newTracks[n+(m-1)*max_points, 1]
      valueY <- valueY + newTracks[n+(m-1)*max_points, 2]
    }
  }
  averageTrack[n, 1] <- valueX / ntracks
  averageTrack[n, 2] <- valueY / ntracks
}

# cleaning up
#averageTrack <- averageTrack[,-3] # remove the routeID
averageTrack <- round(averageTrack, 6) # round to 6 decimal places

cat("Done. \n")
cat("The dataframe 'averageTrack' contains the newly computed average track. \n")
cat("\n")

#Saving the resulting track in averageTrack.csv.
cat("Saving the resulting track in averageTrack.csv. \n")
write.table(averageTrack, file = "averageTrack.csv", quote = FALSE, row.names=FALSE, col.names=FALSE, sep=",", dec=".")

# Show duration of script
endTime <- proc.time()
duration <- endTime - beginTime
duration

