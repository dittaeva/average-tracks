# average tracks is an R script to make an "average" track of several GPS
# tracks by combining/merging them into a "line of best fit". Made by
# Michiel Faber <http://www.openstreetmap.org/user/Michiel%20Faber>
# This software is dedicated to the public domain through CC0:
# http://creativecommons.org/publicdomain/zero/1.0/
#
# See http://wiki.openstreetmap.org/wiki/average_tracks for usage instructions.
# Home: https://github.com/dittaeva/average-tracks
# version: 0.5
# date: 2014-01-02

# Uncomment to set working directory to something else than current directory
#setwd("~/")

# Save the start time
beginTime <- proc.time()


# Import all *.csv files found in the working dir
listOfFiles <- list.files(pattern = ".csv")
fileCount <- length(listOfFiles)
ntracks <- 0
tracks <- NULL
for (n in 1:fileCount) {
  # Each csv file is cleaned and appended to a data.frame with all tracks in it.
  
  # read files
  fileName <- listOfFiles[n]
  
  # Is there a header present?
  firstLine <- read.table(file=fileName, header=FALSE, sep=",", nrows=1, stringsAsFactors=F)
  hasHeader <- any(tolower(firstLine) %in% c("x", "lon", "longitude", "y", "lat", "latitude"))
  
  #read the file
  route <- read.table(file=fileName, header=hasHeader, sep=",", stringsAsFactors=F)
  
  # test if route contains points. 
  if (nrow(route) > 0) {
    
    ntracks <- ntracks + 1 # count the number of routes
    if (!hasHeader) {
      # assuming first two collums are latitude (y) and longitude (x)
      route <- route[, 1:2]
      colnames(route) <- c("lat", "lon") # set column names
      #route <- route[, colnames(route) %in% c("lon", "lat")] # only for cleaning test csv
    } else {
      # translate colnames to lat lon to make the script work
      colnames(route) <- gsub(c("x"), "lon", tolower(colnames(route)))
      colnames(route) <- gsub(c("longitude"), "lon", tolower(colnames(route)))
      colnames(route) <- gsub(c("latitude"), "lat", tolower(colnames(route)))
      colnames(route) <- gsub(c("y"), "lat", tolower(colnames(route)))
      # discard all other columns. They make no sense when averaging a track.
      route <- route[, colnames(route) %in% c("lon", "lat")]
    }
    #route <- route[, colnames(route) %in% c("lon", "lat")] # only for cleaning test csv
    route$routeID <- rep(ntracks, each=nrow(route)) # each route gets it own routeID
    
    #combine the routes
    tracks <- rbind(tracks, route) # all later routes are appended
    
  }
}

# The track with the most points is used as reference track
# in each other track, the point is found closest to the point in the reference track.
# than all tracks have the same ammount of points and can be averaged.

# Find the track with the highest number of points
# count per id
# find the highest count
max_points=0
for (i in 1:ntracks) { # search in all routeID's
  tmpdf <- tracks[tracks$routeID==i, ] 
  if (max_points < nrow(tmpdf)) {
    max_points <- nrow(tmpdf)
    refRouteID <- i # refRoute is the route with the moist gps-points.
  }
}

# Find distance between two points
refRouteXcor <- tracks$lon[tracks$routeID==refRouteID]
refRouteYcor <- tracks$lat[tracks$routeID==refRouteID]
newTracks <- tracks[tracks$routeID==refRouteID, ] # store refRoute in a new list of routes
averageTrack <- data.frame(lat=rep(0, nrow(newTracks)), lon=rep(0, nrow(newTracks)), stringsAsFactors=F)

# for all tracks in tracks -> l
# for all points in refRoute -> k
# for all points in one track -> m
for (l in 1:ntracks) {
  if (l != refRouteID) {
    for (k in 1:max_points) {
      RouteXcor <- tracks$lon[tracks$routeID==l]
      RouteYcor <- tracks$lat[tracks$routeID==l]
      shortest_distance=999999
      for (m in 1:length(RouteXcor)) {
        distance <- sqrt((RouteXcor[m]-refRouteXcor[k])^2+(RouteYcor[m]-refRouteYcor[k])^2)
        if (distance < shortest_distance) {
          shortest_distance <- distance
          pointID <- m
        }
      }
      newTracks <- rbind(newTracks, data.frame(lat=RouteYcor[pointID], lon=RouteXcor[pointID], routeID=l, stringsAsFactors=F)) # add closest point to refRoute-point for each route
    }
  }
}

# Average all tracks into one single track
for (n in 1:(max_points)) {
  for (m in 1:(ntracks)) {
    # Get correct value in each track
    valueX <- newTracks$lon[n+(m-1)*max_points] 
    valueY <- newTracks$lat[n+(m-1)*max_points]
    
    # add its part of the mean to the current value. Now more than 2 tracks can be merged.
    averageTrack$lon[n] <- averageTrack$lon[n] + valueX / ntracks
    averageTrack$lat[n] <- averageTrack$lat[n] + valueY / ntracks
  }
}

# cleaning up
averageTrack <- round(averageTrack, 6) # round to 6 decimal places

# make a plot-image
png(filename="averageTrack.png", width=2048, height=1024, res=200)
plot(averageTrack, type="l", col="red")
for (p in 1:max(tracks$routeID)) {
  lines(tracks[ tracks$routeID==p, c("lon", "lat")])
}
dev.off()

#Saving the resulting track in averageTrack.csv.
cat("Saving the resulting track in averageTrack.csv. \n")
write.table(averageTrack, file = "averageTrack.csv", quote = FALSE, row.names=FALSE, col.names=TRUE, sep=",", dec=".")

cat("Done. \n")
cat("The dataframe 'averageTrack' contains the newly computed average track. \n")
cat("\n")

# Show duration of script
endTime <- proc.time()
duration <- endTime - beginTime
duration
