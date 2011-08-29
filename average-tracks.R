# R script to make an "average" track of several GPS tracks by
# combining/merging them into a "line of best fit". Copyright 2011
# Michiel Faber <http://www.openstreetmap.org/user/Michiel%20Faber>
#
# See http://wiki.openstreetmap.org/wiki/Average_tracks for usage instructions.
# Home: https://github.com/dittaeva/Average-tracks

# read 2 files
route1 <- read.csv ("input1.csv", header=FALSE) 
route2 <- read.csv ("input2.csv", header=FALSE)
# do some pre-processing
colnames(route1) <- c("lon", "lat", "routeID")
colnames(route2) <- c("lon", "lat", "routeID")
#combine the routes
tracks <- rbind(route1,route2)
# add routeID
tracks[,3] <- c(rep(1,each=nrow(route1)),rep(2,each=nrow(route2)))
ntracks <- 2
# Find the track with the highest number of points
max_points=0
# count per id
# find the highest count
for (i in 1:ntracks) {
  tmpdf <- tracks[tracks[, 3]==i, ]
  if (max_points < nrow(tmpdf)) {
    max_points <- nrow(tmpdf)
    refRouteID <- i
    refRouteID
  }
}
# Find distance between two points
refRouteXcor <- tracks[tracks[, 3]==refRouteID, 1]
refRouteYcor <- tracks[tracks[, 3]==refRouteID, 2]
newTracks <- tracks[tracks[, 3]==refRouteID, ]
averageTrack <- newTracks
averageTrack[,3] <- ntracks+1
# for all points in refRoute -> k
# for all tracks in tracks -> l
# for all points in a track -> m
for (l in 1:ntracks) {
  if (l != refRouteID) {
    for (k in 1:max_points) {
      RouteXcor <- tracks[tracks[, 3]==l, 1]
      RouteYcor <- tracks[tracks[, 3]==l, 2]
      shortest_distance=999999
      for (m in 1:length(RouteXcor)) {
        distance <- sqrt((RouteXcor[m]-refRouteXcor[k])^2+(RouteYcor[m]-refRouteYcor[k])^2)
        if (distance < shortest_distance) {
          shortest_distance <- distance
          pointID <- m
        }
      }
      newTracks <- rbind(newTracks, c(RouteXcor[pointID],RouteYcor[pointID],l))
    }
  }
}
# Average all tracks into one single track
for (n in 1:(max_points)) {
  averageTrack[n,1] <- (newTracks[n,1] + newTracks[n+max_points,1]) / ntracks
  averageTrack[n,2] <- (newTracks[n,2] + newTracks[n+max_points,2]) / ntracks
}
#Saving the resulting track in averageTrack.csv.
averageTrack <- averageTrack[,-3]
write.table(averageTrack, file = "averageTrack.csv", quote = FALSE, row.names=FALSE, col.names=FALSE, sep=",", dec=".")
