# get baseline correct trials

participant <- 3

Difficulty <- "Bas"
Side <- "Lft"
Ori <- "Vert"

Eventindices <- resultinfo3[resultinfo3$pname==participant &
                            resultinfo3$Difficulty==Difficulty &
                            resultinfo3$TSide==Side & 
                            resultinfo3$TFeature==Ori &
                            resultinfo3$mainreach=="Correct",1]

tracks <- resultdata3[Eventindices, 3:4,]

# align termination points

endx <- sin(30*pi/180)
endy <- sin(60*pi/180)

for (i in 1:dim(tracks)[1])
{
  tracks[i,1,] <- tracks[i,1,]*(endx/tracks[i,1,101])
  tracks[i,2,] <- tracks[i,2,]*(endy/tracks[i,2,101])
}

# find average (distance)

meantrack <- rbind(x=apply(tracks[,1,], 2, mean),
                   y=apply(tracks[,2,], 2, mean))

#then for each section of
perfectx <- (0:100)*endx/100
perfecty <- (0:100)*endy/100

perfect <- rbind(x=perfectx, y=perfecty)

warp <- perfect - meantrack 

#now for each distance section of a track, we have a modification vector

#apply this to the tracks from a non-baseline condition

Difficulty <- "Hrd"
Side <- "Lft"
Ori <- "Vert"

Eventindices <- resultinfo3[resultinfo3$pname==participant &
                              resultinfo3$Difficulty==Difficulty &
                              resultinfo3$TSide==Side & 
                              resultinfo3$TFeature==Ori &
                              resultinfo3$mainreach=="Correct",1]

trackC <- resultdata3[Eventindices, 3:4,]


endx <- sin(30*pi/180)
endy <- sin(60*pi/180)

for (i in 1:dim(trackC)[1])
{
  trackC[i,1,] <- trackC[i,1,]*(endx/trackC[i,1,101])
  trackC[i,2,] <- trackC[i,2,]*(endy/trackC[i,2,101])
}

# find average (distance)

meantrack <- rbind(x=apply(trackC[,1,], 2, mean),
                   y=apply(trackC[,2,], 2, mean))

trackW <- array(NA, dim = dim(trackC))

for (i in 1:dim(tracks)[1])
{
  trackW[i,,] <- trackC[i,,] + warp
}

#now you have modified tracks in trackW! Yay.

#now get the AUCs for each of these...

plot(trackW[1,1,], trackW[1,2,], type='l')
for (i in 1:30){
  points(trackW[i,1,], trackW[i,2,], type='l')
}

#right we have to back to the "track sectioner" -
#probably easier than before as we know the start and end points.
#plot()

AUCs <- array(NA, dim = dim(trackW)[1])

for (i in 1:dim(trackW)[1]){
  print(i)
  track <- trackW[i,,]                            #get current trajectory
  isabove <- atan2(track[2,], track[1,])>pi/3     #find data points that are above or below perfect trajectory line
  isabove[1] <- isabove[2]                        #first and last will be on line - correct for spurious changes
  isabove[101] <- isabove[100]
  secstarts <- c(1,which(diff(isabove)!=0)+1)     #at which points does each section of track start and end?
  secends <- c(which(diff(isabove)!=0),101)
  crosspoints <- rbind(poly_crossings(perfect,track), c(0.5, sin(pi/3))) #find where sections end.
  
  auc <- 0
  
  for (j in 1:length(secstarts))
  {
    print(j)
    #for section j
    xs <- c(crosspoints[j,1], track[1,secstarts[j]:secends[j]], crosspoints[j+1,1])
    ys <- c(crosspoints[j,2], track[2,secstarts[j]:secends[j]], crosspoints[j+1,2])
    auc <- auc - polyarea(xs,ys)  #NB polyarea expects counterclockwise values
  }
  
  AUCs[i] <- auc
}

