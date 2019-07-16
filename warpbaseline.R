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

#apply this to the original tracks

for (i in 1:dim(tracks)[1])
{
  trackW[i,,] <- tracks[i,,] + warp
}

#now you have modified tracks in trackW! Yay.
