# chart to show variety of reaches to targets

# get baselines

BaseRightVertInd <- resultinfo3[resultinfo3$TSide=="Rgt" & resultinfo3$TFeature=="Vert" & resultinfo3$Difficulty=="Bas", "eventnumber"]
BaseRightHorzInd <- resultinfo3[resultinfo3$TSide=="Rgt" & resultinfo3$TFeature=="Horz" & resultinfo3$Difficulty=="Bas", "eventnumber"]
BaseLeftVertInd <- resultinfo3[resultinfo3$TSide=="Lft" & resultinfo3$TFeature=="Vert" & resultinfo3$Difficulty=="Bas", "eventnumber"]
BaseLeftHorzInd <- resultinfo3[resultinfo3$TSide=="Lft" & resultinfo3$TFeature=="Horz" & resultinfo3$Difficulty=="Bas", "eventnumber"]

plot(x=NA, xlab="x-Coordinate", ylab ="y-Coordinate", xlim=c(-0.7, 0.7), ylim=c(0,1))

for (i in 1:length(BaseLeftHorzInd))
{
  xUnc <- resultdata3[BaseLeftHorzInd[i], 1,]
  yUnc <- resultdata3[BaseLeftHorzInd[i], 2,]
  
  exptrajectory[1,] <- exptrajectory[1,]*(sin(30*pi/180)/exptrajectory[1,101])
  exptrajectory[2,] <- exptrajectory[2,]*(sin(60*pi/180)/exptrajectory[2,101])
  
  lines(x, y, col=alpha("darkblue", alpha=0.1))
}