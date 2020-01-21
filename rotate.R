#rotate lines script

rotate <- function(traj, correct="Correct")
{
  f <- 1
  if (correct=="Incorrect")
    {
    f <- -1
    #print(f)
  }
  track <- list()
  track$x <- traj[1,]*(sin(f*30*pi/180)/traj[1,101])
  track$y <- traj[2,]*(sin(60*pi/180)/traj[2,101])
  return(track)
}

