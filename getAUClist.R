#script for getting and plotting values from the two AUC modification procedures.

#required output: list of sampled original AUC, sim + simply added AUC, sim + trajectory combined AUC.
getAUClist <- function(explines, simlines)
{
  if(length(dim(simlines))!=3) stop("dimemnsions of simlines should be 3")
  if(length(dim(explines))!=3) stop("dimemnsions of explines should be 3")
  
  #get dimensions of simlines
  arraydims <- dim(simlines)
  xycoordsdim <- which(arraydims==2) #find xycoords dimension
  timedim <- which(arraydims==101) #find time dimension
  trialsdim <- which(arraydims==arraydims[c(-xycoordsdim, -timedim)]) #remaining value is ntrials dimension
  #permute explines to standard format
  sims <- aperm(simlines, c(xycoordsdim, timedim, trialsdim))
  
  nsims <- dim(simlines)[3]
  
  #get dimensions of explines
  arraydims <- dim(explines)
  xycoordsdim <- which(arraydims==2) #find xycoords dimension
  timedim <- which(arraydims==101) #find time dimension
  trialsdim <- which(arraydims==arraydims[c(-xycoordsdim, -timedim)]) #remaining value is ntrials dimension
  #permute explines to standard format
  exps <- aperm(explines, c(xycoordsdim, timedim, trialsdim))
  
  nexp <- dim(exps)[3]

  #now you have sims and exps having same shape
  
  #counter <- 0
  
  AUCout <- array(numeric(), c(0,6))
  
  colnames(AUCout) <- c("nSim","nExp","expAUC", "simAUC", "AUCadded", "AUCmodcoord")
  
  for (simnumber in sample(1:nsims, 100, replace=FALSE))
  {
    #counter <- counter+1
    
    simtrajectory <- sims[,,simnumber] #take simulated trajectory
    
    simtrajectory[1,] <- simtrajectory[1,]*(sin(30*pi/180)/simtrajectory[1,101])
    simtrajectory[2,] <- simtrajectory[2,]*(sin(60*pi/180)/simtrajectory[2,101])
    
    expnumber <- sample(nexp,1)
    exptrajectory <- exps[,,expnumber] #take a random baseline trajectory
    
    exptrajectory[1,] <- exptrajectory[1,]*(sin(30*pi/180)/exptrajectory[1,101])
    exptrajectory[2,] <- exptrajectory[2,]*(sin(60*pi/180)/exptrajectory[2,101])
    
    expAUC <- -polyarea(exptrajectory[1,], exptrajectory[2,])
    
    simAUC <- -polyarea(simtrajectory[1,], simtrajectory[2,])
    
    AUCadded <- expAUC+simAUC
    
    #now get modification of baseline
    diffx <- exptrajectory[1,]-seq(from=0, to=0.5, length.out = 101)
    diffy <- exptrajectory[2,]-seq(from=0, to=sind(60), length.out = 101)
    #recalculate AUCs
    
    newx <- simtraj[1,] + diffx
    newy <- simtraj[2,] + diffy
    
    AUCmodcoord <- -polyarea(newx, newy)
    
    AUCout <- rbind(AUCout, c(simnumber, expnumber, expAUC, simAUC, AUCadded, AUCmodcoord))
  }
  return(as.data.frame(AUCout))
}
