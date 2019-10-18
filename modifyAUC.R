#script for getting modiefied AUC.

#required output: list of sampled original AUC, sim + simply added AUC, sim + trajectory combined AUC.
modifyAUC <- function(explines, simlines)
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
  
  AUCexp <- numeric(length=nexp)
  
  for (expnumber in 1:nexp)
  {
    exptrajectory <- exps[,,expnumber] #take a baseline trajectory
    
    exptrajectory[1,] <- exptrajectory[1,]*(sin(30*pi/180)/exptrajectory[1,101])
    exptrajectory[2,] <- exptrajectory[2,]*(sin(60*pi/180)/exptrajectory[2,101])
    
    AUCexp[expnumber] <- -polyarea(exptrajectory[1,], exptrajectory[2,])
  }
  
  
  AUCout <- numeric(length=nsims)
  AUCsim <- numeric(length=nsims)
  
  for (simnumber in 1:nsims)
  {
    simtrajectory <- sims[,,simnumber] #take simulated trajectory
    
    simtrajectory[1,] <- simtrajectory[1,]*(sin(30*pi/180)/simtrajectory[1,101])
    simtrajectory[2,] <- simtrajectory[2,]*(sin(60*pi/180)/simtrajectory[2,101])
    
    simAUC <- -polyarea(simtrajectory[1,], simtrajectory[2,])
    expAUC <- AUCexp[sample(nexp,1)]
    
    AUCadded <- expAUC+simAUC
    
    AUCout[simnumber] <- AUCadded
    AUCsim[simnumber] <- simAUC
  }
  
  modifyAUCoutput <- list()
  
  modifyAUCoutput$AUCout <- AUCout
  modifyAUCoutput$AUCsim <- AUCsim
  modifyAUCoutput$AUCexp <- AUCexp
  
  return(modifyAUCoutput)
}
