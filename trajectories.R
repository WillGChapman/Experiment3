trajectories <- function(walks, decbound = 40, model4 = TRUE)
{
  ntr <- dim(walks)[1]
  nt <- dim(walks)[2]
  
  b <- c(-decbound,decbound)#c(-1,1)
  
  v <- 0.05 #movement velocity
  w <- 1    #width between targets
  ht <- cosd(30) #height of targets (so absolute distance to target is 1)
  
  #set terminating locations (when track in rage of this - stop!)
  x1 <- c(-w/2, ht)
  x2 <- c(w/2, ht)
  
  #just do model 4 for now
    
  listoftraj <- list()
  
  #for each walk
  for (trnum in 1:ntr)
  {
    if(rem(trnum,10)==0) {disp(num2str(trnum, fmt=0))}
    #focus position at start
    focus <- c(0,0)
    
    #effector position at start
    efpos <- matrix(c(0,0), ncol=2)  

    #reset decision flag and time count    
    dec <- 0
    t <- 0
    
    deccount <- numeric()
    
    while (dec==0)
    {
      #x and y are coordinates
      t <- t+1 #increment timestep
      #get current efpos
      efposnow <- efpos[t,]
      
      #model 3 fixation rule
      
      #get decision variable
      z <- walks[trnum,t]
      
      #model 4 bias on decision variable
      if (model4==TRUE) z = z + 4*decbound*((Norm(efposnow-x1)-Norm(efposnow-x2))/(Norm(efposnow-x1)+Norm(efposnow-x2)))
      
      ##
      
      focus <- x1*(z<b[1]) + (x2*abs(b[1]-z)/abs(2*b[1]) + x1*abs(b[2]-z)/abs(2*b[2]))*(z>=b[1] & z<=b[2]) + x2*(z>b[2]) 
      
      #focus is now either on a target, or somewhere between
      #now within each timestep the focus moves at v towards focus
      
      #get one v from old position to focus
      movedir <- (focus-efposnow)/Norm(focus-efposnow) #scaled direction of movement
      deltaXY <- v*movedir #movement by 1*v along direction
      efpos <- rbind(efpos,efposnow + deltaXY) #change to effector position along movedir direction by v
      
      #has effector reached target? if so terminate while loop
      deccount[t] <- 0 + 1*(Norm(efpos[t+1,]-x1)<v/2) + 2*(Norm(efpos[t+1,]-x2)<v/2)
      dec <- deccount[t]
      if (t==nt) dec=3
    }
    trialresult <- list(efpos, deccount)
    
    efpossplinex <- spline(seq(from=0, to=100, length.out = t+1), efpos[,1], n=101)$y
    efposspliney <- spline(seq(from=0, to=100, length.out = t+1), efpos[,2], n=101)$y
    
    efposspline <- rbind(x=efpossplinex, y=efposspliney)
    
    efposspline[1,] <- efposspline[1,]*(sin(30*pi/180)/efposspline[1,101])
    efposspline[2,] <- efposspline[2,]*(sin(60*pi/180)/efposspline[2,101])

    #get AUC of efpos
    #recify termination coodinates to (0.5, 0.8660)
    #measure AUC from simulated trajectory
    listoftraj[["AUC"]][trnum] <- -polyarea(efposspline[1,], efposspline[2,])
    listoftraj[["effectorpos"]][[trnum]] <- efposspline
    listoftraj[["targetreached"]][[trnum]] <- length(deccount)
    listoftraj[["decision"]][[trnum]] <- dec
    
  }
  return(listoftraj)
}
