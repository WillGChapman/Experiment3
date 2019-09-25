trajectories <- function(walks)
{
  ntr <- dim(walks)[1]
  nt <- dim(walks)[2]
  
  b <- 40#c(-1,1)
  
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
    print(trnum)
    #focus position at start
    focus <- c(0,0)
    
    #effector position at start
    efpos <- matrix(c(0,0), ncol=2)  

    #reset decision flag and time count    
    dec <- 0
    t <- 0
    
    trajs <- list(NULL)
    
    while (dec==0)
    {
      #x and y are coordinates
      t <- t+1 #increment timestep
      print(t)
      #get current efpos
      efposnow <- efpos[t,]
      
      #model 3 fixation rule
      
      #get decision variable
      z <- walks[trnum,t]
      print(zdec)
      
      
      
      
      focus <- x1*(z<b[1]) + (x2*abs(b[1]-z)/abs(2*b[1]) + x1*abs(b[2]-z)/abs(2*b[2]))*(z>=b[1] & z<=b[2]) + x2*(z>b[2]) 
      
      
      #if zdec is over or under boundary set focus at x1 or x2
      if (zdec>b) focus = x1
      if (zdec<(-b)) focus = x2
      
      #if zdec is between boundaries, weighted sum of diff between z and b
      
      if (zdec<b || zdec>-b)
      {
        #model4 bias z in terms of closeness to targets - comment out next two lines to return to model 3
        #gain <- 4*b
        #zdec <- zdec+gain*(Norm(efpos-x1)-Norm(efpos-x2))/(Norm(efpos-x1)+Norm(efpos-x2))
        
        focus[1] = abs(b-zdec)*x1[1] +abs(b+zdec)*x2[1] #set focus in x to weighted sum of decision vs width
        focus[2] = ht
      }
      
      #focus is now either on a target, or somewhere between
      #now within each timestep the focus moves at v towards focus
      
      #get one v from old position to focus
      movedir <- (focus-efposnow)/Norm(focus-efposnow) #scaled direction of movement
      deltaXY <- v*movedir #movement by 1*v along direction
      efpos <- rbind(efpos,efposnow + deltaXY) #change to effector position along movedir direction by v
      
      #has effector reached target if so terminate while loop
      if (Norm(focus-efpos)<v/2) dec=1
      if (t==500) dec=2
    }
    listoftraj[[trnum]] <- efpos
  }
  return(listoftraj)
}

