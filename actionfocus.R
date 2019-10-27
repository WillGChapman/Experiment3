actionfocus <- function(onewalk, decbound = 40, gain = 4, model4 = TRUE, suppresscount=TRUE)
{
  nt <- length(onewalk)
  
  b <- c(-decbound,decbound)#c(-1,1)
  
  v <- 0.0076 #movement velocity (metres/sample) - taken from mean of 
  w <- 0.3    #width between targets (metres)
  ht <- 0.2 #height of targets (metres)
  
  #set terminating locations (when track in rage of this - stop!)
  x1 <- c(-w/2, ht)
  x2 <- c(w/2, ht)
  
  #just do model 4 for now
  
  listoftraj <- list()
  
  actionfocus <- c(0,0)
  positionatboundary <- c(x=0,y=0)
  #for the walk
  
  #focus position at start
  focus <- c(0,0)
  
  #effector position at start
  efpos <- matrix(c(0,0), ncol=2)  
  
  #reset decision flag and time count 
  dec <- 0
  t <- 0
  zdec <- numeric()
  zmod <- numeric()
  
  deccount <- numeric()
  
  while (dec==0)
  {
    #print(positionatboundary)
    #x and y are coordinates
    t <- t+1 #increment timestep
    #get current efpos
    efposnow <- efpos[t,]
    
    #model 3 fixation rule
    
    #get decision variable
    z <- onewalk[t]
    zdec[t] <- z

    #if z is over boundary
    if(z>b[2]) if((positionatboundary==0)[1]) positionatboundary <- c(x=efposnow[1], y=efposnow[2])
    
    #model 4 bias on decision variable
    z <- z + gain*decbound*((Norm(efposnow-x1)-Norm(efposnow-x2))/(Norm(efposnow-x1)+Norm(efposnow-x2)))
    
    ##
    
    zmod[t] <- z
    
    focus <- x1*(z<b[1]) + (x2*abs(b[1]-z)/abs(2*b[1]) + x1*abs(b[2]-z)/abs(2*b[2]))*(z>=b[1] & z<=b[2]) + x2*(z>b[2]) 
    
    actionfocus <- rbind(actionfocus, focus)
    
    #focus is now either on a target, or somewhere between
    #now within each timestep the focus moves at v towards focus
    
    #get one v from old position to focus
    movedir <- (focus-efposnow)/Norm(focus-efposnow) #scaled direction of movement
    deltaXY <- v*movedir #movement by 1*v along direction
    
    
    #add noex effector location to efpos[t+1]
    efpos <- rbind(efpos,efposnow + deltaXY) #change to effector position along movedir direction by v
    
    #has effector reached target? if so terminate while loop
    deccount[t] <- 0 + 1*(Norm(efpos[t+1,]-x1)<v/2) + 2*(Norm(efpos[t+1,]-x2)<v/2)
    dec <- deccount[t]
    if (t==nt) dec=3
    
    xflip <- 1
    if (dec==1) xflip <- -1
    
    trialresult <- list(efpos, deccount)
    
    efpossplinex <- spline(seq(from=0, to=100, length.out = t+1), efpos[,1], n=101)$y
    efposspliney <- spline(seq(from=0, to=100, length.out = t+1), efpos[,2], n=101)$y
    
    efposspline <- rbind(x=efpossplinex, y=efposspliney)
    
    efposspline[1,] <- efposspline[1,]*(0.15/efposspline[1,101])#sin(30*pi/180)
    efposspline[2,] <- efposspline[2,]*(0.20/efposspline[2,101]) #sin(60*pi/180)
    
    #get AUC of efpos
    #recify termination coodinates to (15,20)
    #measure AUC from simulated trajectory

    timetobound <- which(cumsum(onewalk>decbound)==1)[1]
    
    listoftraj[["AUC"]] <- -polyarea(efposspline[1,], efposspline[2,])
    listoftraj[["effectorpos"]] <- rbind(x=xflip*efposspline[1,], y=efposspline[2,])
    listoftraj[["targetreached"]] <- length(deccount)
    listoftraj[["decision"]] <- dec
    listoftraj[["actionfocus"]] <- na.omit(actionfocus)
    listoftraj[["decision"]] <- dec
    listoftraj[["targetreached"]] <- t
    listoftraj[["boundreached"]] <- timetobound
    listoftraj[["positionatboundary"]] <- positionatboundary
    listoftraj[["zdec"]] <- zdec[1:timetobound]
    listoftraj[["zmod"]] <- zmod
  }
  return(listoftraj)
}

