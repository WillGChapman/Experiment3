trajectories <- function(walks)
{
  ntr <- dim(walks)[1]
  nt <- dim(walks)[2]
  
  v <- 0.05 #movement velocity
  w <- 1    #width between targets
  ht <- cosd(30) #height of targets
  
  #set terminating locations (when track in rage of this - stop!)
  x1 <- c(-w/2, ht)
  x2 <- c(w/2, ht)
  
  
  
}