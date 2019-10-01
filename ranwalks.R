#generate random walks

# function line add vars - n_time, si, n_trials, drift_rate
ranwalks <- function(n_trials, n_time_samples, drift_rate, noise_sd)
{
  nt <- n_time_samples
  ntr <- n_trials
  si <- c(noise_sd, noise_sd)
  mu <- c(drift_rate, drift_rate)/3
  
  #preallocate storage
  target <- vector(mode="numeric")
  z <- array(data=NA, dim=c(ntr,nt))
  
  for (itr in 1:ntr) 
  {
    if(rem(itr,10)==0) {disp(num2str(itr, fmt=0))}
    
    target <- c(target, 2)
    
    s <- mu[target[itr]] + si[target[itr]]*randn(nt,1)
    # 
    
    z[itr,] <- cumsum(s)
  }
  return(z)
}
