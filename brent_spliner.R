# let's make a trajectory generator based on splining some trials with incorrect guesses and
# variability in starting trajectory.

# ooh maybe a model where incorrect guesses are wider? two processes into launch direction:
#  execution noise
#  some kind of concurrent modification. (interrupt to the reach generation process)

plot(NA, type='l', xlim=c(-10, 10), ylim=c(0,10),col='blue')

i <- 1000
while(i>0)
{
  i <- i-1
  inflection_distance <- rnorm(1,5,0.5)
  
  directionguess = 2*(rbinom(n = 1, size = 1, prob = 0.5)-0.5)
  
  trajectory <- t(array(data=rep(c(0,inflection_distance,10),2), dim = c(3,2)))
  
  execution_noise <- rnorm(n = 1, mean = 0, sd = 5)*pi/180
  
  #rotate inflection_point by execution noise and put in guessed direction too
  trajectory[1,2] <- (cos(execution_noise)*inflection_distance-sin(execution_noise)*inflection_distance)*directionguess
  trajectory[2,2] <- sin(execution_noise)*inflection_distance+cos(execution_noise)*inflection_distance
  
  #plot smoothed trajectory
  lines(spline(trajectory[1,], n=101)$y, spline(trajectory[2,], n=101)$y, col=alpha('black', alpha=0.05))
}