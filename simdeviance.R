# script for fun thing with simulated deviance.

#Pseudo: - Simulate N trajectories
# - Compute N AUCs
# - Fit kernel density to N simulated AUCs: 
#   dens<-density(AUC,n=1024,from=min,to=max,na.rm=TRUE)
#   x<-fixDens$x
#   y<-fixDens$y
#   - Interpolate the (x,y) density at your observed AUCs:
#     approx(observed,x,y,method="linear")
#   - Compute deviance: log, sum, times -2

#load some simulations

simeasy <- readMat("Simulations/auceasymd4.mat")[1]
