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

simhard<- readMat("Simulations/auchardmd4SIM.mat")

simhard <- simhard$auc

dens <- density(simhard$auc, n=1024, from=0, to=max(simhard$auc), na.rm=T) #density of simulated

x <- dens$x
y <- dens$y


#get a list of relevant AUCs
exphard <- resultinfo3[resultinfo3$Difficulty=="Hrd" &
                         resultinfo3$pname==3,"AUC"]

#use the experimental AUCs and see where tey
interpolated <- approx(x, y, exphard, method='linear')

#problem is that a lot of experimental AUCs are negative (tackle that later)

#to get the deviance of the simulation from the experimental data

modely <- interpolated$y

