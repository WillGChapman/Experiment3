#let's make a pipeline for testing parameters.

#make sure we have the right functions loaded
source("modifyAUC.R")
source("ranwalks.R")
source("actionfocus.R")
source("trajectoriesRealGeometry.R")
source("figlabel.R")

#get a nice colour pallette
#####
install.packages("wesanderson")
library(wesanderson)
pal <- wes_palette("FantasticFox1", 25, type = "continuous")
#we need a function to take in parameters and output an AUC distribution

#Fig 3 – Stochastic Model of trajectories and AUC distributions
#####

#initial parameter values
decisionboundary <- 7
drift <- 1
noise <- 1

#generate random walks
walks <- ranwalks(n_trials = 10000, n_time_samples = 500, drift_rate = drift, noise_sd = noise)

#container for walks with varying drift
walksvardrift <- array(dim=c(10,10000,500))

trajslist <- list()
driftsequence <- c(0.25, 0.5, .75, 1, 1.5, 2, 2.5, 5, 7.5, 10)

for (i in 1:length(driftsequence))
{
  set.seed(1)
  walksvardrift[i,,] <- ranwalks(n_trials = 10000, n_time_samples = 500, drift_rate = driftsequence[i], noise_sd = 1)

  trajslist[[i]] <- trajectories(walksvardrift[i,,])
  
}

par(mfrow=c(2,5))
pal <- wes_palette("FantasticFox1", 10, type = "continuous")


for (i in 1:length(driftsequence))
{
  goodsim <- which(trajslist[[i]]$decision==2)
  plot(density(trajslist[[i]]$AUC[goodsim]),
       main = sprintf("AUC Density,\nwith drift rate %0.2f", driftsequence[i]),
       xlim = c(0,0.016))
  polygon(density(trajslist[[i]]$AUC[goodsim]), col=alpha(colour = pal[i], alpha = 0.01), border=pal[i])
}

for (i in 1:length(driftsequence))
{
  goodsim <- which(trajslist[[i]]$decision==2)
  
  meantracks <- apply(array(unlist(trajslist[[i]]$effectorpos[goodsim]), dim = c(2,101,744)), c(1,2), mean)
  
  plot(x=meantracks[1,], y=meantracks[2,], type = 'l',
       col=alpha(colour = pal[i], alpha = 0.6),
       lwd=2.5)

}


load("walksplot.RData")

trajs <- trajectories(walks, decbound = 1)


#####
# get boundary values to simulate

boundarysequence <- 1.5^c(seq(from=0, to=12, by=1))

# save data for plotting rather than plot at the time.

# set up list element for storage
datalist <- list(AUC = numeric(), 
                 XY = array(data = NA, dim = c(2,101)),
                 paramAUC = numeric(2),
                 numsims = numeric(),
                 goodsims = numeric())

# recursively create list
datatochart <- list()

for (i in 1:length(boundarysequence))
{
  datatochart[[i]] <- datalist
}


for (i in 1:length(boundarysequence))
{
  decb <- boundarysequence[i]
  
  trajs <- trajectories(walks, decbound = decb, movementvelocity = 0.0078)
  
  goodsim <- which(trajs$decision==2)
  
  datatochart[[i]]$numsims <- length(goodsim)
  datatochart[[i]]$goodsims <- goodsim
  datatochart[[i]]$AUC <- trajs[["AUC"]][goodsim]

  datatochart[[i]]$paramAUC[1] <- mean(trajs[["AUC"]][goodsim])
  datatochart[[i]]$paramAUC[1] <- sd(trajs[["AUC"]][goodsim])
  
  simXYs <- trajs[["effectorpos"]][which(trajs$decision==2)]
  
  datatochart[[i]]$XY <- apply(array(unlist(simXYs), dim = c(2,101,length(goodsim))), c(1,2), mean)
  
}

pal <- wes_palette("FantasticFox1", length(boundarysequence), type = "continuous")

kdebw <- 0.00025

par(mfrow=c(1,2))

densAUC <- density(datatochart[[1]]$AUC, bw = kdebw)

plot(densAUC, 
     main = "Distribution of AUC as boundary increases",
     ylab = "Proportion of simulated trials",
     xlab = "AUC",
     yaxt = 'n',
     type = 'l')
polygon(x=densAUC$x, y=densAUC$y, col=alpha(colour = pal[1], alpha = 0.01), border=pal[1])

axis(2, at=pretty(densAUC$y), labels = percent(pretty(densAUC$y)/1000))

for (i in 1:length(boundarysequence))
{
  polygon(density(datatochart[[i]]$AUC, bw = kdebw), col=alpha(colour = pal[i], alpha = 0.01), border=pal[i])
}

# plot(density(datatochart[[25]]$AUC, bw = 0.0001), main = "Distribution of AUC as boundary increases")

plot(datatochart[[1]]$XY[1,], datatochart[[1]]$XY[2,],
     type='l', 
     lwd = 1.5,
     col=alpha(colour = pal[1], alpha = 0.6),
     main = "Mean Trajectory",
     ylab = "y-Coordinate",
     xlab = "x-Coordinate")

for (i in 2:length(boundarysequence))
{
  lines(x=datatochart[[i]]$XY[1,], y=datatochart[[1]]$XY[2,],
        col=alpha(colour = pal[i], alpha = 0.6),
        lwd=1.5)
}
# wtf spikes in KDE

#####
# looks at peaks in AUC dist

plot(trajs$effectorpos[[1]][1,], trajs$effectorpos[[1]][2,], type='l', col=alpha(colour = 'black', alpha = 0.01))
for (i in 1:50000)
{
  lines(trajs$effectorpos[[i]][1,], trajs$effectorpos[[i]][2,], type='l', col=alpha(colour = 'black', alpha = 0.001))
}

head(order(abs(AUCvals$AUC-AUCpeaks[1])))

trajs$effectorpos[[test[1]]]

pal <- wes_palette("FantasticFox1", 5, type = "continuous")

plot(trajs$effectorpos[[test[1]]][1,], trajs$effectorpos[[test[1]]][2,], type='l',
     ylim = c(0,0.2), xlim=c(-0.15, 0.15))

for(j in 1:5)
{
  test <- head(order(abs(AUCvals$AUC-AUCpeaks[j])))
  for(i in 1:5)
    {
      lines(trajs$effectorpos[[test[i]]][1,], trajs$effectorpos[[test[i]]][2,], type='l',
            col=pal[i])
    }
}

####

datalist <- list(AUC = numeric(), 
                 XY = array(data = NA, dim = c(2,101)),
                 paramAUC = numeric(2),
                 numsims = numeric())

# recursively create list
datatochart <- list()

###
velocitysequence <- seq(from = 0.02, to = 0.002, by=-0.002)

for (i in 1:length(velocitysequence))
{
  datatochart[[i]] <- datalist
}


for (i in 1:length(velocitysequence))
{
  decb <- 125
  
  trajs <- trajectories(walks, decbound = decb, movementvelocity = velocitysequence[i])
  
  goodsim <- which(trajs$decision==2)
  
  datatochart[[i]]$numsims <- length(goodsim)
  datatochart[[i]]$indsims <- goodsim
  datatochart[[i]]$AUC <- trajs[["AUC"]][goodsim]
  datatochart[[i]]$goodsims <- goodsim
  
  datatochart[[i]]$paramAUC[1] <- mean(trajs[["AUC"]][goodsim])
  datatochart[[i]]$paramAUC[2] <- sd(trajs[["AUC"]][goodsim])
  
  simXYs <- trajs[["effectorpos"]][which(trajs$decision==2)]
  
  datatochart[[i]]$XY <- apply(array(unlist(simXYs), dim = c(2,101,length(goodsim))), c(1,2), mean)
  
}

pal <- wes_palette("FantasticFox1", length(velocitysequence), type = "continuous")

kdebw <- 0.0001

plot(density(datatochart[[length(velocitysequence)]]$AUC, bw = kdebw),
     main = "Distribution of AUC as velocity dencreases",
     ylim=c(0,500), xlim=c(0,0.015))
polygon(density(datatochart[[1]]$AUC, bw=kdebw), col=alpha(colour = pal[1], alpha = 0.01), border=pal[1])

for (i in 7:length(velocitysequence))
{
  polygon(density(datatochart[[i]]$AUC, bw = kdebw), col=alpha(colour = pal[i], alpha = 0.01), border=pal[i])
}


#####
# get and show peaks and find crossover trajectories
AUCdat <- density(datatochart[[23]]$AUC, bw=0.00005)

plot(AUCdat, type='l')

AUCpeaks <- findpeaks(AUCdat$y)

#top three peak locations
topthree <- AUCpeaks[order(AUCpeaks[,1], decreasing = TRUE)[1:3],2]

top3AUCs <- AUCdat$x[topthree]

#for highest AUCs plot trajectories

plot(x=c(0,0.15),y=c(0,0.2), type='n',
     xlab = "x-coordinate", ylab = "y-coordinate")

linepal <- wes_palette("FantasticFox1", 5, type = "continuous")

for (j in 1:3)
{
  #get indices of 5 AUCs in datatochart that are closest to the peak value
  dataindex <- order(abs(datatochart[[25]]$AUC-top3AUCs[j]))[1:5]
  #dataindex is the indices of "good" simulations that are close to required value
  for(i in 1:5)
  {
    lineplotind <- datatochart[[25]]$goodsims[dataindex[i]]
    
    lines(trajs$effectorpos[[lineplotind]][1,], trajs$effectorpos[[lineplotind]][2,],
          col = linepal[i])
  }
}

#####
## panel plot of boundary changes and gain adjustments.

gainsequence <- seq(from=0, to=12, by=1)

# save data for plotting rather than plot at the time.

# set up list element for storage
datalist <- list(AUC = numeric(), 
                 XY = array(data = NA, dim = c(2,101)),
                 paramAUC = numeric(2),
                 numsims = numeric(),
                 goodsims = numeric())

# recursively create list
datatochart <- list()

for (i in 1:length(gainsequence))
{
  datatochart[[i]] <- datalist
}


for (i in 1:length(gainsequence))
{
  gainval <- gainsequence[i]
  
  trajs <- trajectories(walks, decbound = 7, gain = gainval)
  
  goodsim <- which(trajs$decision==2)
  
  datatochart[[i]]$numsims <- length(goodsim)
  datatochart[[i]]$goodsims <- goodsim
  datatochart[[i]]$AUC <- trajs[["AUC"]][goodsim]
  
  datatochart[[i]]$paramAUC[1] <- mean(trajs[["AUC"]][goodsim])
  datatochart[[i]]$paramAUC[1] <- sd(trajs[["AUC"]][goodsim])
  
  simXYs <- trajs[["effectorpos"]][goodsim]
  
  datatochart[[i]]$XY <- apply(array(unlist(simXYs), dim = c(2,101,length(goodsim))), c(1,2), mean)
}

pal <- wes_palette("FantasticFox1", length(gainsequence), type = "continuous")

kdebw <- 0.0002 #"nrd0"

par(mfrow=c(1,2))

densAUC <- density(datatochart[[length(gainsequence)]]$AUC, bw = kdebw)

plot(densAUC, 
     main = "Distribution of AUC as gain increases",
     ylab = "Proportion of simulated trials",
     xlab = "AUC",
     yaxt = 'n',
     type = 'l',
     xlim = c(0,0.025), ylim = c(0,max(densAUC$y)))
polygon(x=densAUC$x, y=densAUC$y, col=alpha(colour = pal[length(gainsequence)], alpha = 0.01),
        border=pal[length(gainsequence)])

axis(2, at=pretty(densAUC$y), labels = percent(pretty(densAUC$y)/1000, accuracy=1))

for (i in (length(gainsequence)):1)
{
  polygon(density(datatochart[[i]]$AUC, bw = kdebw), col=alpha(colour = pal[i], alpha = 0.01), border=pal[i])
}

# plot(density(datatochart[[25]]$AUC, bw = 0.0001), main = "Distribution of AUC as boundary increases")

plot(datatochart[[length(gainsequence)]]$XY[1,], datatochart[[length(gainsequence)]]$XY[2,],
     type='l', 
     lwd = 1.5,
     col=alpha(colour = pal[length(gainsequence)], alpha = 0.6),
     main = "Mean Trajectory",
     ylab = "y-Coordinate",
     xlab = "x-Coordinate")

for (i in (length(gainsequence)-1):1)
{
  lines(x=datatochart[[i]]$XY[1,], y=datatochart[[i]]$XY[2,],
        col=alpha(colour = pal[i], alpha = 0.6),
        lwd=1.5)
}

#####

# 9 distributions in a panel plot.
pal <- wes_palette("FantasticFox1", 9, type = "continuous")

# boundary: 5 to 20
bnd <- c(5,5,5,10,10,10,20,20,20)
# gain value: 2 to 10
gn <- c(0,5,10,0,5,10,0,5,10)

for (i in 1:9)
{
  gainval <- gn[i]
  decb <- bnd[i]
  
  trajs <- trajectories(walks, decbound = decb, gain = gainval)
  
  goodsim <- which(trajs$decision==2)
  
  datatochart[[i]]$numsims <- length(goodsim)
  datatochart[[i]]$goodsims <- goodsim
  datatochart[[i]]$AUC <- trajs[["AUC"]][goodsim]
  
  datatochart[[i]]$paramAUC[1] <- mean(trajs[["AUC"]][goodsim])
  datatochart[[i]]$paramAUC[1] <- sd(trajs[["AUC"]][goodsim])
  
  simXYs <- trajs[["effectorpos"]][which(trajs$decision==2)]
  
  datatochart[[i]]$XY <- apply(array(unlist(simXYs), dim = c(2,101,length(goodsim))), c(1,2), mean)
  
}

par(mfrow=c(3,3))
for (i in 1:9)
{
plot(datatochart[[i]]$XY[1,], datatochart[[i]]$XY[2,],
     type='l', 
     lwd = 4,
     col=alpha(colour = pal[i], alpha = 1),
     main = sprintf("Mean Trajectory \n decb=%d gain=%d", bnd[i], gn[i]),
     ylab = "y-Coordinate",
     xlab = "x-Coordinate")
}

for (i in 1:9)
{
  plot(density(datatochart[[i]]$AUC),
       type='l', 
       lwd = 1.5,
       col=alpha(colour = pal[i], alpha = 0.6),
       main = sprintf("AUC distribution decb=%d gain=%d", bnd[i], gn[i]),
       ylab = "N simulations",
       yaxt = "n",
       xlab = "AUC",
       xlim = c(0, 0.03), ylim=c(0,325))
  axis(2, at=pretty(c(0,325)), labels=percent(pretty(c(0,325))/1000, accuracy=1))
  polygon(density(datatochart[[i]]$AUC), col=alpha(colour = pal[i], alpha = 0.1), border=pal[i])
}

# i can't remember what cas asked me to do earlier.

# probably to write stuff
#####

# four distributions in a panel plot.
pal <- wes_palette("FantasticFox1", 9, type = "continuous")

datatochart <- list()

for (i in 1:9)
{
  datatochart[[i]] <- datalist
}

# boundary: 5 to 20
bnd <- c(5,5,5,10,10,10,20,20,20)
# gain value: 2 to 10
drifts <- rep(c(1,2,3),3)

for (i in 1:9)
{
  
  decb <- bnd[i]
  drft <- drifts[i]
  
  trajs <- trajectories(ranwalks(n_trials = 10000, 
                                 n_time_samples = 500, 
                                 drift_rate = drft, 
                                 noise_sd = 1),
                        decbound = decb, gain = 4)
  
  goodsim <- which(trajs$decision==2)
  
  datatochart[[i]]$numsims <- length(goodsim)
  datatochart[[i]]$goodsims <- goodsim
  datatochart[[i]]$AUC <- trajs[["AUC"]][goodsim]
  
  datatochart[[i]]$paramAUC[1] <- mean(trajs[["AUC"]][goodsim])
  datatochart[[i]]$paramAUC[1] <- sd(trajs[["AUC"]][goodsim])
  
  simXYs <- trajs[["effectorpos"]][which(trajs$decision==2)]
  
  datatochart[[i]]$XY <- apply(array(unlist(simXYs), dim = c(2,101,length(goodsim))), c(1,2), mean)
  
}

par(mfrow=c(3,3))
for (i in 1:9)
{
  plot(datatochart[[i]]$XY[1,], datatochart[[i]]$XY[2,],
       type='l', 
       lwd = 4,
       col=alpha(colour = pal[i], alpha = 1),
       main = sprintf("Mean Trajectory (%d) \n decb=%d drift=%0.2f", datatochart[[i]]$numsims, bnd[i], drifts[i]/3),
       ylab = "y-Coordinate",
       xlab = "x-Coordinate")
}

for (i in 1:9)
{
  plot(density(datatochart[[i]]$AUC),
       type='l', 
       lwd = 1.5,
       col=alpha(colour = pal[i], alpha = 0.6),
       main = sprintf("AUC distribution \n decb=%d drift=%0.2f", bnd[i], drifts[i]/3),
       ylab = "Proportion of Simulations",
       yaxt = "n",
       xlab = "AUC",
       xlim = c(0, 0.03), ylim=c(0,800))
  axis(2, at=pretty(c(0,800)), labels=percent(pretty(c(0,800))/1000, accuracy=1), las=2)
  polygon(density(datatochart[[i]]$AUC), col=alpha(colour = pal[i], alpha = 0.1), border=pal[i])
}

# talk about what does and doesn't happen with the model.


# boundary: 5 to 20
noise <- c(5,5,5,10,10,10,20,20,20)
# gain value: 2 to 10
drifts <- c(0.25,2.5,5,0.25,2.5,5,0.25,2.5,5)

for (i in 1:9)
{
  
  decb <- bnd[i]
  drft <- drifts[i]
  
  trajs <- trajectories(ranwalks(n_trials = 10000, 
                                 n_time_samples = 500, 
                                 drift_rate = drft, 
                                 noise_sd = 1),
                        decbound = decb, gain = 4)
  
  goodsim <- which(trajs$decision==2)
  
  datatochart[[i]]$numsims <- length(goodsim)
  datatochart[[i]]$goodsims <- goodsim
  datatochart[[i]]$AUC <- trajs[["AUC"]][goodsim]
  
  datatochart[[i]]$paramAUC[1] <- mean(trajs[["AUC"]][goodsim])
  datatochart[[i]]$paramAUC[1] <- sd(trajs[["AUC"]][goodsim])
  
  simXYs <- trajs[["effectorpos"]][which(trajs$decision==2)]
  
  datatochart[[i]]$XY <- apply(array(unlist(simXYs), dim = c(2,101,length(goodsim))), c(1,2), mean)
  
}

par(mfrow=c(3,3))
for (i in 1:9)
{
  plot(datatochart[[i]]$XY[1,], datatochart[[i]]$XY[2,],
       type='l', 
       lwd = 4,
       col=alpha(colour = pal[i], alpha = 1),
       main = sprintf("Mean Trajectory (%d) \n decb=%d drift=%f", datatochart[[i]]$numsims, bnd[i], drifts[i]),
       ylab = "y-Coordinate",
       xlab = "x-Coordinate")
}

for (i in 1:9)
{
  plot(density(datatochart[[i]]$AUC),
       type='l', 
       lwd = 1.5,
       col=alpha(colour = pal[i], alpha = 0.6),
       main = sprintf("AUC distribution decb=%d drift=%0.2f", bnd[i], drifts[i]),
       ylab = "N simulations",
       yaxt = "n",
       xlab = "AUC",
       xlim = c(0, 0.03), ylim=c(0,1300))
  axis(2, at=pretty(c(0,1000)), labels=percent(pretty(c(0,1000))/1000, accuracy=1), las=2)
  polygon(density(datatochart[[i]]$AUC), col=alpha(colour = pal[i], alpha = 0.1), border=pal[i])
}