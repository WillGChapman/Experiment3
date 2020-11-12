#2 panel plot for different values of drift while holding other values steady

#reset plotting area
dev.off()

library(wesanderson)
pal <- wes_palette("FantasticFox1", 25, type = "continuous")

# get drift values to simulate

driftsequence <- c(0.025,0.25,0.5, 1, 1.5, 2)

# save data for plotting rather than plot at the time.

# set up list element for storage
datalist <- list(AUC = numeric(), 
                 XY = array(data = NA, dim = c(2,101)),
                 paramAUC = numeric(2),
                 numsims = numeric(),
                 goodsims = numeric())

# recursively create list
datatochart <- list()

for (i in 1:length(driftsequence))
{
  datatochart[[i]] <- datalist
}


for (i in 1:length(driftsequence))
{
  drift <- driftsequence[i]
  
  walks <- ranwalks(n_trials = 10000, n_time_samples = 500, drift_rate = drift, noise_sd = noise)
  
  trajs <- trajectories(walks, decbound = 7, movementvelocity = 0.0078)
  
  goodsim <- which(trajs$decision==2)
  
  datatochart[[i]]$numsims <- length(goodsim)
  datatochart[[i]]$goodsims <- goodsim
  datatochart[[i]]$AUC <- trajs[["AUC"]][goodsim]
  
  datatochart[[i]]$paramAUC[1] <- mean(trajs[["AUC"]][goodsim])
  datatochart[[i]]$paramAUC[1] <- sd(trajs[["AUC"]][goodsim])
  
  simXYs <- trajs[["effectorpos"]][which(trajs$decision==2)]
  
  datatochart[[i]]$XY <- apply(array(unlist(simXYs), dim = c(2,101,length(goodsim))), c(1,2), mean)
  
}

pal <- wes_palette("FantasticFox1", length(driftsequence), type = "continuous")

kdebw <- 0.00025

par(mfrow=c(1,2))

densAUC <- density(datatochart[[length(driftsequence)]]$AUC, bw = kdebw)

plot(densAUC, 
     main = "Distribution of AUC as drift increases",
     ylab = "Proportion of simulated trials",
     xlab = "AUC",
     yaxt = 'n',
     type = 'l',
     xlim = c(-0.001, 0.02))
polygon(x=densAUC$x, y=densAUC$y, col=scales::alpha(colour = pal[length(driftsequence)], alpha = 0.01), border=pal[length(driftsequence)])

axis(2, at=pretty(densAUC$y), labels = percent(pretty(densAUC$y)/1000))

gradientLegend(driftsequence, pal, pos=c(0.75, 0.5, 0.8, 0.9),n.seg = driftsequence , cex=1)

for (i in length(driftsequence):1)
{
  polygon(density(datatochart[[i]]$AUC, bw = kdebw), col=scales::alpha(colour = pal[i], alpha = 0.01), border=pal[i])
}

fig_label("a)", cex = 2)

# plot(density(datatochart[[25]]$AUC, bw = 0.0001), main = "Distribution of AUC as boundary increases")

plot(datatochart[[1]]$XY[1,], datatochart[[1]]$XY[2,],
     type='l', 
     lwd = 1.5,
     col=scales::alpha(colour = pal[1], alpha = 0.9),
     main = "Mean Trajectory",
     ylab = "y-Coordinate",
     xlab = "x-Coordinate")

for (i in 2:length(driftsequence))
{
  lines(x=datatochart[[i]]$XY[1,], y=datatochart[[1]]$XY[2,],
        col=scales::alpha(colour = pal[i], alpha = 0.9),
        lwd=1.5)
}

fig_label("b)", cex = 2)