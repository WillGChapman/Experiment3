#2 panel plot for different values of gain while holding other values steady
library(wesanderson)
pal <- wes_palette("FantasticFox1", 25, type = "continuous")

## RUN SCRIPT FROM HERE

#reset plotting area
dev.off()



# make gain values to simulate

gainsequence <- seq(from=0, to=10)

#set parameters used (in this case it is just "noise")

noise <- 1

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
  gain <- gainsequence[i]
  
  walks <- ranwalks(n_trials = 10000, n_time_samples = 500, drift_rate = 1, noise_sd = noise)
  
  trajs <- trajectories(walks, decbound = 7, gain = gain, movementvelocity = 0.0078)
  
  goodsim <- which(trajs$decision==2)
  
  datatochart[[i]]$numsims <- length(goodsim)
  datatochart[[i]]$goodsims <- goodsim
  datatochart[[i]]$AUC <- trajs[["AUC"]][goodsim]
  
  datatochart[[i]]$paramAUC[1] <- mean(trajs[["AUC"]][goodsim])
  datatochart[[i]]$paramAUC[1] <- sd(trajs[["AUC"]][goodsim])
  
  simXYs <- trajs[["effectorpos"]][which(trajs$decision==2)]
  
  datatochart[[i]]$XY <- apply(array(unlist(simXYs), dim = c(2,101,length(goodsim))), c(1,2), mean)
  
}

pal <- wes_palette("FantasticFox1", length(gainsequence), type = "continuous")

kdebw <- 0.00025

par(mfrow=c(1,2))

densAUC <- density(datatochart[[length(gainsequence)]]$AUC, bw = kdebw)

plot(densAUC, 
     main = "Distribution of AUC as gain increases",
     ylab = "Proportion of simulated trials",
     xlab = "AUC",
     yaxt = 'n',
     type = 'l',
     xlim = c(-0.001, 0.02))
polygon(x=densAUC$x, y=densAUC$y, col=scales::alpha(colour = pal[length(gainsequence)], alpha = 0.01), border=pal[length(gainsequence)])

axis(2, at=pretty(densAUC$y), labels = percent(pretty(densAUC$y)/1000))

gradientLegend(gainsequence, pal, pos=c(0.75, 0.5, 0.8, 0.9),n.seg = gainsequence , cex=1)

for (i in length(gainsequence):1)
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

for (i in 2:length(gainsequence))
{
  lines(x=datatochart[[i]]$XY[1,], y=datatochart[[1]]$XY[2,],
        col=scales::alpha(colour = pal[i], alpha = 0.9),
        lwd=1.5)
}

fig_label("b)", cex = 2)