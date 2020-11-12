boundarysequence <- signif(10^c(seq(from=0, to=2, by=0.25)), digits = 3)

pal <- wes_palette("FantasticFox1", 25, type = "continuous")

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
polygon(x=densAUC$x, y=densAUC$y, col=scales::alpha(colour = pal[1], alpha = 0.01), border=pal[1])

gradientLegend(boundarysequence, pal,
               pos=c(0.75, 0.5, 0.8, 0.9), n.seg = c(1, 50, 100), cex=1)

axis(2, at=pretty(densAUC$y), labels = percent(pretty(densAUC$y)/1000))

for (i in 1:length(boundarysequence))
{
  polygon(density(datatochart[[i]]$AUC, bw = kdebw), col=scales::alpha(colour = pal[i], alpha = 0.01), border=pal[i])
}

# plot(density(datatochart[[25]]$AUC, bw = 0.0001), main = "Distribution of AUC as boundary increases")

plot(datatochart[[1]]$XY[1,], datatochart[[1]]$XY[2,],
     type='l', 
     lwd = 1.5,
     col=scales::alpha(colour = pal[1], alpha = 0.6),
     main = "Mean Trajectory",
     ylab = "y-Coordinate",
     xlab = "x-Coordinate")

for (i in 2:length(boundarysequence))
{
  lines(x=datatochart[[i]]$XY[1,], y=datatochart[[1]]$XY[2,],
        col=scales::alpha(colour = pal[i], alpha = 0.6),
        lwd=1.5)
}