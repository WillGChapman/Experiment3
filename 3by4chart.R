#panel plots for 3x3+1 plots

# four distributions in a panel plot.
pal <- wes_palette("FantasticFox1", 3, type = "continuous")

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

par(mfrow=c(3,4))
for (i in 1:9)
{
  plot(datatochart[[i]]$XY[1,], datatochart[[i]]$XY[2,],
       type='l', 
       lwd = 4,
       col=scales::alpha(colour = pal[i], alpha = 1),
       main = sprintf("Mean Trajectory (%d) \n decb=%d drift=%0.2f", datatochart[[i]]$numsims, bnd[i], drifts[i]/3),
       ylab = "y-Coordinate",
       xlab = "x-Coordinate")
}

for (i in 1:9)
{
  plot(density(datatochart[[i]]$AUC),
       type='l', 
       lwd = 1.5,
       col=scales::alpha(colour = pal[i], alpha = 0.6),
       main = sprintf("AUC distribution \n decb=%d drift=%0.2f", bnd[i], drifts[i]/3),
       ylab = "Proportion of Simulations",
       yaxt = "n",
       xlab = "AUC",
       xlim = c(0, 0.015), ylim=c(0,800))
  axis(2, at=pretty(c(0,800)), labels=percent(pretty(c(0,800))/1000, accuracy=1), las=2)
  polygon(density(datatochart[[i]]$AUC), col=scales::alpha(colour = pal[i], alpha = 0.1), border=pal[i])
  
  if (mod(i,3)==0)
  {
    plot(datatochart[[i-2]]$XY[1,], datatochart[[i-2]]$XY[2,],
         type='l', 
         lwd = 2,
         col=scales::alpha(colour = pal[i-2], alpha = 1),
         main = sprintf("Mean Trajectory (%d) \n decb=%d", datatochart[[i]]$numsims, bnd[i], drifts[i]/3),
         ylab = "y-Coordinate",
         xlab = "x-Coordinate")
    for (j in (i-1):i)
    {
      lines(datatochart[[j]]$XY[1,], datatochart[[j]]$XY[2,],
            type='l', 
            lwd = 2,
            col=scales::alpha(colour = pal[j], alpha = 1))
    }
  }
}