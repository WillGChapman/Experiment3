

pal <- wes_palette("FantasticFox1", 9, type = "continuous")

datatochart <- list()


# set up list element for storage
datalist <- list(AUC = numeric(), 
                 XY = array(data = NA, dim = c(2,101)),
                 paramAUC = numeric(2),
                 numsims = numeric(),
                 goodsims = numeric())


for (i in 1:9)
{
  datatochart[[i]] <- datalist
}

# noise: 5 to 20
noise <- c(5,5,5,10,10,10,20,20,20)
# drifts value: 2 to 10
drifts <- c(0.25,2.5,5,0.25,2.5,5,0.25,2.5,5)

for (i in 1:9)
{
  
  nse <- noise[i]
  drft <- drifts[i]
  
  trajs <- trajectories(ranwalks(n_trials = 10000, 
                                 n_time_samples = 500, 
                                 drift_rate = drft, 
                                 noise_sd = nse),
                        decbound = 7, gain = 4)
  
  goodsim <- which(trajs$decision==2)
  
  datatochart[[i]]$numsims <- length(goodsim)
  datatochart[[i]]$goodsims <- goodsim
  datatochart[[i]]$AUC <- trajs[["AUC"]][goodsim]
  
  datatochart[[i]]$paramAUC[1] <- mean(trajs[["AUC"]][goodsim])
  datatochart[[i]]$paramAUC[1] <- sd(trajs[["AUC"]][goodsim])
  
  simXYs <- trajs[["effectorpos"]][which(trajs$decision==2)]
  
  datatochart[[i]]$XY <- apply(array(unlist(simXYs), dim = c(2,101,length(goodsim))), c(1,2), mean)
  
}

par(mfrow=c(3,4),
    oma=c(1,1,1,1),
    mar=c(1,1,1,1))


#for (i in 1:9)
#{
#  plot(datatochart[[i]]$XY[1,], datatochart[[i]]$XY[2,],
#       type='l', 
#       lwd = 4,
#       col=alpha(colour = pal[i], alpha = 1),
#       main = sprintf("Mean Trajectory (%d) \n decb=%d drift=%f", datatochart[[i]]$numsims, bnd[i], drifts[i]),
#       ylab = "y-Coordinate",
#       xlab = "x-Coordinate")
#}

for (i in 1:9)
{
  plot(density(datatochart[[i]]$AUC),
       type='l', 
       lwd = 1.5,
       col=scales::alpha(colour = pal[i], alpha = 0.6),
       main = " ", # "hello", #sprintf("AUC distribution decb=%d drift=%0.2f", bnd[i], drifts[i]),
       ylab = " ", #"N simulations",
       yaxt = "n",
       xlab = " ", #"AUC",
       xlim = c(-0.005, 0.025), ylim=c(0,350))
  #axis(2, at=pretty(c(0,1000)), labels=percent(pretty(c(0,1000))/1000, accuracy=1), las=2)
  polygon(density(datatochart[[i]]$AUC), col=scales::alpha(colour = pal[i], alpha = 0.1), border=pal[i])
  if (mod(i,3)==0)
  {
    plot(datatochart[[i-2]]$XY[1,], datatochart[[i-2]]$XY[2,],
         type='l', 
         lwd = 2,
         col=scales::alpha(colour = pal[i-2], alpha = 1),
         main = " ", #sprintf("Mean Trajectory (%d)", datatochart[[i]]$numsims),
         ylab = " ", #"y-Coordinate",
         xlab = " ") # x-Coordinate")
    for (j in (i-1):i)
    {
      lines(datatochart[[j]]$XY[1,], datatochart[[j]]$XY[2,],
            type='l', 
            lwd = 2,
            col=scales::alpha(colour = pal[j], alpha = 1))
    }
  }
}