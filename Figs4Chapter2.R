#Charts for simulation narrative document.

#get figures to tell the story

#ok figure 1: two walks, with a trajectory panel.

#get trajectories with wrong eventual decision

trajs <- trajectories(walks,
                      decbound = 5,
                      model4 = TRUE)

#wrong decision index
wdIndex <- which(trajs$decision==1)
cdIndex <- which(trajs$decision==2)

wrongtrajs

#plot some trajectories (wrong'uns only)
plot(trajs$effectorpos[[wdIndex[1]]][1,], trajs$effectorpos[[wdIndex[1]]][2,],
     type='l',
     xlim=c(-15,15),
     col=alpha(colour = 'black', alpha = 0.25))

numwrongs <- length(wdIndex)

for (i in wdIndex[2:numwrongs])
{
  lines(trajs$effectorpos[[i]][1,], trajs$effectorpos[[i]][2,], type='l',
        col=alpha(colour = 'black', alpha = 0.25))
}

biggestwrongAUC <- wdIndex[which.max(trajs$AUC[wdIndex])]
biggestrightAUC <- cdIndex[which.max(trajs$AUC[cdIndex])]

medianrightAUC <- which(trajs$AUC==median(trajs$AUC[cdIndex]))

#plot showing three trajectories
plot(trajs$effectorpos[[biggestwrongAUC]][1,], trajs$effectorpos[[biggestwrongAUC]][2,],
     type='l',
     xlim=c(-15,15),
     col=alpha(colour = 'black', alpha = 1),
     lty='dashed',
     xlab='x-position',
     ylab='y-position')
lines(trajs$effectorpos[[medianrightAUC]][1,], trajs$effectorpos[[medianrightAUC]][2,],
      type='l',
      xlim=c(-15,15),
      col=alpha(colour = 'black', alpha = 1),
      lty='solid')
lines(trajs$effectorpos[[biggestrightAUC]][1,], trajs$effectorpos[[biggestrightAUC]][2,],
      type='l',
      xlim=c(-15,15),
      col=alpha(colour = 'black', alpha = 1),
      lty='dotdash')
legend(x = 'bottomleft',
      lty = c('solid', 'dashed', 'dotdash'),
      legend = c('fast correct sumulated reach',
                 'incorrect simulated reach',
                 'slow correct simulated reach'),
      inset = 0.05)

decTimewrong <- trajs$targetreached[biggestwrongAUC]
decTimeright <- trajs$targetreached[biggestrightAUC]
decTimemedright <- trajs$targetreached[medianrightAUC]

#plot showing three walks
plot(walks[biggestwrongAUC,1:70],
     type='l',
     ylim=c(-6, 35),
     lty='dashed',
     ylab='decision variable',
     xlab='time')
lines(walks[medianrightAUC,1:70], lty='solid')
lines(walks[biggestrightAUC,1:70], lty='dotdash')
abline(h=5)
abline(h=-5)
points(decTimewrong, walks[biggestwrongAUC, decTimewrong])
points(decTimeright, walks[biggestrightAUC, decTimeright])
points(decTimemedright, walks[medianrightAUC, decTimemedright])

#auc distribution plot

plot(trajs$effectorpos[[cdIndex[1]]][1,], trajs$effectorpos[[cdIndex[1]]][2,],
     type='l',
     xlim=c(-15,15),
     col=alpha(colour = 'black', alpha = 0.25),
     xlab='x-position',
     ylab='y-position')

numcorrect <- length(cdIndex)

for (i in sample(cdIndex[2:numcorrect], 50))
{
  lines(trajs$effectorpos[[i]][1,], trajs$effectorpos[[i]][2,], type='l',
        col=alpha(colour = 'black', alpha = 0.25))
}
for (i in sample(wdIndex, 5))
{
  lines(trajs$effectorpos[[i]][1,], trajs$effectorpos[[i]][2,], type='l',
        col=alpha(colour = 'black', alpha = 0.25))
}


#density of AUC (correct reach simulations)
plot(density(trajs$AUC[cdIndex]),
     main = sprintf("AUC Distribution from Simulation \nwith a decision boundary set to %d", b),
     xlab = "AUC (cm^2)")
lines(density(trajs$AUC[wdIndex]), lty='dashed')

b <- 5
trajs2 <- trajectories(walks, 
                      decbound = b,
                      model4 = TRUE)
wdIndex2 <- which(trajs2$decision==1)
cdIndex2 <- which(trajs2$decision==2)

plot(density(trajs2$AUC[cdIndex2]),
     main = sprintf("AUC Distribution from Simulation \nwith a decision boundary set to %d", b),
     xlab = "AUC (cm^2)")
lines(density(trajs2$AUC[wdIndex2]), lty='dashed')

#now for the plots showing baseline and shifted AUCs

simlines <- simplify2array(trajs2$effectorpos[cdIndex2])

modifiedAUCs <- modifyAUC(explines, simlines)

observedAUC <- na.omit(modifiedAUCs$AUCexp)
simulatedAUC <- na.omit(modifiedAUCs$AUCsim)
modifiedAUC <- na.omit(modifiedAUCs$AUCout)

plot(density(observedAUC),
     xlim=c(-0.2, 0.3),
     ylim=c(0,17),
     lty='dotdash')
lines(density(simulatedAUC),
      lty='dashed')
lines(density(modifiedAUC))
legend(x = 'topleft',
       lty = c('solid', 'dashed', 'dotdash'),legend = c('modified AUC', 'simulated AUC', 'observed AUC'),inset = 0.1)




#get some walks