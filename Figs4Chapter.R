#Charts for simulation narrative document.

#get figures to tell the story

#ok figure 1: two walks, with a trajectory panel.

#get trajectories with wrong eventual decision

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
     lty='dashed')
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


decTimewrong <- trajs$targetreached[biggestwrongAUC]
decTimeright <- trajs$targetreached[biggestrightAUC]
decTimemedright <- trajs$targetreached[medianrightAUC]

#plot showing three walks
plot(walks[biggestwrongAUC,1:70],
     type='l',
     ylim=c(-6, 35),
     lty='dashed')
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
     col=alpha(colour = 'black', alpha = 0.25))

numcorrect <- length(cdIndex)

for (i in sample(cdIndex[2:numcorrect], 50))
{
  lines(trajs$effectorpos[[i]][1,], trajs$effectorpos[[i]][2,], type='l',
        col=alpha(colour = 'black', alpha = 0.25))
}

plot(density(trajs$AUC[cdIndex]))

#don't go too much into model 2 or 3. present model 4 as is.


#get some walks