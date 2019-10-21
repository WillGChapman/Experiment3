
source("actionfocus.R")
source("modifyAUC.R")
#action focus plot

#get ranwalks

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
     ylim=c(-6, 15),
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

# model 4 algo

actfocusBR <- actionfocus(walks[biggestrightAUC,])
actfocusMR <- actionfocus(walks[medianrightAUC,])
actfocusBW <- actionfocus(walks[biggestwrongAUC,])
