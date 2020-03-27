
source("actionfocus.R")
source("modifyAUC.R")
source("ranwalks.R")
#action focus plot

#get ranwalks
#walks <- ranwalks(1024,500,1,1,F)



#make trajectories

decisionboundary <- 7

walks <- ranwalks(n_trials = 10000, n_time_samples = 500, drift_rate = 1, noise_sd = 1)

#load("walksplot.RData")

trajs <- trajectories(walks, decbound = decisionboundary)

#get which trajectories get the correct target
wdIndex <- which(trajs$decision==1)
cdIndex <- which(trajs$decision==2)

#get some specific trajectories
biggestwrongAUC <- wdIndex[which.min(trajs$AUC[wdIndex])]
biggestrightAUC <- cdIndex[which.max(trajs$AUC[cdIndex])]
medianrightAUC <- which(trajs$AUC==median(trajs$AUC[cdIndex]))

if(isempty(medianrightAUC)) medianrightAUC <- which(trajs$AUC==median(trajs$AUC[cdIndex[2:length(cdIndex)]]))

#get individual trajectory data for these:

bigright <- actionfocus(onewalk = walks[biggestrightAUC,], decbound = decisionboundary, model4 = TRUE, gain = 4, suppresscount = TRUE)
bigwrong <- actionfocus(onewalk = walks[biggestwrongAUC,], decbound = decisionboundary, model4 = TRUE, gain = 4, suppresscount = TRUE)
medright <- actionfocus(onewalk = walks[medianrightAUC,], decbound = decisionboundary, model4 = TRUE, gain = 4, suppresscount = TRUE)

#plot showing three trajectories

# model 4 algo

actfocusBR <- bigright$actionfocus
actfocusMR <- medright$actionfocus
actfocusBW <- bigwrong$actionfocus

maxtime <- 5*ceiling(max(c(bigright$targetreached, bigwrong$targetreached, medright$targetreached))/5)


#layout of plots
layout(mat = matrix(c(1,2,3,1,2,3,4,4,4),
                    nrow=3,
                    ncol=3),
       heights = c(1,1,1),
       widths = c(1,1,3))

#OK - panel 1
#a random walk to absorbing boundary
par(mar=c(2,4,4,0))
plot(c(0,bigright$zdec),
     xlab='Time',
     ylab='Decision Variable',
     ylim=c(-(decisionboundary+5),(decisionboundary+5)),
     xlim=c(0,maxtime),
     type = 'l',
     lwd=2.5,
     col=alpha('darkblue', 0.75),
     bty='n')
abline(h = c(-decisionboundary,decisionboundary),lty='longdash')
lines(c(0,bigwrong$zdec),
      lwd=2.5,
      col=alpha('orange', 0.75))
lines(c(0,medright$zdec),
      lwd=2.5,
      col=alpha('purple', 0.75))

#panel 2
par(mar=c(2,4,1,0))
plot(c(0,bigright$zmod),
     xlab='Time',
     ylab='DV with Commitment',
     ylim=c(-(decisionboundary+10),(decisionboundary+10)),
     xlim=c(0,maxtime),
     type = 'l',
     lwd=2.5,
     col=alpha('darkblue', 0.75),
     bty='n')
abline(h = c(-decisionboundary,decisionboundary),lty='longdash')
lines(c(0,bigwrong$zmod),
      lwd=2.5,
      col=alpha('orange', 0.75))
lines(c(0,medright$zmod),
      lwd=2.5,
      col=alpha('purple', 0.75))


#panel 3
#the action focus
par(mar=c(5,4,1,0))
plot(bigright$actionfocus[,1],
     ylim=c(-0.2,0.2),
     ylab="Action Focus",
     xlim=c(0,maxtime),
     xlab="Time",
     type='l',
     lwd=2.5,
     col=alpha('darkblue', 0.75),
     bty='n')
lines(bigwrong$actionfocus[,1],
      lwd=2.5,
      col=alpha('orange', 0.75))
lines(medright$actionfocus[,1],
      lwd=2.5,
      col=alpha('purple', 0.75))

#plot 4 showing three trajectories
par(mar=c(5,4,4,1))
plot(t(bigright$effectorpos),
     type='l',
     xlim=c(-0.15,0.15),
     col=alpha(colour = 'darkblue', alpha = 0.75),
     lwd=2.5,
     xlab='x-position',
     ylab='y-position',
     bty='n')
lines(t(bigwrong$effectorpos),
      type='l',
      col=alpha(colour = 'orange', alpha = 0.75),
      lwd=2.5)
lines(t(medright$effectorpos),
      type='l',
      col=alpha(colour = 'purple', alpha = 0.75),
      lwd=2.5)
legend("bottomleft", inset=0.02, title="Example Paths",
       c("Rapid Correct Decision", "Slow Correct Decision", "Slow Incorrect Decision"),
       col = c("purple", "darkblue", "orange"),
       lwd=c(2.5,2.5,2.5))
points(t(bigright$positionatstable), pch=16,lwd=4, col='red')
points(t(bigwrong$positionatstable), pch=16,lwd=4, col='red')
points(t(medright$positionatstable), pch=16,lwd=4, col='red')
