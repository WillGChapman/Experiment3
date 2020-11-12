source("figlabel.R")
source("actionfocus.R")
source("modifyAUC.R")
source("ranwalks.R")
#source("trajectories.R")
source("trajectoriesRealGeometry.R")
#action focus plot

#get ranwalks
#walks <- ranwalks(1024,500,1,1,F)



#make trajectories

decisionboundary <- 5

walks <- ranwalks(n_trials = 10000, n_time_samples = 500, drift_rate = 1, noise_sd = 1)

#load("walksplot.RData")

trajs <- trajectories(walks, decbound = decisionboundary, model4 = FALSE)

#get which trajectories get the correct target
wdIndex <- which(trajs$decision==1)
cdIndex <- which(trajs$decision==2)

#get some specific trajectories
biggestwrongAUC <- wdIndex[which.min(trajs$AUC[wdIndex])]
biggestrightAUC <- cdIndex[which.max(trajs$AUC[cdIndex])]
medianrightAUC <- which(trajs$AUC==median(trajs$AUC[cdIndex]))

if(isempty(medianrightAUC)) medianrightAUC <- which(trajs$AUC==median(trajs$AUC[cdIndex[2:length(cdIndex)]]))

#get individual trajectory data for these:

bigright <- actionfocus(onewalk = walks[biggestrightAUC,], decbound = decisionboundary, model4 = FALSE, gain = 4, suppresscount = TRUE)
bigwrong <- actionfocus(onewalk = walks[biggestwrongAUC,], decbound = decisionboundary, model4 = FALSE, gain = 4, suppresscount = TRUE)
medright <- actionfocus(onewalk = walks[medianrightAUC,], decbound = decisionboundary, model4 = FALSE, gain = 4, suppresscount = TRUE)

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
     bty='n',
     cex.lab=1.25, cex.axis=1.25, cex.main=1.25, cex.sub=1.25)
points(x=bigright$timeatstable, y=bigright$zdec[bigright$timeatstable-1], pch=16,lwd=8, col='red')
abline(h = c(-decisionboundary,decisionboundary),lty='longdash')
lines(c(0,bigwrong$zdec),
      lwd=2.5,
      col=alpha('orange', 0.75))
points(x=bigwrong$timeatstable, y=bigwrong$zdec[bigwrong$timeatstable-1], pch=16,lwd=8, col='red')
lines(c(0,medright$zdec),
      lwd=2.5,
      col=alpha('purple', 0.75))
points(x=medright$timeatstable, y=medright$zdec[medright$timeatstable-1], pch=16,lwd=8, col='red')
fig_label("a)", cex=2)

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
     bty='n',
     cex.lab=1.25, cex.axis=1.25, cex.main=1.25, cex.sub=1.25)
points(x=bigright$timeatstable, y=bigright$zmod[bigright$timeatstable-1], pch=16,lwd=8, col='red')
abline(h = c(-decisionboundary,decisionboundary),lty='longdash')
lines(c(0,bigwrong$zmod),
      lwd=2.5,
      col=alpha('orange', 0.75))
points(x=bigwrong$timeatstable, y=bigwrong$zmod[bigwrong$timeatstable-1], pch=16,lwd=8, col='red')
lines(c(0,medright$zmod),
      lwd=2.5,
      col=alpha('purple', 0.75))
points(x=medright$timeatstable, y=medright$zmod[medright$timeatstable-1], pch=16,lwd=8, col='red')
fig_label("b)", cex=2)

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
     bty='n',
     cex.lab=1.25, cex.axis=1.25, cex.main=1.25, cex.sub=1.25)
points(x=bigright$timeatstable, y=bigright$actionfocus[bigright$timeatstable,1], pch=16,lwd=8, col='red')
lines(bigwrong$actionfocus[,1],
      lwd=2.5,
      col=alpha('orange', 0.75))
points(x=bigwrong$timeatstable, y=bigwrong$actionfocus[bigwrong$timeatstable,1], pch=16,lwd=8, col='red')
lines(medright$actionfocus[,1],
      lwd=2.5,
      col=alpha('purple', 0.75))
points(x=medright$timeatstable, y=medright$actionfocus[medright$timeatstable,1], pch=16,lwd=8, col='red')
fig_label("c)", cex=2)

#plot 4 showing three trajectories
par(mar=c(5,4,4,1))
plot(t(bigright$effectorpos),
     type='l',
     xlim=c(-0.15,0.15),
     col=alpha(colour = 'darkblue', alpha = 0.75),
     lwd=3,
     xlab='x-position',
     ylab='y-position',
     bty='n',
     cex.lab=1.25, cex.axis=1.25, cex.main=1.25, cex.sub=1.25)
lines(t(bigwrong$effectorpos),
      type='l',
      col=alpha(colour = 'orange', alpha = 0.75),
      lwd=3)
lines(t(medright$effectorpos),
      type='l',
      col=alpha(colour = 'purple', alpha = 0.75),
      lwd=3)
legend("bottomleft", inset=0.02, title="Example Paths",
       c("Rapid Correct Decision", "Slow Correct Decision", "Incorrect Decision"),
       col = c("purple", "darkblue", "orange"),
       lwd=c(2.5,2.5,2.5))
points(t(bigright$positionatstable), pch=16,lwd=12, col='red')
points(t(bigwrong$positionatstable), pch=16,lwd=12, col='red')
points(t(medright$positionatstable), pch=16,lwd=12, col='red')
fig_label("d)", cex=2)