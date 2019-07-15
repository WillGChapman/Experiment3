#make a nice chart with 
#data
#density
#prediction from parameter optimisation

#get the data from the baseline trial

participant <- 4
Diff <- levels(valids$Difficulty)[1]
Side <- levels(valids$TSide)[1]
Orie <- levels(valids$TFeature)[1]

temppart <- subset(resultinfo3, pname==participant, c("eventnumber", "pname", "Difficulty", "TSide", "TFeature", "DFeature", "AUC")) #St3[St3$pNo==participant,c(1,3,4,5,13)]

distcomp <- ggplot(temppart, aes(x=AUC, fill=Difficulty))+
  #geom_histogram(fill="grey", colour="black", binwidth = 0.01)+
  geom_density(size=1, kernel="gaussian", adjust=1, alpha=0.25)+
  #geom_line(data=simdata, aes(y=normpred), colour="blue", size=1, alpha=0.7)+
  #geom_line(data=simdata, aes(y=snpred), colour="dark green", size=2)+
  xlim(-0.2,0.2)+
  facet_grid(Difficulty~.)

distcomp

#make up some data
simauc <- seq(from=-0.2, to=0.2, length.out = 100)
Sides <- c(rep("Left", times=100), rep("Right", times=100),rep("Left", times=100), rep("Right", times=100))
Oris <- c(rep("Horizontal", times=200), rep("Vertical", times=200))

simdata <- data.frame(TSide = Sides, TFeature = Oris, AUC=rep(simauc, times=4))

simAUCs <- NULL

for (i in 1:dim(simdata)[1]){
  simAUCs[i] <- dnorm(simdata$AUC[i], fitmeans[simdata$TSide[i], simdata$TFeature[i]], fitsds[simdata$TSide[i], simdata$TFeature[i]])
}

simdata$normpred <- simAUCs

simsnAUC <- NULL
for (i in 1:dim(simdata)[1]){
  simsnAUC[i] <- dsn(simdata$AUC[i], xi= fitxi[simdata$TSide[i], simdata$TFeature[i]], omega=fitomega[simdata$TSide[i], simdata$TOrientation[i]], alpha =  fitalpha[simdata$TSide[i], simdata$TOrientation[i]])#, fittau[simdata$TSide[i], simdata$TOrientation[i]])
}
simdata$snpred <- simsnAUC

tempdat$predsnAUC <- predsnAUC

distcomp <- ggplot(temppart, aes(x=AUC, y=..density..))+
  geom_histogram(fill="grey", colour="black", binwidth = 0.01)+
  geom_density(size=1, colour="purple", kernel="gaussian", adjust=1, alpha=0.1)+
  #geom_line(data=simdata, aes(y=normpred), colour="blue", size=1, alpha=0.7)+
  #geom_line(data=simdata, aes(y=snpred), colour="dark green", size=2)+
  xlim(-0.2,0.2)+
  facet_grid(Difficulty~.)

distcomp

ggsave("P2densitywithest.png")

