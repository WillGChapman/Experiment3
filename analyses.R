#OK time to do some descriptives and model comparisons.

#get data to analyse
resinfo <- resultinfo2

#add Distractor Congruency column

colnames(resinfo)[which(colnames(resinfo)=="pname")] <- "Participant"

features <- levels(resinfo$TFeature)

resinfo$DCong <- as.factor(xor(resinfo$TFeature==features[1], resinfo$DFeature==features[1]))

#remove incorrect reaches
resinfo<- resinfo[resinfo$mainreach=="Correct",]

#some data wrangling
resinfo$RTthresh <- resinfo$VthresholdGC*10
resinfo$MT <- resinfo$movetime*10
resinfo$logRT <- log(resinfo$VthresholdGC)
resinfo$logMT <- log(resinfo$MT)
resinfo$acceltime <- resinfo$movetime-resinfo$deceltime*10
resinfo$transportratio <- resinfo$deceltime/resinfo$acceltime
resinfo$stopY1 <- resinfo$stopY
resinfo$stopX1 <- abs(resinfo$stopX)

#do a model comparison to get which manipulated variables affect the outcome.


#do MLM comparison to see whether effect is down to which bits.
outcomevar = "AUC"

combo=list()

#NULL model first
combo[1] = "(1|Participant)"
#NULL plus target side
#Then with expected kinematic constraints
combo[2] = "(1|Participant) + TSide"
combo[3] = "(1|Participant) + TSide * TFeature"
#Then with just the difficulty manipulation
#Then with the variable of interest (difficulty as fixed)
combo[4] = "(1|Participant) + TSide * TFeature + Difficulty"
combo[5] = "(1|Participant) + Difficulty + TSide * TFeature + DCong"
#then difficulty as random
#combo[6] = "(1|Participant) + TSide * TFeature + DCong + (1|Difficulty)"

listofformulae <- paste(outcomevar, "~", combo)

#clean incorrect main reaches

resinfo <- resinfo[resinfo$mainreach=="Correct",]

models = lapply(listofformulae, lmer, data=resinfo, REML=FALSE)

anovaofall = eval(parse(text=paste("anova(",paste("models[[",1:length(models),"]]",sep="",collapse=","),")")))
anovaofall

levels(resinfo$Difficulty) <- c("Easy", "Hard")
levels(resinfo$TSide) <- c("Left", "Right")
levels(resinfo$TFeature) <- c("Narrow", "Wide")
sumvals <- summarySEwithin(data=resinfo, measurevar = "maxaptime", withinvars = c("Difficulty", "TSide", "TFeature"), idvar="Participant", na.rm=TRUE, conf.interval = 0.95, .drop=TRUE)

AUCplot <- ggplot(sumvals, aes(x=TFeature, y=maxaptime, fill=Difficulty))
AUCplot+geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=maxaptime-se, ymax=AUC+se)) +
  facet_grid(.~TSide)+
  scale_fill_manual(values=c("#CCFFCC", "#FFCCCC"), # "#CCCCFF", 
                    name="Trial Difficulty")+
  ggtitle("Experiment 4 - with baseline trials")+
  xlab("Target Orientation")+
  ylab("maximum aperture time")


RTdf <- cbind(Experiment=4, sumvals[, c(1,3,5)])

RTdfall <- rbind(RTdfall, RTdf)

summarySEwithin(data=resinfo, measurevar =  "maxaptime" , withinvars = c("TFeature"), idvar="Participant", na.rm=TRUE, conf.interval = 0.95, .drop=TRUE)

View(summarySEwithin(data=resinfo, measurevar = "MT", withinvars = c("Difficulty"), idvar="pname", na.rm=TRUE, conf.interval = 0.95, .drop=TRUE))


summaryBy(pathlengthmm~Difficulty, data=resinfo, FUN = function(x) (mean(x)-1)*100)




#do MLM comparison to see whether effect is down to which bits.
outcomevar = "AUC"

combo=list()

#NULL model first
combo[1] = "(1|Participant)"
#Then with expected kinematic constraints
combo[2] = "(1|Participant) + TSide * TFeature"
#Then with the variable of interest (difficulty as fixed)
combo[3] = "(1|Participant) + TSide * TFeature + Difficulty"
combo[4] = "(1|Participant) + TSide * TFeature + Difficulty + DCong"
#then difficulty as random
#combo[4] = "(1|Participant) + TSide * TOrientation + (1|Difficulty)"

listofformulae <- paste(outcomevar, "~", combo)

#clean incorrect main reaches

resinfo <- resultinfo1[resultinfo1$mainreach=="Correct",]

models = lapply(listofformulae, lmer, data=resinfo, REML=FALSE)

anovaofall = eval(parse(text=paste("anova(",paste("models[[",1:length(models),"]]",sep="",collapse=","),")")))
