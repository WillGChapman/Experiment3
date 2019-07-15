Study1 <- resultinfo1
Study2 <- resultinfo2
#Study3 <- resultinfo3



#add study factor
Study1$Study <- as.factor(1)
Study2$Study <- as.factor(2)

#rename because pname is stupid
colnames(Study1)[4] <- "Participant"
colnames(Study2)[4] <- "Participant"

#rename participants because that looks better
levels(Study1$Participant) = paste0("P", 1:length(levels(Study1$Participant)))
levels(Study2$Participant) = paste0("P", (1+length(levels(Study1$Participant))):(length(levels(Study1$Participant))+length(levels(Study2$Participant))))

#combine data frames
StudyComb = rbind(Study1, Study2)

#get only correct reaches
StudyComb = StudyComb[StudyComb$mainreach == "Correct",]

#rename factor levels to make charts look nicer
levels(StudyComb$Difficulty) = c("Easy", "Hard")
levels(StudyComb$TSide) = c("Left", "Right")
levels(StudyComb$TFeature) = c("Horizontal", "Vertical")
levels(StudyComb$Study) = c("Study 1", "Study 2 (with time limit)")

#get reaches with no changes of mind
#StudyComb = Study3[Study3$initreach == "Correct",]

withinSEAUC=summarySEwithin(StudyComb, measurevar="AUC", withinvar=c("Difficulty", "TSide", "TFeature"), betweenvars="Study", idvar="Participant", na.rm=TRUE, conf.interval = 0.95, .drop=TRUE)


levels(withinSEAUC$Difficulty) = c("Easy", "Hard")
levels(withinSEAUC$TSide) = c("Left", "Right")
levels(withinSEAUC$TFeature) = c("Horizontal", "Vertical")
levels(withinSEAUC$Study) = c("Study 1", "Study 2 (with time limit)")


AUCplot = ggplot(withinSEAUC, aes(x=TFeature, y=AUC, fill=Difficulty))
AUCplot+geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=AUC-se, ymax=AUC+se)) +
  facet_grid(.~Study+TSide)+
  scale_fill_manual(values=c("#CCFFCC", "#FFCCCC"), 
                    name="Trial Difficulty")+
  ggtitle("Combined: Area Under Curve broken down by Target Side and Orientation")+
  xlab("Target Orientation")+
  ylab("Area Under Curve")

ggsave("bothAUC.png", width=15, height=7.5)
