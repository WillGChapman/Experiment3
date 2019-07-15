#get valid trials

valids <- na.omit(resultinfo3)

head(valids)

summaryBy(AUC~pname+Difficulty+TSide+TFeature, data=valids, FUN=c(mean, sd))

AUCsum <- summarySEwithin(data=valids, measurevar = "AUC", withinvars = c("Difficulty", "TSide", "TFeature"), idvar = "pname")

