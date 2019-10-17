#plot difference between changing AUC with simple add, and coordinate shift

#load libs
library(tidyverse)
library(R.matlab)
library(pracma)

#source custom functions
source("ranwalks.R")
source("trajectoriesRealGeometry.R")
source("getAUClist.R")

#import data

resultdata3 <- readMat("Data/resultdata3.mat")
resultdata3 <- resultdata3$resultdata
resultinfo3 <- read.csv("Data/resultinfo3.csv")

resultinfo3$plabel <- resultinfo3$pname
resultinfo3$pname <- as.factor(substr(resultinfo3$plabel, 2,2))
resultinfo3$psession <- as.factor(substr(resultinfo3$plabel, 3,3))
resultinfo3$pgender <- as.factor(substr(resultinfo3$plabel, 4,4))
resultinfo3$page <- as.factor(substr(resultinfo3$plabel, 5,6))

#get experimental trajectories
PLogical <- resultinfo3$pname==3
DiffLogical <- resultinfo3$Difficulty=="Bas"
baselineIndex <- resultinfo3[DiffLogical & PLogical, "eventnumber"]

explines <- resultdata3[baselineIndex,3:4,]

walks <- ranwalks(n_trials = 1024, n_time_samples = 500, drift_rate = 1, noise_sd = 1)

trajs <- trajectories(walks,
                      decbound = 5,
                      model4 = TRUE)

simlines <- simplify2array(trajs$effectorpos)

AUCOut <- getAUClist(explines, simlines)


#piping into ggplot does NOT give you a ggplot object to mess around with. hich is nice in it's own way.
AUCOut %>%
  drop_na() %>%
  ggplot(aes(x=simAUC)) +
  geom_point(aes(y=AUCadded-AUCmodcoord), colour='darkblue', size=1)


