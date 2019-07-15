
library(R.matlab)
library(lme4)
library(Rmisc)
library(tidyverse)
library(doBy)

rm(list=ls())


#rerun these if you need to
resultdata1 <- readMat("resultdata1.mat")
resultdata1 <- resultdata1$resultdata
resultinfo1 <- read.csv("resultinfo1.csv")

levels(resultinfo1) <- 1:length(levels(resultinfo1))

resultdata2 <- readMat("resultdata2.mat")
resultdata2 <- resultdata2$resultdata
resultinfo2 <- read.csv("resultinfo2.csv")

levels(resultinfo2) <- 1:length(levels(resultinfo2))


resultdata3 <- readMat("resultdata3.mat")
resultdata3 <- resultdata3$resultdata
resultinfo3 <- read.csv("resultinfo3.csv")

resultinfo3$plabel <- resultinfo3$pname
resultinfo3$pname <- as.factor(substr(resultinfo3$plabel, 2,2))
resultinfo3$psession <- as.factor(substr(resultinfo3$plabel, 3,3))
resultinfo3$pgender <- as.factor(substr(resultinfo3$plabel, 4,4))
resultinfo3$page <- as.factor(substr(resultinfo3$plabel, 5,6))
#levels(resultinfo3$pname) <- 1:length(levels(resultinfo3$pname))

resultdata4 <- readMat("../resultdata4.mat")
resultdata4 <- resultdata4$resultdata
resultinfo4 <- read.csv("../resultinfo4.csv")
levels(resultinfo4$pname)[29:31] <- "p33f20"
levels(resultinfo4) <- 1:length(levels(resultinfo4$pname))

# resultdata[trial, X, T]

# first dimension: event number
# second dimension: normalised variable (call with datavar$[datayouwant]) 
#  1:2, xy in 101 time bins
#  3:4, xy in 101 distance bins
#  5, wrist in 101 time bins
#  6, wrist in 101 distance bins
#  7, finger aperture in 101 time bins
#  8, finger aperture in 101 distance bins
#  9:11, xyz of knuckle from trial start.
#  12, z height by time
#  13, z height by distance
measure <-  list(xytime = 1:2, xydist = 3:4, wristtime = 5, wristdist = 6, fingertime = 7,
     fingerdist = 8, knuckle = 9:11, ztime = 12, zdist = 13)
