#VSS SUpplementary info plots

#Fig 2 – Real Reaching Trajectories (two condition overlaid) and AUC distributions

#get easy and hard, right and wrong reaches from E3

#get p3 reaches

participant <- 5

indexp3 <- resultinfo3[resultinfo3$pname==participant, c("eventnumber")]

indexEasy <- resultinfo3[resultinfo3$pname==participant &
                                  resultinfo3$Difficulty=='Esy' &
                                  resultinfo3, c("eventnumber")]



indexHard <- resultinfo3[resultinfo3$pname==participant &
                           resultinfo3$Difficulty=='Hrd', c("eventnumber")]

#remove clear fuckup somewhere
#indexHard <- indexHard[-30]

#rotate lines

plot(rotate(resultdata3[indexEasy[1],1:2,], resultinfo3[indexEasy[1],"mainreach"]), type='l',
     col=alpha(colour = 'blue', alpha = 0.25),
     xlim=c(-0.6, 0.6),
     ylim=c(0,0.9),
     xlab='x', ylab='y',
     las=1)
for (i in indexEasy) 
{
    lines(rotate(resultdata3[i,1:2,], resultinfo3[i,"mainreach"]), type='l',col=alpha(colour = 'blue', alpha = 0.25))
}

for (i in indexHard){
  #print(i)
  lines(rotate(resultdata3[i,1:2,],  resultinfo3[i,"mainreach"]), type='l',col=alpha(colour = 'red', alpha = 0.25))
}

#indexp3 <- indexp3[c(-77, -293)]


plot(rotate(resultdata3[indexp3[1],1:2,], resultinfo3[indexp3[1],"mainreach"]), type='l',
     col=alpha(colour = 'blue', alpha = 0.25),
     xlim=c(-0.6, 0.6),
     ylim=c(0,0.9),
     xlab='x', ylab='y',
     las=1)
for (i in indexp3) 
{
  colourforline <- 'blue'
  if (resultinfo3[i,"Difficulty"]=="Hrd") colourforline <- 'red'
  lines(rotate(resultdata3[i,1:2,], resultinfo3[i,"mainreach"]), type='l',col=alpha(colour = colourforline, alpha = 0.1))
}

#AUC distribution

hist(resultinfo3[indexHard, "AUC"])

hist(resultinfo3[indexHard, "AUC"], breaks=seq(from=-0.2, to=0.4, by=0.005), col=rgb(1,0,0,0.5), xlim=c(-0.2,0.4), ylim=c(0,100), main="Overlapping Histogram", xlab="Variable")
hist(resultinfo3[indexEasy, "AUC"], breaks=seq(from=-0.2, to=0.4, by=0.005), col=rgb(0,0,1,0.5), add=T)
box()

densE <- density(resultinfo3[indexEasy, "AUC"], bw = 0.015)
densH <- density(resultinfo3[indexHard, "AUC"], bw = 0.015)


plot(densH, 
     ylim=c(0,14),
     xlim=c(-0.2, 0.5),
     main="Density of Curvature Measures",
     las=1,
     xlab='Curvature of Trajectory')
polygon(densH, col=alpha(colour = 'red', alpha = 0.5), border='red')
lines(densE)
polygon(densE, col=alpha(colour = 'blue', alpha = 0.5), border='blue')

#Fig 3 – Stochastic Model of trajectories and AUC distributions


