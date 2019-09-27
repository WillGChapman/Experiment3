#bootstrap approach - add a random baseline trajectory to every simulated trajectory
#other bootstrap - use baseline AUC to modify the output AUC

#get baseline trajectoies and assosiated AUCs

PIndex <- resultinfo3$pname==3

DiffIndex <- resultinfo3$Difficulty=="Bas"

baselineAUC <- resultinfo3[DiffIndex & PIndex, "AUC"]
baselineIndex <- resultinfo3[DiffIndex & PIndex, "eventnumber"]

#take a set of simulated trajectories
#get their AUCs

#resimulate trajectories
walks <- ranwalks(1024, 500, 1, 1)
trajs <- trajectories(walks)

#add random baseline experimental AUC to each simAUC (pool of 30/240 baselines into 1000 simulation AUCs)
#draw AUC from baseline
AUCtoadd <- baselineAUC[21]

#choose a random baseline trajectory
exptrajectory <- resultdata3[baselineIndex[21],3:4,]

#correct to terminate at 0.5,0.8660

exptrajectory[1,] <- exptrajectory[1,]*(sin(30*pi/180)/exptrajectory[1,101])
exptrajectory[2,] <- exptrajectory[2,]*(sin(60*pi/180)/exptrajectory[2,101])

#get AUC
expAUC <- -polyarea(exptrajectory[1,], exptrajectory[2,])

#take a simulated trajectory
simtraj <- trajs$effectorpos[[1]]

#get AUC
simAUC <- -polyarea(simtraj[1,], simtraj[2,])

correctedAUC <- expAUC+simAUC

#now get modification of baseline
diffx <- exptrajectory[1,]-seq(from=0, to=0.5, length.out = 101)
diffy <- exptrajectory[2,]-seq(from=0, to=sind(60), length.out = 101)
#recalculate AUCs

newx <- simtraj[1,] + diffx
newy <- simtraj[2,] + diffy

modAUCcoord <- -polyarea(newx, newy)

#do this 100 times on randomly drawn AUCs and see if they match consistently.
