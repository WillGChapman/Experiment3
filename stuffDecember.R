#script to handle feed a matrix of walks into actionfocus, and get stats out the other end

colfun <- colorRampPalette(colors = c("blue", "yellow"), 1)
colfun(100)[5]


plot(xlim =c(0,100),
     ylim=c(0,2))

for (i in 1:100){
  points(x = i, y=1,
         col=colfun(100)[i],
         pch=16)
}

stuff <- apply(walks[1:5,], MARGIN = 1, actionfocus)[1][1]
stuff



### new stuff for monday:
# one figure per parameter. Each figure has ~3 versions with parameters around the useful value 
# showing trajectories, and three showing AUC distributions.
# once you have the above, make some more AUC dist charts to show any weird interactions between the parameters.