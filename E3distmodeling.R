St3 <- resultinfo3

St3$reachcorr <- as.factor(ifelse(St3$mainreach=="Correct" & St3$initreach=="Incorrect","Corrected","Uncorrected"))

St3$movetime <- St3$movetime/100

St3 <- na.omit(St3)

levels(valids$Difficulty) = c("Baseline","Easy", "Hard")
levels(valids$TSide) = c("Left", "Right")
levels(valids$TFeature) = c("Horizontal", "Vertical")

St3 <- St3[St3$mainreach=="Correct",]

# St3 <- St3[St3$initreach=="Correct",]

minLL <- function(parms, aucdata) {
  if (parms["sd"]<0) 
  {
    return(1000000)} else
  {
    
    dens <- dnorm(aucdata, mean=parms["mean"], sd=parms["sd"])
    return(-2*sum(log(dens)))
  }
}

parms <- c(mean=3,sd=1)

fits <- list()

storefits <- data.frame(TSide=character(), TOrientation=character(), mean=double(), sd=double(), stringsAsFactors = F)

fitmeans <- array(data = NA, dim = c(2,2), dimnames = list(c("Left", "Right"), c("Horizontal", "Vertical")))
fitsds <- array(data = NA, dim = c(2,2), dimnames = list(c("Left", "Right"), c("Horizontal", "Vertical")))

i<-0

for (participant in 2){
  for (Dif in levels(St3$Difficulty)[1]){
    for (Side in levels(St3$TSide)){
      for (Ori in levels(St3$TOrientation)){
        i<-i+1
        partdata <- St3[St3$pNo==participant,c(1,3,4,5,13)]
        conddata <- partdat[partdata$Difficulty==Dif & partdata$TSide==Side & partdata$TOrientation==Ori,]

        aucdata <- conddata[,"AUC"]

        fit <- optim(parms, minLL, aucdata=aucdata, control=list(trace=1))

        fitmeans[Side, Ori] <- fit$par["mean"]
        fitsds[Side,Ori] <- fit$par["sd"]
      }
    }
  }
}


#try  with skewnormal distribution. 
#Note that xi, location, is broadly equivalent to the central tendency
#Note that omega, scale, is broadly equivalent to the sd
#Note that alpha, shape, determines skewness
#Note that tau is the "true mean"

minSN <- function(parms, aucdata) {
  if (parms["omega"]<0)# || abs(parms["xi"])>0.5)# || abs(parms["tau"])>1) 
  {
    return(1000000)} else
    {
      dens <- dsn(aucdata, xi=parms["xi"], omega=parms["omega"], alpha=parms["alpha"])#, tau = parms["tau"])
      return(-2*sum(log(dens)))
    }
}

parms <- c(xi=0,omega=1, alpha=0)#, tau=0)

fitxi <- array(data = NA, dim = c(2,2), dimnames = list(c("Left", "Right"), c("Horizontal", "Vertical")))
fitomega <- array(data = NA, dim = c(2,2), dimnames = list(c("Left", "Right"), c("Horizontal", "Vertical")))
fitalpha <- array(data = NA, dim = c(2,2), dimnames = list(c("Left", "Right"), c("Horizontal", "Vertical")))
fittau <- array(data = NA, dim = c(2,2), dimnames = list(c("Left", "Right"), c("Horizontal", "Vertical")))


for (participant in 2){
  for (Dif in levels(St3$Difficulty)[1]){
    for (Side in levels(St3$TSide)){
      for (Ori in levels(St3$TOrientation)){
        temppart <- St3[St3$pNo==participant,c(1,3,4,5,13)]
        tempdat <- temppart[temppart$Difficulty==Dif & temppart$TSide==Side & temppart$TOrientation==Ori,]
        
        aucdata <- tempdat[,"AUC"]
        
        fit <- optim(parms, minSN, aucdata=aucdata, control=list(trace=1, maxit=10000))
        
        fitxi[Side, Ori] <- fit$par["xi"]
        fitomega[Side, Ori] <- fit$par["omega"]
        fitalpha[Side, Ori] <- fit$par["alpha"]
        #fittau[Side, Ori] <- fit$par["tau"]
      }
    }
  }
}

##fit additive model
#ok, so, baseline model has a mu and sigma associated with it
# if we add a gaussian to the mix (haha) we still need the area under the pdf
# to equal 1. Therefore we add a weighted sum of two gaussians.
# the first gaussian will have a weight of lambda, and the second 1-lambda

data1 <- rnorm(5000,-3,2)
data2 <- 0.5*rnorm(5000,3,2)
datamixed <- c(data1, data2)

hist(datamixed)

#known mean and sd of one distribution mu=-3, sig=2
#joint likelihood will be omega*dnorm(data, -3,2)+(1-omega(dnorm(data, mu2, sig2)))

parms <- c(omega=0.5, mubase=-5, sigbase=1, muadd=4, sigadd=1)

minMG <- function(parms, datamix) {
  mubase <- parms["mubase"]
  sigbase <- parms["sigbase"]
  omega <- parms["omega"]
  muadd <- parms["muadd"]
  sigadd <- parms["sigadd"]
  
  if (sigadd <= 0 || sigbase<=0 || omega > 1) {
    return(100000)} else {
      basedens <- omega*dnorm(datamix, mubase, sigbase)
      adddens <- (1-omega)*dnorm(datamix, muadd, sigadd)
      
      densitysum <- basedens+adddens
      
      return(-2*sum(log(densitysum)))
    }
}

fit <- optim(parms, minMG, datamix=datamixed, control=list(trace=1, maxit=10000))


fit

install.packages("mixtools")
library(mixtools)

fit <- normalmixEM(datamix, k=2)


mixmdl = normalmixEM(aucdata, k=2)
plot(mixmdl,which=2)
lines(density(aucdata), lty=2, lwd=2)

for (participant in 1){
  for (Dif in levels(St3$Difficulty)[1]){
    for (Side in levels(St3$TSide)){
      for (Ori in levels(St3$TOrientation)){
        temppart <- St3[St3$pNo==participant,c(1,3,4,5,13)]
        tempdat <- temppart[temppart$Difficulty==Dif & temppart$TSide==Side & temppart$TOrientation==Ori,]
        
        aucdata <- tempdat[,"AUC"]
        
        name <- paste0("Base", Side, Ori)
        
        print(name)
        
        assign(name, normalmixEM(aucdata))
      }
    }
  }
}
