source("cumulPT.R")
library(dfoptim)

# function to calculate lnL for CPT
fitCPT <- function(theta, prospects,choices){
  
  lnL <- rep(0,length(prospects))
  
  for (i in 1:length(prospects)){
    cprobs <- CPTchoice(prospects[[i]],
                      theta[1],theta[2],theta[3],theta[4],theta[5],theta[6])
    if (!is.vector(choices)){
      lnL[i] <- sum(log(cprobs[choices[i,]+1]))
    } else {
      lnL[i] <- log(cprobs[choices[i]+1])
    }
  }
  if (any(is.infinite(lnL) | is.na(lnL))){
    return(10000)
  } else {
    return(-2*sum(lnL))
  }
}

dat <- read.csv("Rieskamp2008data.csv",
                  header=T)
prospects <- {}

for (i in 1:length(dat$choicepair)){
  p1 <- list(x=c(dat$A1_payoff[i],
                 dat$A2_payoff[i]),
             p=c(dat$A1_prob[i],
                 dat$A2_prob[i]))
  
  p2 <- list(x=c(dat$B1_payoff[i],
                 dat$B2_payoff[i]),
             p=c(dat$B1_prob[i],
                 dat$B2_prob[i]))
  
  prospects[[i]] <- list(p1=p1,p2=p2)
}

choices <- subset(dat, select=X1:X30)

# fit individuals with lambda free
startPoints <- as.matrix(expand.grid(alpha=c(0.7, 0.9),
                           lambda=c(0.7, 1.4),
                           gamma=c(0.5,0.8),
                           delta=c(0.5,0.8),
                           phi=c(0.05,2)))

fits <- {}

for (subj in 1:30){
  tchoice <- choices[,subj]
  print(paste('Fitting subject ',subj))
  bfit <- list(value=10000)
  for (sp in 1:dim(startPoints)[1]){
    tfit <- nmkb(par=startPoints[sp,],
                       fn = function(theta) fitCPT(c(theta[1],theta[1],theta[2:5]),
                       prospects=prospects, choices=tchoice),
                       lower=c(0,0,0,0,0),
                       upper=c(1,10,1,1,10),
                       control=list(trace=0))
    if (tfit$value < bfit$value){
      bfit <- tfit
    }
    print(paste(sp,tfit$value,bfit$value))
  }
  fits[[subj]] <- bfit
}

# fit individuals with lambda fixed at 1
startPoints <- as.matrix(expand.grid(alpha=c(0.7, 0.9),
                                     gamma=c(0.5,0.8),
                                     delta=c(0.5,0.8),
                                     phi=c(0.05,2)))

fits_con <- {}

for (subj in 1:30){
  tchoice <- choices[,subj]
  print(paste('Fitting subject ',subj))
  bfit <- list(value=10000)
  # alpha=beta, labda=1
  for (sp in 1:dim(startPoints)[1]){
    tfit <- nmkb(par=startPoints[sp,],
                 fn = function(theta) fitCPT(c(theta[1],theta[1],1,theta[2:4]),
                                             prospects=prospects, choices=tchoice),
                 lower=c(0,0,0,0),
                 upper=c(1,1,1,10),
                 control=list(trace=0))
    if (tfit$value < bfit$value){
      bfit <- tfit
    }
    print(paste(sp,tfit$value,bfit$value))
  }
  fits_con[[subj]] <- bfit
}

save('fits_con', file="myFits_con.Rdata")

library(xtable)
parms_full <- do.call(rbind,lapply(fits,function(tx) tx$par))
parms_con <- do.call(rbind,lapply(fits_con,function(tx) tx$par))

# overall
lnLs_full <- do.call(rbind,lapply(fits,function(tx) tx$value))
lnLs_con <- do.call(rbind,lapply(fits_con,function(tx) tx$value))
sum(lnLs_con)-sum(lnLs_full)

print(apply(parms_full,2,mean),digits=2)
print(apply(parms_full,2,sd),digits=2)
print(apply(parms_con,2,mean),digits=2)
print(apply(parms_con,2,sd),digits=2)

# individual
parms_full[(lnLs_con-lnLs_full) > qchisq(.95,1),2]