source("priorityHeuristic.R")
library(dfoptim)

# function to calculate lnL for priority heuristic with naive
# noise (Rieskamp, 2008)
fitPriority <- function(alpha, prospects,choices){
  
  lnL <- rep(0,length(prospects))
  
  for (i in 1:length(prospects)){
    cprobs <- priorityHeuristic(prospects[[i]],
                      alpha)
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

fits <- {}

for (subj in 1:30){
  tchoice <- choices[,subj]
  print(paste('Fitting subject ',subj))
  bfit <- list(value=10000)
  # fits[[subj]] <- optim(par=c(0.9,0.9,1,.77,.77,0.25),fn = fitCPT,
  #                        prospects=prospects, choices=tchoice,
  #                        method="L-BFGS-B",
  #                        lower=c(0,0,0,0.0,0.0,0),
  #                        upper=c(1,1,10,1,1,10),
  #                        control=list(trace=1))
  # fits[[subj]] <- nmkb(par=c(0.9,0.9,1.1,.7,.77,0.1),fn = fitCPT,
  #                       prospects=prospects, choices=tchoice,
  #                       lower=c(0,0,1,0.4,0.4,0),
  #                       upper=c(1,1,10,1,1,10),
  #                       control=list(trace=1))
  # alpha=beta
  fits[[subj]] <- optim(par=0.25,
                       fn = function(alpha) fitPriority(alpha,
                       prospects=prospects, choices=tchoice),
                       method="Brent",
                       lower=0,
                       upper=0.5,
                       control=list(trace=0))
}

alpha_est <- do.call(rbind,lapply(fits,function(tx) tx$par))
lnLs <- do.call(rbind,lapply(fits,function(tx) tx$value))