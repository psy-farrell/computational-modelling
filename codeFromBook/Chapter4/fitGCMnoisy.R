source("GCMprednoisy.R")
library(dfoptim)

# A function to get deviance from GCM
GCMutil <- function(theta, stim, exemplars, data, N, retpreds){
  nDat <- length(data)
  dev <- rep(NA, nDat)
  preds <- dev
  
  c <- theta[1]
  w <- theta[2]
  w[2] <- (1-w[1])*theta[3]
  w[3] <- (1-sum(w[1:2]))*theta[4]
  w[4] <- (1-sum(w[1:3]))
  sigma <- theta[5]
  b <- theta[6]
  
  for (i in 1:nDat){
    p <- GCMprednoisy(stim[i,], exemplars, c, w, sigma, b)
    dev[i] <- -2*log(dbinom(data[i] ,size = N,prob = p[1]))
    preds[i] <- p[1]
  }
  
  if (retpreds){
    return(preds)
  } else {
    return(sum(dev))
  }
}


N <- 2*40 # there were 2 responses per face from 40 ppl 

stim <- as.matrix(read.table("faceStim.csv", sep=",")) #* \label{line:MaximumLikelihood:readFaces_noisy}  *\#

exemplars <- list(a=stim[1:5,], b= stim[6:10,]) #* \label{line:MaximumLikelihood:assignExemplars_noisy}  *\#

data <- scan(file="facesDataLearners.txt")
data <- ceiling(data*N)

bestfit <- 10000

for (w1 in c(0.25,0.5,0.75)){ #* \label{line:GCMLoopBegin}  *\#
  for (w2 in c(0.25,0.5,0.75)){
    for (w3 in c(0.25,0.5,0.75)){
      print(c(w1,w2,w3))
      fitres <- nmkb(par=c(1,w1,w2,w3,1,0.2),
           fn = function(theta) GCMutil(theta,stim,exemplars,data, N, FALSE),
           lower=c(0,0,0,0,0,-5),
           upper=c(10,1,1,1,10,5),
           control=list(trace=0))
      print(fitres)
      if (fitres$value<bestfit){
        bestres <- fitres
        bestfit <- fitres$value
      }
    }
  }
} #* \label{line:GCMLoopEnd}  *\#


preds <- GCMutil(bestres$par,stim,exemplars,data, N, TRUE)

#pdf(file="GCMfits.pdf", width=5, height=5)
plot(preds,data/N,
     xlab="Data", ylab="Predictions")
#dev.off()

print(bestres)
theta <- bestres$par
w <- theta[2]
w[2] <- (1-w[1])*theta[3]
w[3] <- (1-sum(w[1:2]))*theta[4]
w[4] <- (1-sum(w[1:3]))
print(w)