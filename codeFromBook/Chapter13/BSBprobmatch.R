# learned vectors; in the text, these are named
# A and B respectively
v <- list(c(1,1,1,1,-1,-1,-1,-1),
          c(1,1,-1,-1,1,1,-1,-1))

n <- 8 # number of units
maxUpdates <- 100 # maximum number of BSB cycles
init_s <- 1 # length of uncorrupted probe vector
tol <- 1e-8 # tolerance for detecting a difference

alpha <- .025 # learning rate

beta <- 1
epsilon <- 1

nReps <- 1000

noiseSet <- c(0,0.1,0.2,0.4)
startSet <- seq(0,1,length.out = 10)

W <- matrix(rep(0,n*n), nrow=n)

# ----------- learning
for (i in 1:2){
  W <- W + alpha*v[[i]]%*%t(v[[i]])
}

# ----------- test
meanAcc <- {}
meanRT <- {}

for (noise in noiseSet){
  
  tAcc <- rep(0,length(startSet))
  tRT <- rep(0,length(startSet))
  
  accI <- 1
  
  for (startp in startSet){ #* \label{line:noiseSetLoop}  *\#
    
    for (rep in 1:nReps){
      
      # make u a weighted combination of A and B...
      u <- startp*v[[2]] + (1-startp)*v[[1]]
      
      # ...normalize u...
      u <- init_s*u/sqrt(sum(u^2))
      
      # ...and add some Gaussian noise
      u <- u + rnorm(n,0,noise)
      
      # loop across BSB cycles
      for (t in 1:maxUpdates){
        
        # store state of u, we'll need it in a bit to see
        # if it has changed
        ut <- u
        
        # update u...
        u <- beta*u + epsilon * W%*%u #* \label{line:BSBpass}  *\#
        
        #...and then squash the activations
        u[u > 1] <- 1
        u[u < -1] <- -1
        
        # did u change on this update cycle? If not, the model
        # has converged and we break out of the updating loop
        if (all(abs(u-ut)<tol)){
          break
        }
      }
      
      # is it an A response?
      if (all(abs(u-v[[1]])<tol)){
        tAcc[accI] <- tAcc[accI] + 1
      }
      # also record response time
      tRT[accI] <- tRT[accI] + t
    }
    accI <- accI + 1
  }
  # store the results
  meanAcc <- cbind(meanAcc,tAcc/nReps)
  meanRT <- cbind(meanRT, tRT/nReps)
}

# plot results
#pdf(file="BSBresults.pdf", width=9, height=6)
par(mfrow=c(1,2))
matplot(startSet, meanAcc, type="b",
        lty=1:4,col=1,pch=1:4,
        xlab="Starting position", ylab="Proportion 'A' response")
legend(0.7,0.8,
       legend = noiseSet,
       lty=1:4,pch=1:4,col=1)
matplot(startSet, meanRT, type="b",
        lty=1:4,col=1,pch=1:4,
        ylim=c(0,20),
        xlab="Starting position", ylab="Convergence Time")
#dev.off()
