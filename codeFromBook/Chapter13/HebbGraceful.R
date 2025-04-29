library(lsa)

n <- 100 # number of input units
m <- 50 # number of output units

listLength <- 20 # number of pairs in each list

nReps <- 100

alpha <- .25

stimSimSet <- c(0, .25, .5, .75, 1)

accuracy <- rep(0,length(stimSimSet))

for (rep in 1:nReps){
  
  W <- matrix(rep(0,m*n), nrow=m)
  
  stim1 <- {}
  resp1 <- {}
  
  # create study set
  for (litem in 1:listLength){
    
    svec <- sign(rnorm(n))
    stim1 <- c(stim1,list(svec))
    
    rvec <- sign(rnorm(m))
    resp1 <- c(resp1,list(rvec))
    
  }
  
  # study list
  for (litem in 1:listLength){ #* \label{line:gracelearningloop}  *\#
    c <- stim1[[litem]]
    o <- resp1[[litem]]
    W <- W + alpha*o%*%t(c)
  }
  
  # loop across probe stimuli of differing similarity to
  # the trained stimuli, and use these to probe for
  # responses
  for (stimSimI in 1:length(stimSimSet)){ #* \label{line:stimSimSetloop}  *\#
    
    stimSim <- stimSimSet[stimSimI]
    
    # create test stimuli
    stim2 <- {} #* \label{line:beginstim2}  *\#
    for (litem in 1:listLength){
      
      svec <- sign(rnorm(n))
      mask <- runif(n)<stimSim  #* \label{line:stim2stimSim}  *\#
      stim2 <- c(stim2,list(mask*stim1[[litem]] + (1-mask)*svec)) #* \label{line:stim2mixture}  *\#
      
    }
    
    # test list #* \label{line:gracetesting}  *\#
    tAcc <- 0
    for (litem in 1:listLength){
      c <- stim2[[litem]]
      o <- W %*% c #* \label{line:cueMatrix}  *\#
      
      tAcc <- tAcc + cosine(as.vector(o),resp1[[litem]])
    }
    accuracy[stimSimI] <- accuracy[stimSimI] + tAcc/listLength
    
  } 
}# end reps loop

#pdf("HebbGraceful.pdf", width=5, height=5)
plot(stimSimSet,accuracy/nReps, type="b",
     xlab="Stimulus-Cue Similarity",
     ylab="Cosine")
#dev.off()