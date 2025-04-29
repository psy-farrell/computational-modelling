nTrials <- 40 # number of trials
nSteps <- 25 # number of time steps in each trial
stimStep <- 5 # time step at which stimulus is presented
    # the reward is presented at the last time step

# a matrix to record the deltas
alld <- matrix(rep(0,nTrials*nSteps),ncol=nSteps)

w <- rep(0,nSteps+1)

gamma <- 1
alpha <- 0.5

for (trial in 1:nTrials){
  
  sumd <- rep(0,nSteps+1)
  
  # we don't use t as a variable, because
  # this is reserved in R
  
  for (s in 1:nSteps){
    
    # to take the temporal difference, we need x(t)...
    x <- rep(0,nSteps+1)
    if (s>stimStep){
      x[s-stimStep] <- 1
    }
    
    # ...and also x(t+1)
    x1 <- rep(0,nSteps+1)
    if ((s+1)>stimStep){
      x1[s+1-stimStep] <- 1
    }
    
    # if it is the last step, we get a reward
    if (s==nSteps){
      r=1
    } else {
      r=0
    }
    
    # calculate reward predictions for t and t+1
    Vt <- sum(w*x)
    Vt1 <- sum(w*x1)
    
    # calculate prediction error
    dt <- r + gamma*Vt1 - Vt
    
    # this is just record keeping, to track prediction errors
    # (we'll plot this later)
    alld[trial,s] <- dt
    
    # this is the sum across t that is used to update w
    # at the end of the trial
    sumd <- sumd + x*dt
  }
  w <- w + alpha * sumd
}

pdf(file="phasicTD.pdf", width = 5, height=8)
par(mfrow=c(4,1))
for (sp in c(1,12,25,40)){
  plot(alld[sp,], type="l", lwd=2, las=1,
       xlab="Time step", ylab="Prediction Error",
       ylim=c(0,1))
  text(2,0.8,paste("Trial ",sp))
}
dev.off()

# library(plot3D)
# axes3d <- mesh(1:nSteps, 1:nTrials)
# persp3D(axes3d$x, axes3d$y, t(alld),
#        ticktype="detailed", expand=0.5)
# persp(1:nSteps,1:nTrials,t(alld))
