nTrials <- 1000

r1 <- rnorm(nTrials,mean = 5,sd = 1)
r2 <- rnorm(nTrials,mean = 5.5,sd = 1)

r <- rbind(r1,r2)

epsilon <- 0.1
alpha = 0.1

Qrecord <- r*0

nRuns <- 1000

for (run in 1:nRuns){ #* \label{line:NeuroscienceModels:banditLoop}  *\#
  
  Q <- rnorm(2,0,.001)
  
  QthisRun <- r*0
  
  
  for (i in 1:nTrials){
    
    # select action using e-greedy
    if (runif(1)<epsilon){
      # explore
      a <- sample(2,1)
    } else {
      # greedy
      a <- which.max(Q)[1]
    }
    
    # learn from the reward
    Q[a] <- Q[a] + alpha*(r[a,i] - Q[a])
    QthisRun[,i] <- Q
  }
  Qrecord <- Qrecord + QthisRun
}
#pdf(file="banditTask.pdf", width=5, height=4)
matplot(t(Qrecord/nRuns), type="l", ylim=c(0,10),
        las=1, xlab="Trial", ylab="Mean Q")
#dev.off()