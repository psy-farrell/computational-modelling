n <- 20
n_transfer <- 2
nReps <- 1000
maxOrder <- 6

trainErr <- rep(0, maxOrder)
testErr <- rep(0, maxOrder)

m <- 1:n

tcoef <- c(1,-1,1)
kk <- poly(m,length(tcoef))
realf <- t(kk%*%tcoef)

plot(m, realf)

for (rep in 1:nReps){
  #tcoef <- c(-1,-1,1)
  dat <- realf + rnorm(n, sd=0.5)
  m_t <- sample.int(n, size=n_transfer)
  m <- setdiff(1:n,m_t)
  for (modOrder in 1:maxOrder){
    tTrain <- 0
    tTest <- 0
    lm_t <- lm(dat[m] ~ poly(m,modOrder))
    tTrain <- tTrain + mean(lm_t$residuals^2)
    tTest <- tTest + mean((predict(lm_t, data.frame(m=m_t))-dat[m_t])^2)
    trainErr[modOrder] <- trainErr[modOrder] + tTrain
    testErr[modOrder] <- testErr[modOrder] + tTest
  }
}

kk <- cbind(trainErr,testErr)/nReps

#pdf(file="outOfSet.pdf",width=4,height=5)
matplot(kk, type="l",lty=1,
        ylim=c(0,0.7),
        lwd=2,col=c("black","grey"),
        xlab="Complexity",ylab="Prediction Error")
legend(1,0.7,c("Train","Test"),lwd=2,lty=1,col=c("black","grey"))
#dev.off()