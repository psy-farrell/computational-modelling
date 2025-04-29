n <- 20
n_transfer <- 10
nReps <- 500
maxOrder <- 10
realOrder <- 3

m <- 1:n

bias <- rep(0, maxOrder)
variab <- rep(0,maxOrder)

tcoef <- c(1,-1,1)
kk <- poly(m,length(tcoef))
realf <- kk%*%tcoef

allPreds <- {}
for (modOrder in 1:maxOrder){
  allPreds[[modOrder]] <- matrix(rep(0,n*nReps),ncol=n)
}

for (rep in 1:nReps){
  #tcoef <- c(-1,-1,1)
  dat <- realf + rnorm(n, sd=0.5)
  for (modOrder in 1:maxOrder){
    lm_t <- lm(dat ~ poly(m,modOrder))
    allPreds[[modOrder]][rep,] <- predict(lm_t)
  }
}

#pdf(file="biasVarianceIllus.pdf",width=7,height=7)
par(mfrow=c(2,2))
for (ii in c(1,3,7,10)){
  plot(m,realf,ylim=c(-1.5,1),
       type="b",lwd=2,las=1,
       xlab="x", ylab="y")
  lines(colMeans(allPreds[[ii]]),
        col=rgb(0,0,0,0.5),lwd=3)
  kk <- apply(allPreds[[ii]],2,function(x) quantile(x,c(0.1,0.9)))
  lines(smooth(kk[1,]), lwd=2,lty=2,col=rgb(0,0,0,0.5))
  lines(smooth(kk[2,]), lwd=2,lty=2,col=rgb(0,0,0,0.5))
  legend(10,-0.5,legend=c("true","fitted"), lty=1, 
         col=c("black","grey"),lwd=2)
  title(paste("Order = ",ii))
}
#dev.off()

bias <- rep(0,maxOrder)
variab <- rep(0,maxOrder)
for (ii in 1:maxOrder){
  mm <- colMeans(allPreds[[ii]])
  bias[ii] <- mean((mm-realf)^2)
  kk <- apply(allPreds[[ii]],1,function(x) (x-mm)^2)
  variab[ii] <- mean(rowMeans(kk))
}

#pdf(file="biasVarTradeOff.pdf",width=5,height=5)
par(mfrow=c(1,1))
plot(bias+variab, type="l", lwd=2, lty=2,
     ylim=c(0,0.15), 
     xlab="Complexity", ylab="Error")
lines(bias,lwd=2)
lines(variab,lwd=2,col=rgb(0,0,0,0.5))
legend(6,0.07,
       legend=c("Total Error",expression("Bias" ^ "2"),"Variance"),
       lty=c(2,1,1),lwd=1,col=c("black","black","grey"))
#dev.off()
