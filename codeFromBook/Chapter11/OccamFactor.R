theta <- seq(0,1,length.out = 100)
y <- seq(0,1,length.out = 100)

z <- matrix(rep(0,100*100),nrow=100)
#pdf(file="BayesOccam.pdf", width=8, height=8)
    
par(mfrow=c(2,2))

for (tt in 1:100){
  for (ty in 1:100){
    z[tt,ty] <- dnorm(y[ty], mean=theta[tt], sd=0.1)
  }
}

image(z, col= gray(seq(1,0.5,length.out = 1000)),
      xlab=expression(theta), ylab="y")
abline(0.5,0, lty=1, lwd=2); text(0.1, 0.4, "y=0.5")
abline(0.8,0, lty=2, lwd=2); text(0.1, 0.7, "y=0.8")
title("Complex Model")

for (tt in 1:100){
  for (ty in 1:100){
    z[tt,ty] <- dnorm(y[ty], mean=0.5, sd=0.1)
  }
}

image(z, col= gray(seq(1,0.5,length.out = 1000)),
      xlab=expression(theta), ylab="y")
abline(0.5,0, lty=1, lwd=2); text(0.1, 0.4, "y=0.5")
abline(0.8,0, lty=2, lwd=2); text(0.1, 0.7, "y=0.8")
title("Simple Model")

yy <- cbind(dnorm(0.5, mean=theta, sd=0.1),dnorm(0.8, mean=theta, sd=0.1))
#ylab=paste("p(y|", expression(theta), ")"))
matplot(theta, yy, type="l", lty=1:2, xlab=expression(theta),col=1,
        ylab=expression(paste("p(y|")* theta * "," * M[complex] * ")"),
        ylim=c(0,5))

yy <- cbind(dnorm(0.5, mean=rep(0.5,100), sd=0.1),dnorm(0.8, mean=rep(0.5,100), sd=0.1))
#ylab=paste("p(y|", expression(theta), ")"))
matplot(theta, yy, type="l", lty=1:2, xlab=expression(theta),col=1,
        ylab=expression(paste("p(y|")* theta * "," * M[simple] * ")"),
        ylim=c(0,5))
legend(0,3,lty=1:2, col=1, c("y=0.5","y=0.8"), text.width = 0.2)
#dev.off()