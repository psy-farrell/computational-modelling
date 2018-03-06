n <- 1000
m <- seq(1,10,length.out = n)

#pdf(file="polycomplex.pdf", width=7, height=4)

par(mfrow=c(1,2))

for (modOrder in c(2,10)){
  plot(1:10,1:10,xlim=c(0.5,10.5), ylim=c(0,5), type="n",
       xlab="Physical intensity", ylab="Sensation", las=1)
  kk <- poly(x = m,degree = modOrder)
  for (rep in 1:10){
    tcoef <- rnorm(modOrder, sd=5)
    #tcoef <- rexp(modOrder, rate=0.2)
    y <- t(kk%*%tcoef)
    y <- y - min(y)
    lines(x=m, y, type="l", col=rgb(0,0,0,0.5))
  }
}

#dev.off()