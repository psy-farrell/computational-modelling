library(logspline)

source("SDT_small.R")
mcmcs <- as.matrix(mcmcfin)

blogspl <- logspline(mcmcs[,"b"])

BF <- dlogspline(0,blogspl)/dnorm(0,0,1)
print(BF)

#pdf(file="SavageD.pdf", width=5, height = 5)
x <- seq(-0.25,0.25,length.out = 1000)
priy <- dnorm(x,0,1)
posy <- dlogspline(x, blogspl)
matplot(x, cbind(priy,posy), type="l",
        xlab="b", ylab="Prob Density", lwd=2)
legend(-0.2,1,legend=c("Prior","Posterior"), lty=1:2, col=1:2, lwd=2)
points(0, dnorm(0,0,1)); text(0.015, dnorm(0,0,1)+0.05, "p(b=0|H1)")
points(0, dlogspline(0, blogspl)); text(0.05, dlogspline(0, blogspl)-0.05, "p(b=0|y,H1)")
#dev.off()

SDT_ll <- function(d,B,h,f,sigtrials,noistrials){ #* \label{line:beginMaxLikH0}  *\#
  return(-2*(
    log(dbinom(h,sigtrials,pnorm(d/2-B)))+ 
  log(dbinom(f,noistrials,pnorm(-d/2-B)))
  ))
}

llgen <- optim(c(1,0),function(x) SDT_ll(x[1],x[2],h,f,sigtrials,noistrials))
llspec <- optim(1,function(x) SDT_ll(x[1],0,h,f,sigtrials,noistrials),
                method="Brent",lower=-5,upper=5) 
chi2diff <- llspec$value - llgen$value
print(chi2diff);print(1-pchisq(chi2diff,1)) #* \label{line:endMaxLikH0}  *\#
