#gibbs sampler

require(mvtnorm)
require(MASS)

nsamples <- 1000  
rho <- .8
mux  <- muy <- 0
sigx <- 1
sigy <- .5
sigma <- matrix(c(sigx^2,rho*sigx*sigy,rho*sigy*sigx,sigy^2),
                nrow=2)  
#draw contour plot of known distribution
fiftyticks <- seq(from=-3, to =3, length.out=50) 
y<-rep(fiftyticks,50)
x<-rep(fiftyticks,each=50)
z<-matrix( dmvnorm(cbind(y,x),c(mux,muy),sigma),50,50)
contour(list(x=fiftyticks,y=fiftyticks,z=z),
        ylim=c(-3,3),xlim=c(-3,3),drawlabels=FALSE) 

#gibbs sampling
sxt1mr <- sqrt(sigx^2*(1-rho^2)) 
syt1mr <- sqrt(sigy^2*(1-rho^2))
rxy <- rho*(sigx/sigy)
ryx <- rho*(sigy/sigx)             
xsamp <- ysamp <- rep(0,nsamples)
xsamp[1] <- -2   
ysamp[1] <- 2   
for (i in c(1:(nsamples-1))) { 
    xsamp[i+1] <- rnorm(1, mean=rxy*ysamp[i], sd=sxt1mr)  
    ysamp[i+1] <- rnorm(1, mean=ryx*xsamp[i+1], sd=syt1mr)  
}
points(xsamp[-c(1:500)],ysamp[-c(1:500)],pch=21,bg="red")
for (j in c(1:5)){
  points(xsamp[j],ysamp[j]-.005,pch=21,cex=3.5,bg="white")
  text(xsamp[j],ysamp[j],as.character(j))
}
cor.test(xsamp,ysamp)
sd(xsamp)
sd(ysamp)

bivn<-rmvnorm(1000,rep(0,2),sigma)
#some checks
apply(bivn,2,mean)
apply(bivn,2,sd)
cor.test(bivn[,1],bivn[,2])


