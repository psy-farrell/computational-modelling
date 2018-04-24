library(rtdists)

#generate RTs from the diffusion model as data
v  <- c(.042,.079,.133,.227,.291,.369) 
a  <- .11
z  <- 0.5*a
d  <- 0
sz <- 0 
t0 <- 0.3
st0<- 0.2
sv <- 0
npc <- 1000       #n per condition
nv  <- length(v)  #n conditions                 
set.seed(8)       # for reproducibility  
movedata <- NULL     

qtiles <- seq(from=.1, to=.9, by=.2)
forqpfplot <- matrix(0,length(qtiles),nv*2)
pLow <- pUp <- rep(0,nv)
for (i in c(1:nv)) {
  rt41cond <- rdiffusion(npc,a=a,v=v[i],t0=t0,z=z,d=d,sz=sz,sv=sv,st0=st0,s=.1, precision=3)  
  movedata <- rbind(movedata,rt41cond) 
  
  pLow[i] <- sum(rt41cond$response=="lower")/npc
  pUp[i]  <- sum(rt41cond$response=="upper")/npc
  forqpfplot[,nv+1-i] <- quantile(rt41cond$rt[rt41cond$response=="lower"],qtiles)*1000 
  forqpfplot[,i+nv]   <- quantile(rt41cond$rt[rt41cond$response=="upper"],qtiles)*1000 
}

#plot the synthetic data in QPFs
x11()
plot(0,0,type="n",las=1,
     ylim=c(0,max(forqpfplot)+200),xlim=c(0,1),
     xlab="Response proportion",
     ylab="RT quantile (ms)")
apply(forqpfplot,1, FUN=function(x) points(c(rev(pLow),pUp),x,pch=4) )


#function returns -loglikelihood of predictions
diffusionloglik2 <- function(pars, rt, response) 
{
  if (any(pars<0)) return(1e6+1e3*rnorm(1))
  ptrs <- grep("v[1-9]",names(pars))
  eachn <- length(rt)/length(ptrs)
  likelihoods <- NULL
  for (i in c(1:length(ptrs))) {
    likelihoods <- c(likelihoods,
                     tryCatch(ddiffusion(rt[((i-1)*eachn+1):(i*eachn)], 
                                         response=response[((i-1)*eachn+1):(i*eachn)],                 
                                         a=pars["a"], 
                                         v=pars[ptrs[i]], 
                                         t0=pars["t0"], 
                                         z=0.5*pars["a"],d=0, 
                                         sz=pars["sz"], 
                                         st0=pars["st0"], 
                                         sv=pars["sv"],s=.1,precision=3),
                              error = function(e) 0))
  }
  if (any(likelihoods==0)) return(1e6+1e3*rnorm(1))
  return(-sum(log(likelihoods)))
} 

#generate starting values for parameters
sparms <- c(runif(1, 0.1, 0.2),   
           v+rnorm(length(v),0,.05),
           0.3, 
           0.05, 
           runif(1, 0, .2),
           0.1)
names(sparms) <- c("a", paste("v",1:nv,sep=""), "t0", "sz", "st0", "sv")  
#now estimate the parameters
fit2rts <- optim(sparms, diffusionloglik2, gr = NULL, 
                 rt=movedata$rt, response=movedata$response)
round(fit2rts$par, 3)


#now obtain predictions from model for plotting
#first the maximum proportion
vfitted <- fit2rts$par[paste("v",1:nv,sep="")]
maxpUp <- pdiffusion(rep(Inf, length(vfitted)), response="upper", 
                            a=fit2rts$par["a"], 
                            v=vfitted,
                            t0=fit2rts$par["t0"],
                            z=0.5*fit2rts$par["a"],d=0,
                            sz=fit2rts$par["sz"],
                            sv=fit2rts$par["sv"],
                            st0=fit2rts$par["st0"],s=.1,precision=3)
maxpLr <- pdiffusion(rep(Inf, length(vfitted)), response="lower", 
                     a=fit2rts$par["a"], 
                     v=vfitted,
                     t0=fit2rts$par["t0"],
                     z=0.5*fit2rts$par["a"],d=0,
                     sz=fit2rts$par["sz"],
                     sv=fit2rts$par["sv"],
                     st0=fit2rts$par["st0"],s=.1,precision=3)
#now RT quantiles
forqpfplot2 <- matrix(0,length(qtiles),nv*2)
for (i in c(1:nv)) {  
  forqpfplot2[,i]   <- qdiffusion(qtiles*maxpLr[nv+1-i], 
                                  response="lower",
                                  a=fit2rts$par["a"],
                                  v=vfitted[nv+1-i],
                                  t0=fit2rts$par["t0"],
                                  z=0.5*fit2rts$par["a"],d=0,
                                  sz=fit2rts$par["sz"],
                                  sv=fit2rts$par["sv"],
                                  st0=fit2rts$par["st0"],s=.1,precision=3)*1000
  forqpfplot2[,nv+i] <- qdiffusion(qtiles*maxpUp[i], 
                                   response="upper",
                                   a=fit2rts$par["a"],
                                   v=vfitted[i],
                                   t0=fit2rts$par["t0"],
                                   z=0.5*fit2rts$par["a"],d=0,
                                   sz=fit2rts$par["sz"],
                                   sv=fit2rts$par["sv"],
                                   st0=fit2rts$par["st0"],s=.1,precision=3)*1000
}                     
apply(forqpfplot2,1, FUN=function(x) lines(c(rev(maxpLr),maxpUp),x) )

