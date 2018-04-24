library(rtdists)

#generate RTs from the LBA as data
v   <- c(.55,.65,.8,1.05) 
A   <- .7
b   <- .71
t0  <- .35
sv  <- .25
st0 <- 0
npc <- 1000        #n per condition
nv  <- length(v)  #n conditions                 

movedata <- NULL     

qtiles <- seq(from=.1, to=.9, by=.2)
forqpfplot <- matrix(0,length(qtiles),nv*2)
pLow <- pUp <- rep(0,nv)
for (i in c(1:length(v))) {
  rt41cond <- rLBA(npc, A, b, t0, mean_v=c(v[i],1-v[i]),sd_v=c(sv,sv))  
  movedata <- rbind(movedata,rt41cond)  
  
  pLow[i] <- sum(rt41cond$response==2)/npc
  pUp[i]  <- sum(rt41cond$response==1)/npc
  forqpfplot[,nv+1-i] <- quantile(rt41cond$rt[rt41cond$response==2],qtiles)*1000  
  forqpfplot[,i+nv]   <- quantile(rt41cond$rt[rt41cond$response==1],qtiles)*1000  
} 

#plot the synthetic data in QPFs
x11()
plot(0,0,type="n",las=1,
     ylim=c(0,max(forqpfplot)+200),xlim=c(0,1),
     xlab="Response proportion",
     ylab="RT quantile (ms)")
apply(forqpfplot,1, FUN=function(x) points(c(rev(pLow),pUp),x,pch=4) )

#function returns -loglikelihood of predictions
LBAloglik <- function(pars, rt, response) 
{
  if (any(pars<0)) return(1e6+1e3*rnorm(1))
  ptrs <- grep("v[1-9]",names(pars)) 
  eachn <- length(rt)/length(ptrs)
  likelihoods <- NULL
  for (i in c(1:length(ptrs))) {
    likelihoods <- c(likelihoods,
                     tryCatch(dLBA(rt[((i-1)*eachn+1):(i*eachn)],  
                                   response=response[((i-1)*eachn+1):(i*eachn)],                 
                                   A=pars["A"], 
                                   b=pars["b"],
                                   t0=pars["t0"], 
                                   mean_v=c(pars[ptrs[i]],1-pars[ptrs[i]]), 
                                   sd_v=c(pars["sv"],pars["sv"])),
                              error = function(e) 0))
  }
  if (any(likelihoods==0)) return(1e6+1e3*rnorm(1))
  return(-sum(log(likelihoods)))
} 

#generate starting values for parameters
sparms <- c(.7, .71, .35, v+rnorm(1,0,.05), .25)
names(sparms) <- c("A", "b", "t0", paste("v",1:nv,sep=""), "sv")  
#now estimate the parameters
fit2rts <- optim(sparms, LBAloglik, gr = NULL, rt=movedata$rt, response=movedata$response)
round(fit2rts$par, 3)

#now obtain predictions from model for plotting
#first the maximum proportion
vfitted <- fit2rts$par[paste("v",1:nv,sep="")]
maxpUp <- vapply(vfitted, FUN=function(x)
                 pLBA(100, response=2, 
                           A=fit2rts$par["A"],
                           b=fit2rts$par["b"], 
                           t0=fit2rts$par["t0"],
                           mean_v=c(x,1-x),
                           sd_v=c(fit2rts$par["sv"],fit2rts$par["sv"])),
                numeric(1))
maxpLr <- 1-maxpUp
#now RT quantiles
forqpfplot2 <- matrix(0,length(qtiles),nv*2)
for (i in c(1:nv)) {  
  forqpfplot2[,i]   <- qLBA(qtiles*maxpLr[nv+1-i], 
                            response=1,
                            A=fit2rts$par["A"],
                            b=fit2rts$par["b"], 
                            t0=fit2rts$par["t0"],
                            mean_v=c(vfitted[nv+1-i],1-vfitted[nv+1-i]),
                            sd_v=c(fit2rts$par["sv"],fit2rts$par["sv"]))*1000
  forqpfplot2[,nv+i] <- qLBA(qtiles*maxpUp[i], 
                             response=2,
                             A=fit2rts$par["A"],
                             b=fit2rts$par["b"], 
                             t0=fit2rts$par["t0"],
                             mean_v=c(vfitted[i],1-vfitted[i]),
                             sd_v=c(fit2rts$par["sv"],fit2rts$par["sv"]))*1000
}                     
apply(forqpfplot2,1, FUN=function(x) lines(c(rev(maxpLr),maxpUp),x) )


##########################
#sparms <-c(.11/ratcl2sing,v, 0.3,0,0.2,0)
diffusionloglik2 (sparms, movedata$rt, movedata$response) 

rt<-movedata$rt
boundary <-movedata$response
pars <- sparms
fit2rts <- nlminb(sparms, diffusionloglik2, lower = 0, 
                  rt=movedata$rt, boundary=movedata$response)







#above is last line

genparms

hist(rts$rt[rts$response=="upper"])
hist(rts$rt[rts$response=="lower"])


#plot predictions from 
maxpUp <- pdiffusion(20,a=fit2rts$par["a"],v=fit2rts$par["v"],t0=fit2rts$par["t0"],
                         z=fit2rts$par["a"]/2,d=0,sz=fit2rts$par["sz"],sv=fit2rts$par["sv"],
                         st0=fit2rts$par["st0"],boundary="upper",precision=2.1)
maxpLr <- 1-maxpUp
qtiles <- seq(from=.1, to=.9, by=.2)
forqpfplotlower <- qdiffusion(qtiles*maxpLr, 
                              boundary="lower",
                              a=fit2rts$par["a"],v=fit2rts$par["v"],t0=fit2rts$par["t0"],
                              z=fit2rts$par["a"]/2,d=0,sz=fit2rts$par["sz"],sv=fit2rts$par["sv"],
                              st0=fit2rts$par["st0"],
                              precision=2.8)*1000
forqpfplotupper <- qdiffusion(qtiles*maxpUp, 
                              boundary="upper",
                              a=fit2rts$par["a"],v=fit2rts$par["v"],t0=fit2rts$par["t0"],
                              z=fit2rts$par["a"]/2,d=0,sz=fit2rts$par["sz"],sv=fit2rts$par["sv"],
                              st0=fit2rts$par["st0"],
                              precision=2.)*1000



## End(Not run)
recov <- nlminb(sparms, diffusionloglik, lower = 0, rt=rts$rt, boundary=rts$response)
round(recov$par, 3)

sparms<-c(runif(2, 0.5, 3), 0.1, runif(3, 0, 0.5))

# plot density:
curve(ddiffusion(x, a=1, v=2, t0=0.5, boundary = "upper"), 
      xlim=c(0,3), main="Density of upper responses", ylab="density", xlab="response time")
curve(ddiffusion(x, a=1, v=2, t0=0.5, st0=0.2, boundary = "upper"), 
      add=TRUE, lty = 2)
legend("topright", legend=c("no", "yes"), title = "Starting Point Variability?", lty = 1:2)

# plot cdf:
curve(pdiffusion(x, a=1, v=2, t0=0.5, st0=0.2, boundary="u"), 
      xlim = c(0, 3),ylim = c(0,1), 
      ylab = "cumulative probability", xlab = "response time",
      main = "CDF of diffusion model with start point variability")
curve(pdiffusion(x, a=1, v=2, t0=0.5, st0=0.2, boundary="l"), 
      add=TRUE, lty = 2)
legend("topleft", legend=c("upper", "lower"), title="boundary", lty=1:2)


# get density for random RTs:
sum(log(ddiffusion(rt1$rt, rt1$response, a=1, v=2, t0=0.5)))  # boundary is factor
sum(log(ddiffusion(rt1$rt, as.numeric(rt1$response), a=1, v=2, t0=0.5))) # boundary is numeric
sum(log(ddiffusion(rt1$rt, as.character(rt1$response), a=1, v=2, t0=0.5))) # boundary is character

sum(log(ddiffusion(rt2$rt, rt2$response, a=1, v=2, t0=0.5)))
