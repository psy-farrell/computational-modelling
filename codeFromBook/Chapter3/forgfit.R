rm(list=ls())

#discrepancy for power forgetting function 
powdiscrep <- function (parms,rec,ri) {                    
  if (any(parms<0)||any(parms>1)) return(1e6)
  pow_pred <- parms["a"] *(parms["b"]*ri + 1)^(-parms["c"])
  return(sqrt( sum((pow_pred-rec)^2)/length(ri) ))
}                                                          

#Carpenter et al. (2008) Experiment 1
rec <- c(.93,.88,.86,.66,.47,.34)
ri  <- c(.0035, 1, 2, 7, 14, 42)

#initialize starting values 
sparms <-c(1,.05,.7)
names(sparms) <- c("a","b","c")
#obtain best-fitting estimates
pout <- optim(sparms,powdiscrep,rec=rec,ri=ri)   
pow_pred <- pout$par["a"] *(pout$par["b"]*c(0:max(ri)) + 1)^(-pout$par["c"])  

#plot data and best-fitting predictions
x11()
par(cex.axis=1.2,cex.lab=1.4)
par(mar=(c(5, 5, 3, 2) + 0.1),las=1)
plot(ri,rec, 
     xlab = "Retention Interval (Days)", 
     ylab = "Proportion Items Retained",  
     ylim=c(0.3,1),xlim=c(0,43),xaxt="n",type="n")
lines(c(0:max(ri)),pow_pred,lwd=2)
points(ri,rec,pch=21, bg="dark grey",cex=2)
dev <- pow_pred[ri+1]
for (x in c(1:length(ri))) {
  lines(c(ri[x],ri[x]),c(dev[x],rec[x]),lwd=1)
  }
axis(1,at=c(0:43))

#perform bootstrapping analysis
ns  <- 55
nbs <- 1000
bsparms <- matrix(NA,nbs,length(sparms))
bspow_pred <- pout$par["a"] *(pout$par["b"]*ri + 1)^(-pout$par["c"])
for (i in c(1:nbs)) {   
    recsynth     <- vapply(bspow_pred, FUN=function(x) mean(rbinom(ns,1,x)), numeric(1))
    bsparms[i,]  <- unlist(optim(pout$par,powdiscrep,rec=recsynth,ri=ri)$par)
}

#function to plot a histogram
histoplot<-function(x,l4x) {
    hist(x,xlab=l4x,main="",xlim=c(0,1),cex.lab=1.5,cex.axis=1.5)
    lq <- quantile(x,0.025)
    abline(v=lq,lty="dashed",lwd=2)
    uq <- quantile(x,0.975)
    abline(v=uq,lty="dashed",lwd=2)
    return(c(lq,uq))
}
x11(5,2)
par(mfcol=c(1,3),las=1)
for (i in c(1:dim(bsparms)[2])) {
    print(histoplot(bsparms[,i],names(sparms)[i]))
}
