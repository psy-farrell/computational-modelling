#rm(list=ls()) 
# setwd("C:/Users/Lewan/Documents/Papers Written/Modeling Book/_Big Book/bigbook/BayesianJAGS")

#next line must be L5
library(rjags)
#provide data from experiment
h <- 60         
f <- 11
sigtrials <- noistrials <- 100  

#initialize for JAGS
oneinit <- list(d=0, b=0)   #
myinits <- list(oneinit)[rep(1,4)]
sdtj <- jags.model("SDT.j", 
                   data = list("h"=h, "f"=f, 
                               "sigtrials"=sigtrials,
                               "noistrials"=noistrials),
                   inits=myinits,
                   n.chains=4)
# burnin
update(sdtj,n.iter=1000)  
# perform MCMC
parameters <- c("d", "b", "phih", "phif")
mcmcfin<-coda.samples(sdtj,parameters,5000) 

# summary(mcmcfin)
# plot(mcmcfin)
# gelman.plot(mcmcfin)
# #previous line must be 28
# 
# #> install.packages("superdiag")
# #> library(superdiag)
# #> superdiag(mcmcfin, burnin = 100)
# 
# #for plotting (R2jags version only)
# d1 <- samples$BUGSoutput$sims.list$d[,1]
# b1 <- samples$BUGSoutput$sims.list$b[,1]
# h1 <- samples$BUGSoutput$sims.list$phih[,1]
# f1 <- samples$BUGSoutput$sims.list$phif[,1]
# 
# #make the four panel plot:
# x11()
# layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
# #layout.show(4)
# #some plotting options to make things look better:
# par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
#     font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
# # Discriminability panel:    
# plot(density(d1), lwd=2, col="red", main="", ylab="", xlab="", 
#      xlim=c(0,3), axes=F)
# axis(1)
# axis(2, labels=F, at=c(0,24))
# mtext("Probability Density", side=2, line = 1, cex=1., las=0)
# mtext("Discriminability (d)", side=1, line = 2.5, cex=1.)
# 
# # Bias panel:    
# plot(density(b1), lwd=2, col="red", main="", ylab="", xlab="", 
#      xlim=c(-1,1), axes=F)
# axis(1)
# axis(2, labels=F, at=c(0,24))
# mtext("Probability Density", side=2, line = 1, cex=1., las=0)
# mtext("Bias (b)", side=1, line = 2.5, cex=1.)
# 
# # Hit Rate panel:    
# plot(density(h1), lwd=2, col="red", main="", ylab="", xlab="", 
#      xlim=c(0,1), axes=F)
# axis(1)
# axis(2, labels=F, at=c(0,24))
# mtext("Probability Density", side=2, line = 1, cex=1., las=0)
# mtext(expression(paste("Hit Rate (",phi[h],")")), side=1, line = 2.5, cex=1.)
# 
# # False-Alarm Rate panel:    
# plot(density(f1), lwd=2, col="red", main="", ylab="", xlab="", 
#      xlim=c(0,1), axes=F)
# axis(1)
# axis(2, labels=F, at=c(0,24))
# mtext("Probability Density", side=2, line = 1, cex=1., las=0)
# mtext(expression(paste("False-Alarm Rate (",phi[f],")")), side=1, line = 2.5, cex=1.)
# 
# 

