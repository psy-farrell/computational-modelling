#random walk model 
nreps <- 10000
nsamples <- 2000

drift <- 0.03  # 0 = noninformative stimulus; >0 = informative
sdrw <- 0.3
criterion <- 3 

latencies <- rep(0,nreps)  
responses <- rep(0,nreps)
evidence <- matrix(0, nreps, nsamples+1) 
for (i in c(1:nreps)) { 
  evidence[i,] <- cumsum(c(0,rnorm(nsamples,drift,sdrw)))  
  p <-  which(abs(evidence[i,])>criterion)[1]
  responses[i] <- sign(evidence[i,p])
  latencies[i]  <- p
}

#plot up to 5 random walk paths
tbpn <- min(nreps,5)
plot(1:max(latencies[1:tbpn])+10,type="n",las=1,
     ylim=c(-criterion-.5,criterion+.5),
     ylab="Evidence",xlab="Decision time")
for (i in c(1:tbpn)) {
  lines(evidence[i,1:(latencies[i]-1)])   
}
abline(h=c(criterion,-criterion),lty="dashed")  

#plot histograms of latencies
par(mfrow=c(2,1))
toprt <- latencies[responses>0]
topprop <- length(toprt)/nreps
hist(toprt,col="gray",
     xlab="Decision time", xlim=c(0,max(latencies)),
     main=paste("Top responses (",as.numeric(topprop),
          ") m=",as.character(signif(mean(toprt),4)),
          sep=""),las=1)
botrt <- latencies[responses<0]
botprop <- length(botrt)/nreps
hist(botrt,col="gray",
     xlab="Decision time",xlim=c(0,max(latencies)),
     main=paste("Bottom responses (",as.numeric(botprop),
          ") m=",as.character(signif(mean(botrt),4)),
          sep=""),las=1)




