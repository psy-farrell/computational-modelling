#use priors in MH algorithm for a normal distribution

######### this first version uses densities (dnorm)
plausrange <- c(200,600)
chain <- rep(0,5000)      
burnin<-200
set.seed(1234)
propsd <- 5     #tuning parameter  

p2f <- FALSE # do we plot to file?

obs <- 415               
obssd <- 20   
priormu <- 326
priorsd <- 88           

chain[1] <- 500  #starting value  
for (i in c(2:length(chain))) { 
    current <- chain[i-1]
    proposal <- current + rnorm(1,0,propsd)
    if((dnorm(obs,proposal,obssd)*       
        dnorm(proposal,priormu,priorsd)) > 
        (dnorm(obs,current,obssd)*
        dnorm(current,priormu,priorsd)) ) {
      chain[i] <- proposal  #accept proposal  
    } else {                 
       llratio <- (dnorm(obs,proposal,obssd)*          
                   dnorm(proposal,priormu,priorsd)) / 
                  (dnorm(obs,current,obssd)*
                   dnorm(current,priormu,priorsd))
       chain[i] <- ifelse(runif(1) < llratio,proposal,current)
    }
}  


print(c(mean(chain),var(chain)))
if (p2f) {
  pdf(file=paste("npostwpriorsd",as.character(priorsd),".pdf",sep=""),height=5,width=5)
} else {x11(5,5)}
par(mar=c(4, 3, 1, 0.5))
plot(density(chain),las=1,xlab=bquote("Sampled values of "*mu),
     yaxt="n",lwd=2,lty="dashed",
     main="",xlim=plausrange,ylab="",
     ylim=c(0,max(max(density(chain)$y),
                  max(density(chain[-c(1:burnin)])$y),
                  max(dnorm(min(plausrange):max(plausrange),obs,obssd)))*1.4))
lines(min(plausrange):max(plausrange),
      dnorm(min(plausrange):max(plausrange),obs,obssd),
      col="gray",lwd=5)
lines(min(plausrange):max(plausrange),
      dnorm(min(plausrange):max(plausrange),priormu,priorsd),
      col="gray",lwd=4,lty="dotdash")
lines(density(chain[-c(1:burnin)]),lwd=2,lty="solid")
mtext("   Density",2,1)
legend("topright",inset=.02,c("Normal PDF","All MCMC","Excluding burnin","Prior PDF"),
       lty=c("solid","dashed","solid","dotdash"),col=c("gray","black","black","gray"),lwd=c(4,2,2,4))
if (p2f) {dev.off()}

x11() #caterpillar plot
plot(chain,type="l",las=1,xlab="Iteration",ylab="Value of accepted sample")
lines(1:burnin,chain[1:burnin],col="red")




############## below here deal with likelihood function not density
#normLik    <- function(precision,mu,x) { return(exp(-0.5*precision*(mu^2-2*x*mu))) }
normlogLik <- function(precision,mu,x) { return((-0.5*precision*(mu^2-2*x*mu))) } #* \label{line:BayesianMCMC:mhploglik}  *\#

obs <- 415                         #* \label{line:BayesianMCMC:mhpinit1}  *\#
obsprecision <- .0025   
priormu <- 326
priorprecision <- .001293           #* \label{line:BayesianMCMC:mhpinit1}  *\#
set.seed(1234)
chain[1] <- 500  #starting value  
for (i in c(2:length(chain))) {  #* \label{line:BayesianMCMC:mhploop}  *\#
  current <- chain[i-1]
  proposal <- current + rnorm(1,0,propsd)
  if (normlogLik(obsprecision,proposal,obs)+                  #* \label{line:BayesianMCMC:mhpnewcomp}  *\#
      normlogLik(priorprecision,proposal,priormu) > 
      normlogLik(obsprecision,current,obs)+
      normlogLik(priorprecision,current,priormu)) {
    chain[i] <- proposal  #accept proposal  
  } else {
    llratio <- exp((normlogLik(obsprecision,proposal,obs)+   #* \label{line:BayesianMCMC:mhpllratio}  *\#
                      normlogLik(priorprecision,proposal,priormu))-  
                     (normlogLik(obsprecision,current,obs)+
                        normlogLik(priorprecision,current,priormu)))
    chain[i] <- ifelse(runif(1) < llratio,proposal,current)
  }
} 