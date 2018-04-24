library(rjags)

# initialize the data 
consistent    <- c( 78, 70, 7, 15) 
inconsistent  <- c(102, 55, 40, 53)
neutral       <- c( 63, 45, 13, 21)
Nsubj         <- c(170, 250, 142)

#define JAGS model
noconflict <- jags.model("wagenaar.j", 
                         data = list("Nsubj"=Nsubj, 
                                     "consistent"=consistent, 
                                     "inconsistent"=inconsistent, 
                                     "neutral"=neutral),
                         n.chains=3)  
# burnin
update(noconflict,n.iter=1000)  
# perform MCMC
parms4j <- c("p", "q", "c","predprob") 
mcmcfin<-coda.samples(noconflict,parms4j,5000) 

summary(mcmcfin)
plot(mcmcfin)
acfplot(mcmcfin)
gelman.plot(mcmcfin)

#Listing 8.10 from here on
allpost <- function(mcmcfin,pn) {
  return (unlist(lapply(mcmcfin,FUN=function(x) c(x[,pn]))))
}
mean(allpost(mcmcfin,"c")>.5)
mean(allpost(mcmcfin,"p")>.5)
mean(allpost(mcmcfin,"q")>.5)
