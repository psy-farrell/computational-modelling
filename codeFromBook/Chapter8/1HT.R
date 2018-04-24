library(rjags)
#provide data from experiment
h <- 60          
f <- 11
sigtrials <- noistrials <- 100   

#define JAGS model
onehtj <- jags.model("1HT.j", 
                   data = list("h"=h, "f"=f, 
                               "sigtrials"=sigtrials,
                               "noistrials"=noistrials),
                    n.chains=4)  
# burnin
update(onehtj,n.iter=1000)  
# perform MCMC
parameters <- c("th1", "th2", "predh", "predf")
mcmcfin<-coda.samples(onehtj,parameters,5000)  

summary(mcmcfin)
x11()
plot(mcmcfin)
x11()
acfplot(mcmcfin)
gelman.plot(mcmcfin)






