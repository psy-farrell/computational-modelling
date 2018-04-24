library(R.utils)
library(rjags) 
grabfun<-function(x,p,var) {return(x[x$subj==p,var])} 

itcdata<-read.table("hierarchicalITC.dat",header=TRUE)
subjects <- unique(itcdata$subj)  
ntrials  <- dim(itcdata)[1]/length(unique(itcdata$subj))
nsubj    <- length(unique(itcdata$subj)) 

delays4A  <- t(vapply(subjects,FUN=function(x) grabfun(itcdata,x,"DA"),integer(ntrials))) 
delays4B  <- t(vapply(subjects,FUN=function(x) grabfun(itcdata,x,"DB"),integer(ntrials)))
amounts4A <- t(vapply(subjects,FUN=function(x) grabfun(itcdata,x,"A"),integer(ntrials)))
amounts4B <- t(vapply(subjects,FUN=function(x) grabfun(itcdata,x,"B"),integer(ntrials)))
responses <- t(vapply(subjects,FUN=function(x) grabfun(itcdata,x,"R"),integer(ntrials)))

#initialize model for JAGS
hierITC <- jags.model("hierarchicalITC.j",  
                 data = list("nsubj"=nsubj,
							   "DA"=delays4A,
							   "DB"=delays4B,
							   "A"=amounts4A,
							   "B"=amounts4B,
							   "T"=ntrials,
                 "R"=responses),                   
                   n.chains=4)  
# burnin
update(hierITC,n.iter=1000)  
# perform MCMC
parameters <- c("k", "alpha", "groupkmu", "groupksigma", "groupALPHAmu", "groupALPHAsigma",
                "VA","VB","P","DB") 
mcmcfin<-coda.samples(hierITC,parameters,5000) 
summary(mcmcfin)
