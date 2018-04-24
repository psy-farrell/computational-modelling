require(lme4)  
n <- 10  
sigtrials <- noistrials <- 100   
ntrials <- sigtrials + noistrials
h <- rbinom(n,sigtrials, .60) 
f <- rbinom(n,noistrials,.11)     

subj <- rep(c(1:n),each=ntrials)  
stim <- rep(c(rep(1,sigtrials),rep(0,noistrials)),n)
resp <- as.vector( vapply(h,FUN=function(x) 
                            as.integer(c(rep(1,x),
                                         rep(0,ntrials-x))),
                                         integer(ntrials)) 
                            +
                   vapply(f,FUN=function(x) 
                            as.integer(c(rep(0,sigtrials),
                                         rep(1,x),rep(0,noistrials-x))),
                                         integer(ntrials))  )

#model with intercept = z(FA) default
mlhierarchSDT <- glmer(resp ~ stim + (1+stim|subj), family=binomial(probit)) 
summary(mlhierarchSDT)

#reparameterize so intercept = c
reparmstim <- cbind(-1,stim)
colnames(reparmstim) <- c("_c", "_d'")
mlhierarchSDTc <- glmer(resp ~ reparmstim-1 + (1+stim|subj), family=binomial(probit))
summary(mlhierarchSDTc)

#reparameterize so b is not highly correlated with d'
rmstim <- stim-.5 
reparmstim <- cbind(-1,rmstim)   
colnames(reparmstim) <- c("_b", "_d'")
mlhierarchSDTrp <- glmer(resp ~ reparmstim-1 + (1+rmstim|subj), family=binomial(probit))
summary(mlhierarchSDTrp)




