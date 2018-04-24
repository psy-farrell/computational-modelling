library(rjags)
epsilon <- .01
#simulate data for 4 subjects
tlags <- c(0, 1, 5, 10, 20, 50) 
nlags <- length(tlags)
nsubj <- 4
nitems <- 20
nrecalled <- matrix(0,nsubj,nlags)
for (i in c(1:nsubj)) {
  a     <- runif(1,.0,.2)
  b     <- runif(1,.9,1.0)
  beta <- runif(1,.1,.4)
  print(c(a,b,beta))                      
  for (j in c(1:nlags)) {
      p <- a + (1-a) * b * (tlags[j]+1)^(-beta) 
      nrecalled[i,j] <- rbinom(1,nitems,p) 
  }  
}                                           
#define model
forgpowjh <- jags.model("hierarchforgpow.j",  
                   data = list("epsilon"=epsilon,
                               "t"  = tlags,
                               "k"  = nrecalled,
                               "n"  = nitems,
                               "ns" = nsubj,
                               "nt" = nlags),
                   n.chains=1)   
# burnin
update(forgpowjh,n.iter=1000)  
# perform MCMC
parameters <- c("mubeta", "mua", "mub", 
                "taubeta", "taua", "taub", 
                "a", "b", "beta","theta")     
mcmcfin<-coda.samples(forgpowjh,parameters,5000) 
summary(mcmcfin)
