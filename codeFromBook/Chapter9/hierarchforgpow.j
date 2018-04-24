# hierarchical power forgetting model
model{
  # Priors for parent Distributions
  mubeta   ~ dunif(0,1)
  taubeta  ~ dgamma(epsilon + 0.1, epsilon) 
  mua      ~ dunif(0,1)
  taua     ~ dgamma(epsilon + 0.1, epsilon) 
  mub      ~ dunif(0,1)
  taub     ~ dgamma(epsilon + 0.1, epsilon) 
  
  # individual sampled parameters
  for (i in 1:ns){
    beta[i]  ~ dnorm(mubeta,taubeta)T(0,1)  
	a[i]     ~ dnorm(mua,taua)T(0,1)
	b[i]     ~ dnorm(mub,taub)T(0,1)  
  }
  
  # predictions for each subject at each lag
  for (i in 1:ns){
    for (j in 1:nt){
	    theta[i,j] <- a[i]+(1-a[i])*b[i]*pow((t[j]+1),-beta[i]) 
    }
  }

  # observed data   
  for (i in 1:ns){
    for (j in 1:nt){
      k[i,j] ~ dbin(theta[i,j],n) 
     }
  }
}