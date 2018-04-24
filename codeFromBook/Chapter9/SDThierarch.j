# Hierarchical Signal Detection Theory
model{
	# parent distributions for priors
	mud ~ dnorm(0,epsilon) 
	mub ~ dnorm(0,epsilon)
	taud ~ dgamma(epsilon,epsilon)
	taub ~ dgamma(epsilon,epsilon)  
	
	#modeling all n subjects
	for (i in 1:n) { 
		# priors for discriminability and bias
		d[i] ~ dnorm(mud,taud) 
		b[i] ~ dnorm(mub,taub)
	
		# predictions for hits and false alarms
		phih[i] <- phi( d[i]/2 - b[i])   
		phif[i] <- phi(-d[i]/2 - b[i])
	
		# Observed hits and false alarms
		h[i] ~ dbin(phih[i],sigtrials) 
		f[i] ~ dbin(phif[i],noistrials)
	}
}