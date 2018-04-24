#Gaussian
model {
    #model the data
	for (i in 1:N) { 
		xx[i] ~ dnorm(mu, tau)
	}
	#priors for parameters
	mu ~ dunif(-100,100)  
	tau <- pow(sigma, -2) 
	sigma ~ dunif(0, 100)
}

 