# High-threshold model
model{
	# priors for MPT parameters
    th1 ~ dbeta(1,1) 
    th2 ~ dbeta(1,1) 
	
    # predictions for responses
    predh <- th1+(1-th1)*th2    
    predf <- th2				
	
    # Observed responses
    h ~ dbin(predh,sigtrials)
    f ~ dbin(predf,noistrials)
}