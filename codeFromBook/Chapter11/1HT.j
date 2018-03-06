# High-threshold model
model{
	# priors for MPT parameters
    th1 ~ dbeta(1,1) #* \label{line:BayesianJAGS:1ht1}  *\#
    th2 ~ dbeta(1,1) #* \label{line:BayesianJAGS:1ht1b}  *\#
	
    # predictions for responses
    predh <- th1+(1-th1)*th2    #* \label{line:BayesianJAGS:1ht2}  *\#
    predf <- th2				#* \label{line:BayesianJAGS:1ht3}  *\#
	
    # Observed responses
    h ~ dbin(predh,sigtrials) #* \label{line:BayesianJAGS:1ht3}  *\#
    f ~ dbin(predf,noistrials)
}