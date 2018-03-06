# Signal Detection Theory
model{
	# priors for discriminability and bias
    d ~ dnorm(1,1) #* \label{line:BayesianJAGS:sdt1}  *\#
    b ~ dnorm(0,1)
	
    # express as areas under curves
    phih <- phi(d/2-b)   #normal cdf  #* \label{line:BayesianJAGS:sdt2}  *\#
    phif <- phi(-d/2-b)
	
    # Observed hits and false alarms
    h ~ dbin(phih,sigtrials)  #* \label{line:BayesianJAGS:sdt3}  *\#
    f ~ dbin(phif,noistrials)
}