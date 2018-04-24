model{
    # Group-level hyperpriors
    # k (steepness of hyperbolic discounting)
    groupkmu        ~ dnorm(0, 1/100)
    groupksigma     ~ dunif(0, 100)

    # comparison acuity (alpha)
    groupALPHAmu        ~ dunif(0,5)
    groupALPHAsigma     ~ dunif(0,5)

    # Participant-level parameters
    for (p in 1:nsubj){
      k[p]        ~ dnorm(groupkmu, 1/(groupksigma^2)) T(0,)
      alpha[p]    ~ dnorm(groupALPHAmu, 1/(groupALPHAsigma^2)) T(0,)

      for (t in 1:T) {
        # calculate present subjective value for each reward
        VA[p,t] <- A[p,t] / (1+k[p]*DA[p,t])  
        VB[p,t] <- B[p,t] / (1+k[p]*DB[p,t])  

        # Psychometric function yields predicted choice
        P[p,t] <-  phi( (VB[p,t]-VA[p,t]) / alpha[p] )  

        # Observed responses 
        R[p,t] ~ dbern(P[p,t]) 
      }
    }
}