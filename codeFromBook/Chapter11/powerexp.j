model{
  # model node
  M  ~ dcat(p[]);
  p[1] <- prior1; p[2] <- 1-p[1]
  pM2 <- step(M-1.5)
  
  # Likelihoods
  for (j in 1:nt){
      theta[1,j] <- a1+(1-a1)*b1*exp(-alpha*t[j])
      theta[2,j] <- a2+(1-a2)*b2*pow((t[j]+1),-beta)
      k[j] ~ dbin(theta[M,j],n) #* \label{line:PSbinom}  *\#
  }
  
  ## -------- Model 1 (exp) priors
  a1 ~ dbeta(a1.s1[M],a1.s2[M])T(0,0.2)
  b1 ~ dbeta(b1.s1[M],b1.s2[M])
  alpha ~ dbeta(alpha.s1[M],alpha.s2[M])
  
  # --------Model 2 (power) priors
  a2 ~ dbeta(a2.s1[M],a2.s2[M])T(0,0.2)
  b2 ~ dbeta(b2.s1[M],b2.s2[M])
  beta ~ dbeta(beta.s1[M],beta.s2[M])
  
}