#function returns -loglikelihood of predictions
diffusionloglik <- function(pars, rt, response) 
{
  likelihoods <- tryCatch(ddiffusion(rt, response=response,                 
                          a=pars["a"], 
                          v=pars["v"], 
                          t0=pars["t0"], 
                          z=0.5*pars["a"], 
                          sz=pars["sz"], 
                          st0=pars["st0"], 
                          sv=pars["sv"],s=.1,precision=1),
                        error = function(e) 0)      
  if (any(likelihoods==0)) return(1e6) 
  return(-sum(log(likelihoods)))
}  

library(rtdists)
#generate RTs from the diffusion model as data

genparms <- c(.1,.2,.5,.05,.2,.05)           
names(genparms) <- c("a", "v", "t0", "sz", "st0", "sv") 
rts <- rdiffusion(500, a=genparms["a"],
                       v=genparms["v"], 
                       t0=genparms["t0"], 
                       z=0.5*genparms["a"],d=0, 
                       sz=genparms["sz"], 
                       sv=genparms["sv"],  
                       st0=genparms["st0"],s=.1)   

#generate starting values for parameters     
sparms <- c(runif(1, 0.01, 0.4), 
           runif(1, 0.01, 0.5),
           0.3, 
           runif(1, 0.02, 0.08),
           runif(1, .1, .3),
           runif(1, 0, 0.1))
names(sparms) <- c("a", "v", "t0", "sz", "st0", "sv") 

#now estimate parameters
fit2rts <- optim(sparms, diffusionloglik, gr = NULL, 
               rt=rts$rt, response=rts$response)
round(fit2rts$par, 3)


