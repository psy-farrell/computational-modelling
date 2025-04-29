library("ggplot2")
# generate some data
set.seed(1540614451)

N <- 1000
pShort <- 0.3

genpars <- list(c(100,10),
                c(150,20))

# we assume equal sampling probability for the three distributions
whichD <- sample(c(1,2),N, replace=TRUE, prob=c(pShort, 1-pShort))

dat <- sapply(whichD, function(x) 
  rnorm(1,genpars[[x]][1],genpars[[x]][2]))

# function needed in EM
weighted.sd <- function(x,w,mu=mean(x)){
  wvar <- sum(w*(x-mu)^2)/
    sum(w)
  return(sqrt(wvar))
}

# guess parameters
mu1 <- mean(dat,1)*0.8
mu2 <- mean(dat, 1)*1.2
sd1 <- sd(dat)
sd2 <- sd(dat)
ppi <- 0.5
oldppi <- 0

while (abs(ppi-oldppi)>.00001){ #* \label{line:MultipleParticipants:gmmloop}  *\#
  
  oldppi <- ppi
  
  # E step
  resp <- ppi*dnorm(dat,mu2,sd2)/
    ((1-ppi)*dnorm(dat,mu1,sd1) + ppi*dnorm(dat,mu2,sd2))
  
  # M step
  mu1 <- weighted.mean(dat,1-resp)
  mu2 <- weighted.mean(dat,resp)
  
  sd1 <- weighted.sd(dat,1-resp,mu1)
  sd2 <- weighted.sd(dat,resp,mu2)
  
  ppi <- mean(resp)
  print(ppi)
  
}

df <- data.frame(rt=dat)

#pdf(file="GMMexample.pdf", width=5, height=4)
ggplot(df, aes(x = rt)) + 
  geom_histogram(aes(y = ..density..),colour = "black", fill = "white", 
                 binwidth = 3) + 
  stat_function(fun = function(k) (1-ppi)*dnorm(k,mu1,sd1)) +
  stat_function(fun = function(k) ppi*dnorm(k,mu2,sd2)) +
  xlab("RT (ms)") + ylab("Density")
#dev.off()

# mixtools for comparison

library(mixtools) # you'll need to install this library
myEM <- normalmixEM( dat, mu = c(1,4),
                     sigma=c(sd(dat),sd(dat)))