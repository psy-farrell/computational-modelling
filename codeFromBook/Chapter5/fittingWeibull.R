#dat <- read.csv(file="rt_data.csv") # uncomment this if you read in data
nsubj <- 30
nobs <- 20
q_p <- c(.1,.3,.5,.7,.9)

shift <- rnorm(nsubj,250,50)
scale <- rnorm(nsubj,200,50)
shape <- rnorm(nsubj,2,0.25)

params <- rbind(shift,scale,shape)

print(rowMeans(params))

# rows are participants, columns are observations
dat <- apply(params, 2, function(x) rweibull(nobs,shape=x[3],scale=x[2])+x[1]) #* \label{line:MultipleParticipants:gendata}  *\#

# calculate sample quantiles for each particpant
kk <- apply(dat, 2, function(x) quantile(x, probs=q_p))

## FITTING VIA QUANTILE AVERAGING
# average the quantiles
vinq <- rowMeans(kk)

# fit the shifted Weibull to averaged quantiles
weib_qdev <- function(x,q_emp, q_p){
  if (any(x<=0)){
    return(10000000)
  }
  q_pred <- qweibull(q_p,shape=x[3],scale=x[2])+x[1] #* \label{line:MultipleParticipants:qweibull}  *\#
  dev <- sqrt(mean((q_pred-q_emp)^2))
}

res <- optim(c(225,225,1),  #* \label{line:MultipleParticipants:qfit}  *\#
             function(x) weib_qdev(x, vinq, q_p))

print(res)

## FITTING INDIVIDUAL PARTICIPANTS
weib_deviance <- function(x,rts){ #* \label{line:MultipleParticipants:weib_deviance}  *\#
  if (any(x<=0) || any(rts<x[1])){
    return(10000000)
  }
  likel <- dweibull(rts-x[1],shape=x[3],scale=x[2])
  dev <- sum(-2*log(likel))
}

res <- apply(dat,2,function(a) optim(c(100,225,1),#* \label{line:MultipleParticipants:run_deviance}  *\#
             function(x) weib_deviance(x, a)))

# Extract parameter estimates and put in to a matrix
parest <- matrix(
  unlist(lapply(res, function(x) x$par)),
  ncol=3, byrow=T)

print(colMeans(parest)) # mean parameter estimates
print(apply(parest,2,sd)) # SD of estimates

# note correlations between parameter estimates