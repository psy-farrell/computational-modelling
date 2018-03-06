library(ggplot2)

logistic_act <- function(x){
  return(1/(1+exp(-x)))
}

nStem <- 10
nEnd <- 20
nIn <- nStem + nEnd
nHid <- 15
nOut <- 30

nSets <- 5
nReg <- 4
nIrreg <- 1
nPatterns <- nSets*(nReg + nIrreg)

inputs <- {}
outputs <- {}

nTrain <- 6000

eta <- 0.1
m <- 0.9


for (tset in 1:nSets){
  
  stem <- rbinom(nStem,1,0.5)
  tout <- rbinom(nOut,1,0.5)
  
  # regular
  for (w in 1:nReg){
    inputs <- rbind(inputs, c(stem, rbinom(nEnd,1,0.5)))
    outputs <- rbind(outputs,tout)
  }
  
  # irregular
  inputs <- rbind(inputs, c(stem, rbinom(nEnd,1,0.5)))
  outputs <- rbind(outputs, rbinom(nOut,1,0.5))
}

Wih <- matrix(rnorm(nIn*nHid)*.01, nrow=nHid)
Who <- matrix(rnorm(nHid*nOut)*.01, nrow=nOut)

Bh <- rep(.01, nHid)
Bo <- rep(.01, nOut)

dWho <- Who*0
dWih <- Wih*0

toTrain <- 1:nPatterns

error <- rep(0,nTrain)
patterr <- matrix(rep(NaN,nTrain*nPatterns), nrow=nPatterns)
patts <- rep(0, nTrain)

for (sweep in 1:nTrain){
  
  # which item to train?
  i <- sample(toTrain,1)
  cue <- as.numeric(inputs[i,])
  target <- as.numeric(outputs[i,])
  
  ## Cue the network
  net <- Wih %*% cue
  act_hid <- logistic_act(net + Bh)
  
  net <- Who %*% act_hid
  act_out <- logistic_act(net + Bo)
  
  # score up performance
  patterr[i,sweep] <- sqrt(mean((target-act_out)^2))
  error[sweep] <- patterr[i,sweep]
  patts[sweep] <- i
  
  # update hidden-output weights
  d_out <- (target-act_out)*act_out*(1-act_out) #* \label{line:deltalog}  *\#
  dWho <- eta * d_out%*%t(act_hid) + m*dWho #* \label{line:logisticlearn}  *\#
  
  # Backpropagation: update input--hidden weights
  d_hid <- t(Who)%*%d_out * act_hid*(1-act_hid) 
  dWih <- eta * d_hid%*%t(cue) + m*dWih
  
  # update weights
  Who <- Who + dWho #* \label{line:Bpupdateweights}  *\#
  Wih <- Wih + dWih
  Bo <- Bo + eta*d_out
  Bh <- Bh + eta*d_hid
  
}

# Another
regu <- rep('reg',nTrain)
regu[patts%%5==0] <- 'irreg'
df <- data.frame(error=error, trial=1:nTrain, regu=factor(regu))

# pdf(file = "BPreg.pdf", width = 7, height=5)
# 
ggplot(df, aes(x=trial, y=error, colour=regu)) + 
  geom_point(size=2) + 
  xlab("Trial") + ylab("Error") + labs(colour="Regularity") +
  scale_colour_grey(start=0.5, end=0) +
  theme(legend.position=c(0.8,0.8))
# 
# dev.off()



# test the network

hidState <- {}

for (i in 1:nPatterns){
  
  cue <- as.numeric(inputs[i,])
  net <- Wih %*% cue
  act_hid <- logistic_act(net + Bh)
  
  net <- Who %*% act_hid
  act_out <- logistic_act(net + Bo)
  
  hidState <- cbind(hidState,act_hid)
}

d <- matrix(rep(0,nPatterns*nPatterns),nPatterns)
for (i in 1:25){
  for (j in 1:25){
    d[i,j]<- sqrt(mean((hidState[,i]-hidState[,j])^2))
  }
}