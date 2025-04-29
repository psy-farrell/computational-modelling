source("GCMpred.R")

N <- 2*80 # there were 2 responses per face from 80 ppl 
N_A <- round(N*.968) #N_B is implicitly N - N_A
  
c <- 4
w <- c(0.19, 0.12, 0.25, 0.45)

stim <- as.matrix(read.table("faceStim.csv", sep=",")) #* \label{line:MaximumLikelihood:readFaces}  *\#

exemplars <- list(a=stim[1:5,], b= stim[6:10,]) #* \label{line:MaximumLikelihood:assignExemplars}  *\#

preds <- GCMpred(stim[1,], exemplars, c, w)

likelihood <- dbinom(N_A ,size = N,prob = preds[1])

#preds <- t(apply(testProbe, 1, function(x) GCMpred(x, exemplars, c, w)))

#likelihood <- GCMlik(N_A, N, stim[1,], exemplars, c, w)