library(lsa)

n <- 100 # number of input units
m <- 50 # number of output units

listLength <- 20 # number of pairs in each list

nReps <- 100

alpha <- .25

lesionPSet <- seq(0,1,by = 0.1)

accuracy <- rep(0,length(lesionPSet))

for (rep in 1:nReps){
  
  #W <- matrix(rnorm(m*n, sd=.1), nrow=m)
  W <- matrix(rep(0,m*n), nrow=m)
  
  stim1 <- {}
  resp1 <- {}
  
  # create study set
  for (litem in 1:listLength){
    
    svec <- sign(rnorm(n))
    stim1 <- c(stim1,list(svec))
    
    rvec <- sign(rnorm(m))
    resp1 <- c(resp1,list(rvec))
    
  }
  
  # study list
  for (litem in 1:listLength){
    c <- stim1[[litem]]
    o <- resp1[[litem]]
    W <- W + alpha*o%*%t(c)
    #W <- W + alpha*tcrossprod(o,c)
  }
  
  for (lesionPI in 1:length(lesionPSet)){
    
    lesionP <- lesionPSet[lesionPI]
    Wlesion <- W
    mask <- matrix(runif(m*n)<lesionP, nrow=m)
    Wlesion[mask] = 0
    
    # test list
    tAcc <- 0
    for (litem in 1:listLength){
      c <- stim1[[litem]]
      o <- Wlesion %*% c #* \label{line:cueMatrix2}  *\#
      
      tAcc <- tAcc + cosine(as.vector(o),resp1[[litem]])
    }
    accuracy[lesionPI] <- accuracy[lesionPI] + tAcc/listLength
    
  } 
}# end reps loop

#pdf("HebbLesion.pdf", width=5, height=5)
plot(lesionPSet,accuracy/nReps, type="b",
     xlab="Lesion Probability",
     ylab="Cosine",
     ylim=c(0,1))
#dev.off()