#perform SDT via ABC

#simulate sdt given parameters and number of trials
simsdt<- function(d,b,ntrials) {
  old <- rnorm(ntrials/2,d)
  hits <-sum(old>(d/2+b))/(ntrials/2)*100
  new <- rnorm(ntrials/2,0)
  fas <- sum(new>(d/2+b))/(ntrials/2)*100
  return(X<-c(hits,fas))
}

y   <- c(60,11)  #define target data   
dmu <- 1         #define hyperparameters
bmu <- 0
dsigma <- bsigma <- 1            

ntrials <- 100                   
epsilon <- 1
posterior <- matrix(0,1000,2)
for (s in c(1:1000)) {  #commence ABC
  while(TRUE) {
    dprop <- rnorm(1,dmu,dsigma)     
    bprop <- rnorm(1,bmu,bsigma)
    X<-simsdt(dprop,bprop,ntrials) #simulate proposal  
    if (sqrt(sum((y-X)^2)) <= epsilon) {break}  
  }
  posterior[s,]<-c(dprop,bprop) #keep good simulation
  print(s)                      #show sign of life
}

apply(posterior,2,mean)
apply(posterior,2,FUN=function(x) quantile(x,c(.025,.975)))
apply(posterior,2,hist)
print(c(s,sqrt(sum((y-X)^2)),X,posterior[s,]))
