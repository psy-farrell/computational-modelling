#plot data and current predictions          
getregpred <- function(parms,data) {
  getregpred <- parms["b0"] + parms["b1"]*data[ ,2]           
  
  #wait with drawing a graph until key is pressed
  par(ask=TRUE)
  plot   (data[ ,2], type="n", las=1, ylim=c(-2,2), xlim=c(-2,2), xlab="X", ylab="Y")
  par(ask=FALSE)
  points (data[ ,2], data[ ,1], pch=21, bg="gray")
  lines  (data[ ,2], getregpred, lty="solid")
  
  return(getregpred)
}                                           

#obtain current predictions and compute discrepancy
rmsd <-function(parms, data1) {          
  preds<-getregpred(parms, data1)        
  rmsd<-sqrt(sum((preds-data1[ ,1])^2)/length(preds))
}

#define parameters to generate data
nDataPts  <- 20
rho       <- .8
intercept <- .0

#generate synthetic data
data<-matrix(0,nDataPts,2)
data[ ,2] <- rnorm(nDataPts)   
data[ ,1] <- rnorm(nDataPts)*sqrt(1.0-rho^2) + data[ ,2]*rho + intercept  

#do conventional regression analysis
lm(data[,1] ~ data[,2])        
#assign starting values 
startParms <- c(-1., .2) 
names(startParms) <- c("b1", "b0")
#obtain parameter estimates
xout <- optim(startParms, rmsd, data1=data)

