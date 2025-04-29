#plot some betas
curve(dbeta(x, 2, 4),ylim=c(0,6),ylab="Probability Density",las=1)
curve(dbeta(x, 8, 16),add=TRUE,lty="dashed")
legend("topright",c("Johnnie","Jane"), inset=.05,lty=c("solid","dashed"))

#the remaining lines are not listed in the book but perform some of the computations mentioned there
pbeta(.53,8,16)-pbeta(.13,8,16)
pbeta(.53,2,4)-pbeta(.13,2,4)

x11(7,7)
alpha <- beta <- 12
curve(dbeta(x, alpha, beta),ylim=c(0,40),ylab="Probability Density",las=1,lwd=3)
t<-c(12,100,1000)
i<-0
for (h in c(14,113,1130)){
  i<-i+1
  curve(dbeta(x, alpha+h, beta+t[i]),add=TRUE,lty=log10(t)+1)
  print(c((alpha+h)/(alpha+h+beta+t[i]),h/(h+t[i])))
}
legend("topright",c("{14, 26}","{113, 213}", "{1130, 2130}"), 
       inset=.05,lty=c(2:4))
abline(v=0.5,col="red")

pbeta(.5,1130,1000)
pbeta(.5305164,1130,1000)
