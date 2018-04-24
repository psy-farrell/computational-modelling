#plot some vwm data
source("MixModel.R")

#choose data source by commenting out the undesired one
#fn <- "colorSubjccFromvdb2012.dat"
# you will need to obtain the data from Zhang and Luck (2008)
fn <- "zhl08subj1.dat"
vwmerrors <- read.table(fn,sep=",",col.names=c("setsize","errors"))
vwmerrors$errdiscrete<-cut(vwmerrors$error,breaks=seq(from=-180,to=180,by=20),labels=FALSE)
npss <- mean(as.numeric(table(vwmerrors$setsize)))
vwmmeans <- aggregate(errors~setsize+errdiscrete,data=vwmerrors,FUN=function(x) length(x)/npss)
p2f<-FALSE

#get predictions
svalues<-c(0.5,20)
preds<-posteriors<-vector("list",2)
ssz<-c(3,6)
for (s in c(1,2)) {
  cp<-getMixtmodel(subset(vwmerrors,setsize==ssz[s])$errors,svalues)
  preds[[s]] <- cp$preds
  posteriors[[s]] <- cp$posterior
  preds[[s]] <- preds[[s]]/sum(preds[[s]])  #normalize
}
x4preds <- seq(from=0,to=20,length.out=length(preds[[1]]))

#now start plotting
if (p2f) {
  pdf(file="vwmerrors.pdf",height=6,width=6)
} else {x11(7,7)}

par(mar=c(4, 5, 1, 0.5),cex.lab=1.5)
ylimits <- c(0,0.4)
plot(vwmmeans$errdiscrete,vwmmeans$errors,
     type="n",
     ylab="Proportion of responses",
     yaxt="n",
     ylim=ylimits,
     xlab="Difference from actual color value (degrees)",
     xaxt="n",
     xlim=c(0,20),
     las=1)
abline(v=10,lty="dashed")
#plots for different setsizes superimposed with pdf of mixture model
with(subset(vwmmeans,setsize==ssz[1]),{
  points(errdiscrete,errors,pch=21, cex=1.5, col="black",bg="white")
  print(length(errdiscrete))
  lines(x4preds,preds[[1]]*(length(preds[[1]])/length(errdiscrete)),lty="dashed",lwd=2)
})
with(subset(vwmmeans,setsize==ssz[2]),{
  points(errdiscrete,errors,pch=21, cex=1.5, col="black",bg="gray")
  print(length(errdiscrete))
  lines(x4preds,preds[[2]]*(length(preds[[2]])/length(errdiscrete)),lwd=2)
})

legend("topleft",inset=.05,c("Set size 3","Set size 6"),pch=21,pt.bg=c("white","gray"),
       pt.cex=2,lty=c("dashed","solid"),lwd=1.5)

par(tcl= -0.2)  #minor ticks
axis(1, at=seq(from=1,to=19,by=.5), labels=F, lwd=1, lwd.ticks=0.5)
axis(2, at=seq(from=0,to=ylimits[2],by=.01), labels=F, lwd=1, lwd.ticks=0.5)

par(tcl= -0.5)  #major ticks with labels
axis(1, at=c(1,3,5,7,9,10,11,13,15,17,19), labels=c(-180,-140,-100,-60,-20,0,20,60,100,140,180),
     lwd=0, lwd.ticks=1,cex.axis=0.8)
axis(2, at=seq(from=0,to=ylimits[2],by=.1), labels=TRUE, lwd=1, lwd.ticks=0.5,las=1)
if (p2f) {dev.off()}


########plot posteriors
if (p2f) {
  pdf(file="vmposteriors.pdf",height=5,width=5)
} else {x11(5,5)}
xl<- c("Sampled values of g",bquote("Sampled values of "*sigma[vM]))

par(mar=c(4, 4, 1, 0.5))
par(mfrow=c(2,2))
for (s in c(1,2)){
  pe<-apply(posteriors[[s]],2,mean)
  for(p in c(1,2)){
    plot(density(posteriors[[s]][,p]),las=1,xlab=xl[p],
         yaxt="n",lwd=2,lty="solid",
         main="",ylab="")
    mtext("    Density",2,1)
    abline(v=pe[p],lty="solid",lwd=2,col="gray")
    abline(v=quantile(posteriors[[s]][,p],.05),lty="dotted")
    abline(v=quantile(posteriors[[s]][,p],.95),lty="dotted")
  }
}
if (p2f) {dev.off()}

x11()
par(mfrow=c(1,2))
plot(c(1:length(posteriors[[1]][,1])),posteriors[[1]][,1],type="l")
plot(c(1:length(posteriors[[1]][,2])),posteriors[[1]][,2],type="l")
