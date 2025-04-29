# Read in the data
# Rows are participants, columns are serial positions
# spcdat <- read.table("freeAccuracy.txt")

# Or generate some example data
nPrim <- 25
nRec <- 50
nBoth <- 25

ll <- 12
serpos <- 1:ll
nTrials <- 10

primDat <- matrix(rep(0,ll*nPrim),nPrim,ll)
for (j in 1:nPrim){
  asym <- 0.3
  expp <- 1
  tdat <- (1-asym)*exp(-expp*(serpos-1)) + asym
  primDat[j,] <- rbinom(n=ll,size=nTrials,prob=tdat)/nTrials
}

recDat <- matrix(rep(0,ll*nRec),nRec,ll)
for (j in 1:nRec){
  asym <- 0.3
  expp <- 1
  tdat <- (1-asym)*exp(-expp*rev(serpos-1)) + asym
  recDat[j,] <- rbinom(n=ll,size=nTrials,prob=tdat)/nTrials
}

bothDat <- matrix(rep(0,ll*nBoth),nBoth,ll)
for (j in 1:nBoth){
  asym <- 0.5
  expp <- 1
  pc <- 0.5 * exp(-expp*rev(serpos-1)) + 0.5 * exp(-expp*(serpos-1))
  tdat <- (1-asym)*pc + asym
  bothDat[j,] <- rbinom(n=ll,size=nTrials,prob=tdat)/nTrials
}

spcdat <- rbind(primDat,recDat,bothDat)

#------------------------------------------
#pdf(file="gap_plot.pdf", width=4, height=4)
par(mfrow=c(1,1))

library(cluster) #* \label{line:MultipleParticipants:libclust}  *\#
gskmn <- clusGap(spcdat, FUN = kmeans, nstart = 20, K.max = 8, B=500)
plot(gskmn, ylim=c(0.15, 0.5))

#dev.off()

#-------------------------------------------
#pdf(file="kmeansSPC.pdf", width=8, height=4)
par(mfrow=c(1,2))
plot(colMeans(spcdat), ylim=c(0,1), type="b",
     xlab="Serial Position", ylab="Proportion Correct", main=NULL)

kmres <- kmeans(spcdat, centers=3, nstart=10) #* \label{line:MultipleParticipants:runkmeans}  *\#
matplot(t(kmres$centers), type="b", ylim=c(0,1), 
        xlab="Serial Position", ylab="Proportion Correct")
#dev.off()
# kk <- {}
# 
# for (nClust in 2:7){
#   kmres <- kmeans(spcdat, centers=nClust, nstart=10)
#   #kk <- c(kk, kmres$betweenss/kmres$totss)
#   kk <- c(kk,kmres$tot.withinss)
# }

#plot(2:7, kk, type="b", xlab="N Clusters", ylab="")

