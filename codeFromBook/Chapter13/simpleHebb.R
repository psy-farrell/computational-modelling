stim <- list(c(1,-1,1,-1),
             c(1,1,1,1))
resp <- list(c(1,1,-1,-1),
             c(1,-1,-1,1))
             

n <- 4 # number of input units
m <- 4 # number of output units

W <- matrix(rep(0,m*n), nrow=m)

alpha <- 0.25

# Learning
for (pair in 1:2){ # store association for each pair
  for (i in 1:m){ # loop across output units
    for (j in 1:n){ # loop across input units
      W[i,j] <- W[i,j] + alpha*stim[[pair]][j]*resp[[pair]][i] #* \label{line:NeuralNetworks:simpleLearn}  *\#
    }
  }
}

# Learning 2
W2 <- resp[[1]]%*%t(stim[[1]]) + resp[[2]] %*% t(stim[[2]])

# Test phase; test with first stimulus
o <- rep(0,m) #* \label{line:NeuralNetworks:simpleInitOut}  *\#
for (i in 1:m){
  for (j in 1:n){
    o[i] <- o[i] + W[i,j]*stim[[1]][j]
  }
}

library(lsa)
cosine(o,resp[[1]])