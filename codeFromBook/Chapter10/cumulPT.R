# ---------Cumulative prospect theory-------#

# probability weighting function
probw <- function(p,c){
  return(p^c/
           ((p^c + (1-p)^c)^(1/c))
  )
}

# CPT calling fn: calculates subjective value of a prospect
# Assumes x are unique (i.e., no ties)
cumulPTv <- function(x, p, alpha=1,beta=1,lambda=1,gamma=1,delta=1){
  
  if ((length(x)<1) || (length(p)<1) || (length(x)!=length(p))){
    print(x)
    print(p)
    stop("x and p must be of the same length (>1)")
  }
  
  oo <- order(x)
  x <- x[oo]
  p <- p[oo]
  
  # ---- deal with x>= 0
  ii <- which(x>=0)
  
  if (length(ii)>0){
    tx <- x[ii]
    tp <- p[ii]
  
    vp <- tx^alpha
  
    pp <- {}
    pp[length(tp)] <- probw(tp[length(tp)],gamma)
    if (length(ii)>1){
      for (j in 1:(length(ii)-1)){
        pp[j] <- probw(sum(tp[j:length(tp)]),gamma)-
          probw(sum(tp[(j+1):length(tp)]),gamma)
      }
    }
    sv_pos <- sum(vp*pp)
  } else {
    sv_pos <- 0
    vp <- {}
    pp <- {}
  }
  
  # ---- deal with x< 0
  ii <- which(x<0)
  
  if (length(ii)>0){
    tx <- x[ii]
    tp <- p[ii]
    
    vn <- -lambda*(-tx)^beta
    
    pn <- {}
    pn[1] <- probw(tp[1],delta)
    if (length(ii)>1){
      for (i in 2:length(ii)){
        pn[i] <- probw(sum(tp[1:i]),delta)-
          probw(sum(tp[1:(i-1)]),delta)
      }
    }
    sv_neg <- sum(vn*pn)
  } else {
    sv_neg <- 0
    vn <- {}
    pn <- {}
  }
  
  # calculate subjective value of prospect
  sv <- sv_pos + sv_neg
  
  return(list(sv=sv,v=c(vn,vp),pip=c(pn,pp)))
  
}

CPTchoice <- function(prospect,alpha=1,beta=1,lambda=1,gamma=1,delta=1,phi=1){
# prospect is a list of alternative gambles, each gamble holding a vector of values x and a vector of probabilities p
  
  SVs <- lapply(prospect,
                function(tx) cumulPTv(tx$x, tx$p, alpha,beta,lambda,gamma,delta)$sv)
  scalSVs <- exp(phi*unlist(SVs))
  return(scalSVs/sum(scalSVs))
}