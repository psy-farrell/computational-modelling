priorityHeuristic <- function(prospect,alpha=0.1){
  
  # priority heuristic for two gambles
  
  allx <- c(prospect[[1]]$x, prospect[[2]]$x)
  if (all(allx<0)){ # all negative
    # first compare minimums
    max_x <- min(allx)
    mins <- c(max(prospect[[1]]$x),max(prospect[[2]]$x))
    if ((abs(mins[1]-mins[2])/abs(max_x))>0.1){
      choice_p <- rep(alpha,2)
      choice_p[which.max(mins)] <- 1-alpha
      return(choice_p)
    }
    
    # then compare probability of minimums
    p_of_mins <- c(prospect[[1]]$p[which.max(prospect[[1]]$x)],
                   prospect[[2]]$p[which.max(prospect[[2]]$x)])
    if (abs(p_of_mins[1]-p_of_mins[2])>0.1){
      choice_p <- rep(alpha,2)
      choice_p[which.min(p_of_mins)] <- 1-alpha
      return(choice_p)
    }
    
    # then compare maximums
    maxs <- c(max(prospect[[1]]$x),max(prospect[[2]]$x))
    choice_p <- rep(alpha,2)
    choice_p[which.max(maxs)] <- 1-alpha
    return(choice_p)
    
  } else { # mixed or positive
    
    # first compare minimums
    max_x <- max(allx)
    mins <- c(min(prospect[[1]]$x),min(prospect[[2]]$x))
    if ((abs(mins[1]-mins[2])/max_x)>0.1){
      choice_p <- rep(alpha,2)
      choice_p[which.max(mins)] <- 1-alpha
      return(choice_p)
    }
    
    # then compare probability of minimums
    p_of_mins <- c(prospect[[1]]$p[which.min(prospect[[1]]$x)],
                   prospect[[2]]$p[which.min(prospect[[2]]$x)])
    if (abs(p_of_mins[1]-p_of_mins[2])>0.1){
      choice_p <- rep(alpha,2)
      choice_p[which.min(p_of_mins)] <- 1-alpha
      return(choice_p)
    }
    
    # then compare maximums
    maxs <- c(max(prospect[[1]]$x),max(prospect[[2]]$x))
    choice_p <- rep(alpha,2)
    choice_p[which.max(maxs)] <- 1-alpha
    return(choice_p)
  }
}