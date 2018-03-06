GCMpred <- function(probe, exemplars, c, w){
  
  # calculate likelihod of N_A `A' responses out of N given parameter c
  # 'stim' is a single vector representing the stimulus to be categorised
  # 'exemplars' is a list of exemplars; the first list item is the 'A' exemplars
  #   in memory, and the second list item is the `B` exemplars in memory
  #   each list item is a matrix in which the rows correspond to individual
  #   exemplars
  # 'c' is the scaling parameter, and 'w' is a vector giving weighting for each
  #   stimulus dimension (the columns in 'stim' and 'exemplars')
  
  # note: for a large number of categories we could use lapply to loop across
  # the categories in the list 'exemplars'
  
  dist <- list() #* \label{line:MaximumLikelihood:initdist}  *\#
  for (ex in exemplars){ #* \label{line:MaximumLikelihood:exemplarloop}  *\#
    dist[[length(dist)+1]] <- apply(as.array(ex), 1, 
                          function(x) sqrt(sum(w*(x-probe)^2)))
  }
  
  sumsim <- lapply(dist, function(a) sum(exp(-c*a)))
  
  r_prob <- unlist(sumsim)/sum(unlist(sumsim))
  
}