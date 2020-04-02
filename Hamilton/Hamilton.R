Hamilton <- function(p, h, q = 0) {
  #  Largest remainder method for the Apportionment Problem
  #
  # Args:
  #   p: a data frame with the names of the states as first column 
  #     and the equivalent population
  #     size in the second column.
  #   h: the house size as variable.
  #   q: the quota. Choose: 0 for Hare Quota(Default)
  #                         1 for Droop Quota
  #                         2 for Hagenbach-Bischoff Quota
  #                         3 for Imperiali Quota
  #
  # Returns:
  #   A DataFrame containing all the Information 
  #   and the final result in the last column.
  
  psum <- sum(p[, 2])
  
  if(q == 0){
    #Hare Quota(Default):
    a <- floor(psum / h)
    
  } else if(q == 1){
    #Droop Quota
    a <- floor(1 + psum/(h + 1))
    
  } else if(q == 2){
    #Hagenbach-Bischoff Quota
    a <-  floor(psum/(h + 1))

  } else if(q == 3){
    #Imperiali Quota
    a <- floor(psum/(h + 2))
    
  } else {
    # Exception!
    stop("Chosen quota option not valid!")
  }
  
  
  p["share"] <- array(0, length(p[,1]))
  p["share.floor"] <- array(0, length(p[,1]))
  p["fraction"] <- array(0, length(p[,1]))
  
  for (i in 1:length(p[,2])) {
    p[i, "share"] <- p[i, 2] / a
    p[i, "share.floor"] <- floor(p[i, "share"])
    p[i, "fraction"] <-  p[i, "share"] - p[i, "share.floor"]
    
  }
  
  if (sum(p[, "share.floor"]) == h) {
    return(p)
  }
  p[["result"]] <- NA
  p["result"] <- replicate(1, p[["share.floor"]])
  
  ranks <- order(p$fraction, decreasing = TRUE)
  for (i in 1:(h - sum(p[, "share.floor"]))) {
    p[[ranks[i], "result"]] <-  p[[ranks[i], "result"]] + 1
  }
  return(p)
}