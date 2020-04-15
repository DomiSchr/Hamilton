Hamilton <- function(p, h, q = 0) {
  #  Largest remainder method for the Apportionment Problem
  #  Author: Dominik Schröder
  #
  # Args:
  #   p: a vector containing the population of each state per column.
  #   h: the house size as variable.
  #   q: the quota. Choose: 0 for Hare Quota(Default)
  #                         1 for Droop Quota
  #                         2 for Hagenbach-Bischoff Quota
  #                         3 for Imperiali Quota
  #
  # Returns:
  #   A Vector containing the allotment of seats.
  
  if(h < 1){
    stop("House size cannot be less than 1")
  }

  p <- data.frame(p)
  
  psum <- sum(p[, 1])
  
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
  
  p["share.floor"] <- array(0, length(p[,1]))
  p["fraction"] <- array(0, length(p[,1]))
  
  for (i in 1:length(p[,1])) {
    tmp <- p[i, 1] / a
    if(tmp < 0){
      stop("Seat amount cannot be a negative value!")
    }
    p[i, "share.floor"] <- floor(tmp)
    p[i, "fraction"] <- tmp - floor(tmp)
    
  }
  
  if (sum(p[, "share.floor"]) == h) {
    return(as.vector(p[["share.floor"]]))
  }
  p["result"] <- NA
  p["result"] <- replicate(1, p[["share.floor"]])
  
  ranks <- order(p$fraction, decreasing = TRUE)
  for (i in 1:(h - sum(p[, "share.floor"]))) {
    p[[ranks[i], "result"]] <-  p[[ranks[i], "result"]] + 1
  }
  return(as.vector(p[["result"]]))
}