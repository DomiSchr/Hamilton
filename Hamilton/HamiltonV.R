HamiltonV <- function(p, h, q) {
  #  Largest remainder method for the Apportionment Problem
  #
  # Args:
  #   p: a data frame with the names of the states as first column 
  #     and the equivalent population
  #     size in the second column.
  #   h: the house size as variable.
  #   q: the quota. Choose: 0 for Hare Quota
  #                         1 for Droop Quota
  #                         2 for Hagenbach-Bischoff Quota
  #                         3 for Imperiali Quota
  #
  # Returns:
  #   A DataFrame containing all the Information 
  #   and the final result in the last column.
  
  p <- data.frame(p)
  
  psum <- sum(p[, 1])
  
  if(q == 0){
    #Hare Quota:
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
    p[i, "share.floor"] <- floor(tmp)
    p[i, "fraction"] <- tmp - floor(tmp)
    
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
  
  return(p[["result"]])
}

HamiltonV <- function(p, h1, h2, q){
  for(i in h1:h2){
    HamiltonV(p, i, q)
  }
}

HamiltonV(c(630, 480, 390, 500),10 ,12 ,0)