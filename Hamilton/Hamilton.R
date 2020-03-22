Hamilton <- function(p, h) {
  # Implemented Hamilton-Method for the Apportionment Problem
  #
  # Args:
  #   x: a data frame with the names of the states as first column 
  #     and the equivalent population
  #     size in the second column named 'population'.
  #   h: the house size as variable.
  #
  # Returns:
  #   A DataFrame containing all the Information 
  #   and the final result in the last column.
  
  psum <- sum(p2[, "population"])
  a <- psum / h
  
  share <- array(0, length(p))
  share.floor <- array(0, length(p))
  fraction <- array(0, length(p))
  
  p[["share"]] <- share
  p[["share.floor"]] <- share.floor
  p[["fraction"]] <- fraction
  
  for (i in 1:length(p[["population"]])) {
    p[i, "share"] <- p[i, "population"] / a
    p[i, "share.floor"] <- floor(p[i, "share"])
    p[i, "fraction"] <-  p[i, "share"] - p[i, "share.floor"]
    
  }
  
  if (sum(p[, "share.floor"]) == h) {
    return(p)
  }
  
  p[["result"]] <- replicate(1, p[["share.floor"]])
  
  ranks <- order(p$fraction, decreasing = TRUE)
  for (i in 1:(h - sum(p[, "share.floor"]))) {
    p[[ranks[i], "result"]] <-  p[[ranks[i], "result"]] + 1
  }
  return(p)
}


#TODO:
#Wie verpacken?? Wie deployen? Auf den GitLab Server pushen??