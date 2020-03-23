Hamilton <- function(p, h) {
  # Implemented Hamilton-Method for the Apportionment Problem
  #
  # Args:
  #   p: a data frame with the names of the states as first column 
  #     and the equivalent population
  #     size in the second column.
  #   h: the house size as variable.
  #
  # Returns:
  #   A DataFrame containing all the Information 
  #   and the final result in the last column.
  
  psum <- sum(p[, 2])
  a <- psum / h
  
  share <- array(0, length(p))
  share.floor <- array(0, length(p))
  fraction <- array(0, length(p))
  
  p[["share"]] <- NA
  p["share"] <- share
  browser()
  p[["share.floor"]] <-NA
  p["share.floor"] <- share.floor
  p[["fraction"]] <- NA
  p["fraction"] <- fraction
  
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

states <- c("a","b", "c")
population <- c(630, 480, 390)

p <- data.frame(states, population )
Hamilton(p,10)
