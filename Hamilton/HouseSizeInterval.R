HouseSizeInterval <- function(FUN, p, h1, h2, variation = -1) {
  #  The method calculates a range of house sizes for the given Apportionment Method.
  #  Author: Dominik Schröder
  #
  # Args:
  #   FUN: the desired apportionment method
  #   p: a vector containing the population of each state per column.
  #   h1: the lower bound of the desired house size.
  #   h2: the upper bound of the desired house size.
  #   variation(optional): enter value if the apportionment method accepts a variation eg. different quotas
  #
  # Returns:
  #   A data frame containing the allotment for each state per row and the different house sizes per column.
  
  
  # calc <- function(p, i, variation){
  #   tryCatch({
  #     return(FUN(p, i, variation))},
  #     error = function(e) {
  #       return(FUN(p, i))
  #     }
  #   )
  # }
  
  
  calc <- function(p, i, variation){
  if(variation == -1){
    return(FUN(p, i))
  } else {
    return(FUN(p, i, variation))
  }
  }
  
  
  
  if(h1 < 1 || h2 < 1){
    stop("House size cannot be less than 1")
  }
  
  output <- data.frame(p)

    
  
  for (i in h1:h2) {
    output[paste(i)] <- calc(p, i, variation)
  }
  output["p"] <- NULL
  return(output)
}