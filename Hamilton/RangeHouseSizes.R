RangeHouseSizes <- function(FUN, p, h1, h2, variation = 0) {
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
  
  if(h1 < 1 || h2 < 1){
    stop("House size cannot be less than 1")
  }
  
  for(i in 1:length(p)){
    if(p[1] < 1){
      stop("Seat amount cannot be a negative value!")
    }
  }
  
  output <- data.frame(p)
  
  for (i in h1:h2) {
    tryCatch({
      tmp <- FUN(p, i, variation)},
      error = function(e) {
        tmp <- FUN(p, i)
      }
    )
    output[paste(i)] <- tmp
  }
  output["p"] <- NULL
  return(output)
}