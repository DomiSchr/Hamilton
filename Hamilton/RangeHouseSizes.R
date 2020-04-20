  RangeHouseSizes <- function(FUN, p, h1, h2, quota = 0) {
    #  The method calculates a range of house sizes for the given Apportionment Method.
    #  Author: Dominik Schröder
    #
    # Args:
    #   FUN: the desired apportionment method
    #   p: a vector containing the population of each state per column.
    #   h1: the lower bound of the desired house size.
    #   h2: the upper bound of the desired house size.
    #   quota: if the apportionment method accepts a quota
    #
    # Returns:
    #   A data frame containing the allotment for each state per row and the different house sizes per column.
    output <- data.frame(p)
    
    for(i in h1:h2){
      output[paste(i)] <- FUN(p, i, quota)
    }
    output["p"] <- NULL
    return(output)
  }