MinimumRangeAlt <- function(vector.population, integer.housesize) {
  # Lexicographic Burt-Harris/Minimum range method for the Apportionment Problem
  # Author: Dominik Schröder
  #
  # Args:
  #   vector.population: a vector containing the population of each state per column.
  #   integer.housesize: the house size as variable.
  #
  #
  # Returns:
  #   A Vector containing the optimized allotment of seats.
  #   
  data <- data.frame("population" = vector.population)
  integer.numberofstates <- length(vector.population)
  
  #__________________________________________________________________________
  #Create arbitrary allotment
  data["allotment"] <- array(0, integer.numberofstates)
  
  tmp <- floor(integer.housesize/integer.numberofstates)
  
  for(i in 1:integer.numberofstates){
    data[i, "allotment"] <- tmp
  }
  
  i <- 1
  while(sum(data[, "allotment"]) < integer.housesize){
    data[i, "allotment"] <- data[i, "allotment"] + 1
    i <- (i + 1) %% (integer.housesize - 1)
  }
  
  #________________________________________________________________________
  #Start of Algorithm:
  
  data["avgconstituency"] <- array(0, integer.numberofstates)
  
  # Looks at two elements in each iteration.
  for (count in 0:(ceiling(integer.numberofstates / 2))) {
    #_________________________________________________________________________________________
    #Biggest aj Value
    maxDisparity1 <- MaxDisparity(data)
    #Optimization by adding one seat the the state with the largest average consituency size:

    data <- CalcAvg(data)
    ranks <- order(data[, "avgconstituency"])  
    
    
    #If Algorithm didn't find a new optimisation, bool is set to 1. Iteration continues
    bool <- TRUE
    
    while (bool) {
      data[ranks[integer.numberofstates - count], "allotment"]  <- data[ranks[integer.numberofstates - count], "allotment"] + 1
      data <- CalcAvg(data)
      
      maxDisparity2 <- 0
      bool <- FALSE
      
      #Break when the first smaller disparity is found!
      for (i in (1 + count):integer.numberofstates)  {
        if (i != ranks[integer.numberofstates - count]) {
          data[i, "allotment"] <- data[i, "allotment"] - 1
          data <- CalcAvg(data)
          maxDisparity2 <- MaxDisparity(data)
          if (maxDisparity2 >= maxDisparity1) {
            data[i, "allotment"] <- data[i, "allotment"] + 1
            data <- CalcAvg(data)
          } else {
            maxDisparity1 <- maxDisparity2
            bool <- TRUE
            break
          }
        }
      }
      if (!bool) {
        data[ranks[integer.numberofstates - count], "allotment"]  <- data[ranks[integer.numberofstates - count], "allotment"] - 1
        data <- CalcAvg(data)
      }
    }
    
    
    #_________________________________________________________________________
    #Smallest rj Value
    maxDisparity1 <- MaxDisparity(data)
    
    #Optimization by adding one seat the the state with the largest average consituency size:
    #TODO: Repeat this process until largest average consituency can't be optimized...
    data <- CalcAvg(data)
    
    bool <- TRUE
    
    while (bool) {
      data[ranks[1 + count], "allotment"]  <- data[ranks[1 + count], "allotment"] - 1
      data <- CalcAvg(data)
      
      maxDisparity2 <- 0
      bool <- FALSE
      
      #Break when the first smaller disparity is found!
      for (i in (integer.numberofstates - count):1)  {
        if (i != ranks[1 + count]) {
          data[i, "allotment"] <- data[i, "allotment"] + 1
          data <- CalcAvg(data)
          maxDisparity2 <- MaxDisparity(data)
          if (maxDisparity2 >= maxDisparity1) {
            data[i, "allotment"] <- data[i, "allotment"] - 1
            data <- CalcAvg(data)
            
          } else {
            maxDisparity1 <- maxDisparity2
            bool <- TRUE
            break
          }
        }
      }
      if (!bool) {
        data[ranks[1 + count], "allotment"]  <- data[ranks[1 + count], "allotment"] + 1
        data <- CalcAvg(data)
      }
    }
  }
  
  return(data)
  
}

# Searches for largest and smallest element in the given array and calculates max.disparty with those.

MaxDisparity <- function(data){
  data <- CalcAvg(data)
  max <- max(data[, "avgconstituency"])
  min <- min(data[, "avgconstituency"])
  return(abs((max/min)-1))
}

CalcAvg <- function(data){
  for (i in 1:length(data[, 1])) {
    data[i, "avgconstituency"] <- data[i, "population"] / data[i, "allotment"]
  }
  return(data)
} 
