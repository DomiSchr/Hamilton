MinimumRange2 <- function(vector.population, integer.housesize) {
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
  
  tmp <- floor(integer.housesize / integer.numberofstates)
  
  for (i in 1:integer.numberofstates) {
    data[i, "allotment"] <- tmp
  }
  
  i <- 1
  while (sum(data[, "allotment"]) < integer.housesize) {
    data[i, "allotment"] <- data[i, "allotment"] + 1
    i <- (i + 1) %% (integer.housesize - 1)
  }
  
  data["avgconstituency"] <- array(0, integer.numberofstates)
  
  #________________________________________________________________________
  #Start of the Algorithm:
  
  # Looks at two elements per iteration.
  for (count in 0:(ceiling(integer.numberofstates / 2))) {
    maxDisparity1 <- MaxDisparity(data)
    
    data <- CalcAvg(data)
    ranks <- order(data[, "avgconstituency"])
    
    #If Algorithm didn't find a new optimisation, iterate is set to 1. Iteration continues
    iterate <- TRUE
    
    while (iterate) {
      currentAllotment <-
        data[ranks[integer.numberofstates - count], "allotment"]
      
      #Guarantees that the allotment for every state is at least one seat.
      if (currentAllotment > 0) {
        #Optimization by adding one seat to the state with the largest average consituency size.
        data[ranks[integer.numberofstates - count], "allotment"]  <-
          currentAllotment + 1
        maxDisparity2 <- 0
        
        integer.bestindex <- 0
        iterate <- FALSE
        
        #Iterating over the Dataframe, searching for the best disparity
        for (i in (1 + count):(integer.numberofstates - count))  {
          if (i != ranks[integer.numberofstates - count] &&
              data[i, "allotment"] > 1) {
            data[i, "allotment"] <- data[i, "allotment"] - 1
            maxDisparity2 <- MaxDisparity(data)
            
            #When a lower disparity is found, remember the index in integer.bestindex
            if (maxDisparity2 < maxDisparity1) {
              maxDisparity1 <- maxDisparity2
              integer.bestindex <- i
              iterate <- TRUE
            }
            #Undoes the changes
            data[i, "allotment"] <- data[i, "allotment"] + 1
          }
        }
        
        if (!iterate) {
          #No optimisation was found, undoes the change
          data[ranks[integer.numberofstates - count], "allotment"]  <-
            data[ranks[integer.numberofstates - count], "allotment"] - 1
        } else {
          #Restore to the best found disparity
          data[integer.bestindex, "allotment"] <-
            data[integer.bestindex, "allotment"] - 1
        }
      } else {
        iterate <- FALSE
      }
    }
    
    #_________________________________________________________________________
    
    maxDisparity1 <- MaxDisparity(data)
    
    #Optimization by adding one seat the the state with the largest average consituency size:
    data <- CalcAvg(data)
    
    #If Algorithm didn't find a new optimisation, iterate is set to 1. Iteration continues
    iterate <- TRUE
    
    while (iterate) {
      #Guarantees that the allotment for every state is at least one seat.
      if (data[ranks[1 + count], "allotment"] > 1) {
        #Optimization by removing one seat to the state with the smallest average consituency size.
        data[ranks[1 + count], "allotment"]  <-
          data[ranks[1 + count], "allotment"] - 1
        
        data <- CalcAvg(data)
        maxDisparity2 <- 0
        
        integer.bestindex <- 0
        iterate <- FALSE
        
        #Iterating over the Dataframe, searching for the best disparity
        for (i in (integer.numberofstates - count):(1 + count))  {
          if (i != ranks[1 + count] && data[i, "allotment"] > 1) {
            data[i, "allotment"] <- data[i, "allotment"] + 1
            maxDisparity2 <- MaxDisparity(data)
            
            #When a lower disparity is found, remember the index in integer.bestindex
            if (maxDisparity2 < maxDisparity1) {
              maxDisparity1 <- maxDisparity2
              integer.bestindex <- i
              iterate <- TRUE
            }
            #Undoes the changes
            data[i, "allotment"] <- data[i, "allotment"] - 1
          }
        }
        data <- CalcAvg(data)
        if (iterate == FALSE) {
          #No optimisation was found, undoes the change
          data[ranks[1 + count], "allotment"]  <-
            data[ranks[1 + count], "allotment"] + 1
        } else {
          #Restore to the best found disparity
          data[integer.bestindex, "allotment"] <-
            data[integer.bestindex, "allotment"] + 1
        }
        data <- CalcAvg(data)
      } else {
        iterate <- FALSE
      }
    }
  }
  
  return(as.vector(data[["allotment"]]))
  
}

# Searches for largest and smallest element in the given array and calculates max.disparty with those.

MaxDisparity <- function(data) {
  data <- CalcAvg(data)
  max <- max(data[, "avgconstituency"])
  min <- min(data[, "avgconstituency"])
  return(abs((max / min) - 1))
}

CalcAvg <- function(data) {
  for (i in 1:length(data[, 1])) {
    data[i, "avgconstituency"] <-
      data[i, "population"] / data[i, "allotment"]
  }
  return(data)
}
