MinimumRange3 <- function(vector.population, integer.housesize) {
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
  #________________________________________________________________________
  #Start of Algorithm:
  
  data["avgconstituency"] <- array(0, integer.numberofstates)
  data["originalorder"] <- array(1:integer.numberofstates, integer.numberofstates)
  
  results <- data
  
  
  # Looks at two elements in each iteration.
  for (count in 1:(ceiling(integer.numberofstates / 2))) {
    #_________________________________________________________________________________________
    
    maxDisparity1 <- MaxDisparity(data)
    
    #Optimization by adding one seat the the state with the largest average consituency size:
    
    data <- CalcAvg(data)
    ranks <- order(data[, "avgconstituency"])
    
    
    #If Algorithm didn't find a new optimisation, bool is set to 1. Iteration continues
    bool <- TRUE
    
    while (bool) {
      tmp <- data[ranks[length(ranks)], "allotment"]
      if (tmp > 0) {
        data[ranks[length(ranks)], "allotment"]  <- tmp + 1
        data <- CalcAvg(data)
        maxDisparity2 <- 0
        
        integer.bestindex <- 0
        bool <- FALSE
        
        #Break when the first smaller disparity is found!
        for (i in 1:length(ranks))  {
          if (i != ranks[length(ranks)] &&
              data[i, "allotment"] > 1) {
            data[i, "allotment"] <- data[i, "allotment"] - 1
            maxDisparity2 <- MaxDisparity(data)
            
            if (maxDisparity2 < maxDisparity1) {
              maxDisparity1 <- maxDisparity2
              integer.bestindex <- i
              bool <- TRUE
            }
            data[i, "allotment"] <- data[i, "allotment"] + 1
          }
        }
        data <- CalcAvg(data)
        if (!bool) {
          data[ranks[length(ranks)], "allotment"]  <- data[ranks[length(ranks)], "allotment"] - 1
        } else {
          data[integer.bestindex, "allotment"] <-
            data[integer.bestindex, "allotment"] - 1
          
        }
        data <- CalcAvg(data)
      } else {
        bool <- FALSE
      }
    }
    
    
    #_________________________________________________________________________
    
    maxDisparity1 <- MaxDisparity(data)
    
    #Optimization by adding one seat the the state with the largest average consituency size:
    #TODO: Repeat this process until largest average consituency can't be optimized...
    data <- CalcAvg(data)
    
    bool <- TRUE
    
    while (bool) {
      if (data[ranks[1], "allotment"] > 1) {
        data[ranks[1], "allotment"]  <- data[ranks[1], "allotment"] - 1

        data <- CalcAvg(data)
        maxDisparity2 <- 0
        
        integer.bestindex <- 0
        bool <- FALSE
        
        #Break when the first smaller disparity is found!
        for (i in length(ranks):1)  {
          if (i != ranks[1] && data[i, "allotment"] > 1) {
            data[i, "allotment"] <- data[i, "allotment"] + 1
            maxDisparity2 <- MaxDisparity(data)
            
            
            if (maxDisparity2 < maxDisparity1) {
              maxDisparity1 <- maxDisparity2
              integer.bestindex <- i
              bool <- TRUE
            }
            data[i, "allotment"] <- data[i, "allotment"] - 1
          }
        }
        data <- CalcAvg(data)
        if (bool == FALSE) {
          data[ranks[1], "allotment"]   <- data[ranks[1], "allotment"]  + 1
        } else {
          data[integer.bestindex, "allotment"] <-
            data[integer.bestindex, "allotment"] + 1
        }
        data <- CalcAvg(data)
      } else {
        bool <- FALSE
      }
    }
    
    #Copying the result in the results dataframe
    #The largest/last element in the ranks array
    idx <- data[ranks[length(ranks)], "originalorder"]
    results[idx, "allotment"] <- data[ranks[length(ranks)], "allotment"]

      
    #The smallest/first element in the ranks array
    idx <- data[ranks[1], "originalorder"]
    results[idx, "allotment"] <- data[ranks[1], "allotment"]
    
    #Remove elements from data
    data <- data[-c(ranks[length(ranks)]), ]
    data <- data[-c(ranks[1]), ]
  }
  
  
  
  return(results)
  
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
