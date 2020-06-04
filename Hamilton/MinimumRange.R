#' @name MinimumRange
#' @title Compute vector of allotment according to Lexicographic Burt-Harris/Minimum range method
#' @description \strong{Lexicographic Burt-Harris/Minimum range computation:} \cr
#' For further information see \link{MinimumRange}
#' @export MinimumRange
#' @template author/DS
#' @template author/JT
#' @template param/vectorpopulation
#' @template param/housesize
#' @return Returns an allotment vector
#' @examples
#' library(apportionment)
#' population <-c(1328726, 309694, 424057, 291425, 539907, 335810, 344872, 360048, 435284, 245305,
#'  309838, 248455, 160796, 1008073, 255831, 459677, 186034, 207573, 288071, 228583)
#' house <- 106
#' MinimumRange(population,house)
#' #Output
#' #[1] 16  4  6  4  8  4  5  5  6  3  4  3  2 14  3  6  3  3  4  3


MinimumRange <- function(vector.population, housesize) {
  
  # CheckInput(vector.population, housesize)
  
  if (length(housesize) != 1) {
    data.result <- data.frame(vector.population)
    for (i in 1:length(housesize)) {
      data.result[paste(housesize[i])] <- MinimumRangeInteger(vector.population, housesize[i])
    }
    data.result["vector.population"] <- NULL
    return(data.matrix(t(data.result)))
    
  } else {
    return(MinimumRangeInteger(vector.population, housesize))
  }
}
  
  MinimumRangeInteger <-
    function(vector.population, integer.housesize) {
      # CheckInput(vector.population, integer.housesize)
      
      data.allotment <- data.frame("population" = vector.population)
      integer.numberofstates <- length(vector.population)
      
      #__________________________________________________________________________
      #Create arbitrary allotment
      data.allotment["allotment"] <- array(0, integer.numberofstates)
      
      integer.sumpopulation <- sum(vector.population)
      
      tmp.avgAllotment <- floor(integer.housesize / integer.numberofstates)
      

      for (i in 1:integer.numberofstates) {
        data.allotment[i, "allotment"] <- floor((data.allotment[i, "population"] / integer.sumpopulation) * integer.housesize)
      }
      
      i <- 1
      while (sum(data.allotment[, "allotment"]) < integer.housesize) {
        data.allotment[i, "allotment"] <- data.allotment[i, "allotment"] + 1
        i <- (i + 1) %% (integer.housesize - 1)
      }
      
      #________________________________________________________________________
      #Start of Algorithm:
      
      data.allotment["avgconstituency"] <-
        array(0, integer.numberofstates)
      data.allotment["originalorder"] <-
        array(1:integer.numberofstates, integer.numberofstates)
      
      data.results <- data.allotment
      
      # Looks at two elements in each iteration.
      for (count in 1:(ceiling(integer.numberofstates / 2))) {
        #_________________________________________________________________________________________
        
        
        maxDisparity1 <- MaxDisparity(data.allotment)
        #Optimization by adding one seat the the state with the largest average consituency size:
        
        while (TRUE) {
          data.allotment <- CalcAvg(data.allotment)
          integer.maxIndex <-
            which(data.allotment$avgconstituency %in% c(max(data.allotment[, "avgconstituency"])))
          
          #If Algorithm didn't find a new optimisation, bool is set to 1. Iteration continues
          bool.isBestIndex <- TRUE
          
          tmp.allotment <- data.allotment[integer.maxIndex, "allotment"]
          if (tmp.allotment > 0) {
            data.allotment[integer.maxIndex, "allotment"]  <- tmp.allotment + 1
            maxDisparity2 <- 0
            
            integer.bestindex <- 0
            bool.isBestIndex <- FALSE
            
            for (i in 1:length(data.allotment[, 1]))  {
              if (i != integer.maxIndex &&
                  data.allotment[i, "allotment"] > 1) {
                data.allotment[i, "allotment"] <- data.allotment[i, "allotment"] - 1
                maxDisparity2 <- MaxDisparity(data.allotment)
                
                if (maxDisparity2 < maxDisparity1) {
                  maxDisparity1 <- maxDisparity2
                  integer.bestindex <- i
                  bool.isBestIndex <- TRUE
                }
                data.allotment[i, "allotment"] <-
                  data.allotment[i, "allotment"] + 1
              }
            }
            if (!bool.isBestIndex) {
              #No optimisation found!
              data.allotment[integer.maxIndex, "allotment"]  <-
                data.allotment[integer.maxIndex, "allotment"] - 1
              break
            } else {
              #Optimisation found!
              data.allotment[integer.bestindex, "allotment"] <-
                data.allotment[integer.bestindex, "allotment"] - 1
              
            }
          } else {
            break
          }
        }
        
        maxDisparity1 <- MaxDisparity(data.allotment)
        
        #_________________________________________________________________________
        while (TRUE) {
          #Optimization by adding one seat the the state with the largest average consituency size:
          data.allotment <- CalcAvg(data.allotment)
          integer.minIndex <-
            which(data.allotment$avgconstituency %in% c(min(data.allotment[, "avgconstituency"])))
          
          bool.isBestIndex <- TRUE
          
          tmp.allotment <- data.allotment[integer.minIndex
                                          , "allotment"]
          
          if (tmp.allotment > 1) {
            data.allotment[integer.minIndex
                           , "allotment"]  <- tmp.allotment - 1
            
            maxDisparity2 <- 0
            
            integer.bestindex <- 0
            bool.isBestIndex <- FALSE
            
            for (i in length(data.allotment[, 1]):1)  {
              if (i != integer.minIndex
                  && data.allotment[i, "allotment"] > 1) {
                data.allotment[i, "allotment"] <- data.allotment[i, "allotment"] + 1
                maxDisparity2 <- MaxDisparity(data.allotment)
                
                if (maxDisparity2 < maxDisparity1) {
                  maxDisparity1 <- maxDisparity2
                  integer.bestindex <- i
                  bool.isBestIndex <- TRUE
                }
                data.allotment[i, "allotment"] <-
                  data.allotment[i, "allotment"] - 1
              }
            }
            if (!bool.isBestIndex) {
              data.allotment[integer.minIndex
                             , "allotment"]   <-
                data.allotment[integer.minIndex
                               , "allotment"]  + 1
              break
            } else {
              data.allotment[integer.bestindex, "allotment"] <-
                data.allotment[integer.bestindex, "allotment"] + 1
              
            }
          } else {
            break
          }
        }
        
        data.allotment <- CalcAvg(data.allotment)
        
        #Index of the biggest/smallest avgconstituency in the data DataFrame
        integer.minIndex <-
          which(data.allotment$avgconstituency %in% c(min(data.allotment[, "avgconstituency"])))
        integer.maxIndex <-
          which(data.allotment$avgconstituency %in% c(max(data.allotment[, "avgconstituency"])))
        
        integer.indexOriginalOrder <-
          data.allotment[integer.maxIndex, "originalorder"]
        data.results[integer.indexOriginalOrder, "allotment"] <-
          data.allotment[integer.maxIndex, "allotment"]
        data.results[integer.indexOriginalOrder, "avgconstituency"] <-
          data.allotment[integer.maxIndex, "avgconstituency"]
        
        #The smallest/first element in the ranks array
        
        integer.indexOriginalOrder <- data.allotment[integer.minIndex
                                                     , "originalorder"]
        data.results[integer.indexOriginalOrder, "allotment"] <-
          data.allotment[integer.minIndex
                         , "allotment"]
        data.results[integer.indexOriginalOrder, "avgconstituency"] <-
          data.allotment[integer.minIndex
                         , "avgconstituency"]
        
        #Remove elements from data
        data.allotment <-
          data.allotment[-c(integer.maxIndex, integer.minIndex), ]
        
      }
      
      return(as.vector(data.results[["allotment"]]))
      
    }
  
  # Searches for largest and smallest element in the given array and calculates max.
  # disparity with those.
  
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