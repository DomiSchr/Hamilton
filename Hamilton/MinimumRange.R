MinimumRange <- function(p, a) {
  data <- data.frame("population" = p, "allotment" = a)
  data["avg"] <- array(0, length(data[, 1]))
  
  for (count in 0:(ceiling(length(data[, 1]) / 2))) {
    #_________________________________________________________________________________________
    #Biggest aj Value
    max1 <- MaxDisparty(data)
    #Optimization by adding one seat the the state with the largest average consituency size:
    #TODO: Repeat this process until largest average consituency can't be optimized...
    data <- CalcAvg(data)
    ranks <- order(data[, "avg"])
    
    #Wiederholtes addieren, bis es nichtmehr kleiner wird
    
    max3 <- .Machine$integer.max
    
    #C zum testen
    c <- 0
    while (c < 5) {
      max3 <- max1
      data[ranks[length(ranks) - count], "allotment"]  <- data[ranks[length(ranks) - count], "allotment"] + 1
      
      max2 <- 0
      bool <- 0
      
      #Bei der ersten kleineren disparty wird abgebrochen!
      for (i in 1:length(data[,1]))  {
        if (i != ranks[length(ranks)] - count) {
          data[i, "allotment"] <- data[i, "allotment"] - 1
          print("for")
          max2 <- MaxDisparty(data)
          if (max2 >= max1) {
            data[i, "allotment"] <- data[i, "allotment"] + 1
            
          } else {
            max1 <- max2
            bool <- bool + 1
            print("else")
            break
          }
        }
      }
      if (bool == 0) {
        data[ranks[length(ranks) - count], "allotment"]  <- data[ranks[length(ranks) - count], "allotment"] - 1
      }
      c <- c + 1
    }
    
    
    #_________________________________________________________________________
    #Smallest rj Value
    max1 <- MaxDisparty(data)
    
    #Optimization by adding one seat the the state with the largest average consituency size:
    #TODO: Repeat this process until largest average consituency can't be optimized...
    data <- CalcAvg(data)
    ranks <- order(data[, "avg"])
    
    #Wiederholtes addieren, bis es nichtmehr kleiner wird
    
    max3 <- .Machine$integer.max
    
    #C zum testen
    c <-0 
    while (c < 5) {
      max3 <- max1
      data[ranks[1 + count], "allotment"]  <- data[ranks[1 + count], "allotment"] + 1
      
      max2 <- 0
      bool <- 0
      
      #Bei der ersten kleineren disparty wird abgebrochen!
      for (i in 1:length(data[,1]))  {
        if (i != ranks[1 + count]) {
          data[i, "allotment"] <- data[i, "allotment"] - 1
          max2 <- MaxDisparty(data)
          if (max2 >= max1) {
            data[i, "allotment"] <- data[i, "allotment"] + 1
            
          } else {
            max1 <- max2
            bool <- bool + 1
            print("else")
            break
          }
        }
      }
      if (bool == 0) {
        data[ranks[1 + count], "allotment"]  <- data[ranks[1 + count], "allotment"] - 1
      }
      c <- c + 1
    }
  }
  
  return(data)
  
}

#Kann man evtl. optimieren!
# Größtes und kleinstes Element raussuchen und nur damit rechnen!
MaxDisparty1 <- function(data) {
  max.disparty <- 0
  for (i in 1:length(data[, 1])) {
    for (j in 1:length(data[, 1])) {
      tmp <-  abs(data[i, "avg"] / data[j, "avg"] - 1)
      if (tmp > max.disparty) {
        max.disparty <- tmp
      }
    }
  }
  return(max.disparty)
}

MaxDisparty <- function(data){
  data <- CalcAvg(data)
  max <- max(data[, "avg"])
  min <- min(data[, "avg"])
  return(abs((max/min)-1))
}

CalcAvg <- function(data){
  for (i in 1:length(data[, 1])) {
    data[i, "avg"] <- data[i, "population"] / data[i, "allotment"]
  }
  return(data)
} 
