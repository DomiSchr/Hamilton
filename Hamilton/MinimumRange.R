MinimumRange <- function(p, a) {
  data <- data.frame("population" = p, "allotment" = a)
  data["avg"] <- array(0, length(data[, 1]))
  
  print(data)
  
  for (i in 1:length(data[, 1])) {
    data[i, "avg"] <- p[i] / a[i]
  }
  
  max1 <- MaxDisparty(data)
  
  #Optimization by adding one seat the the state with the largest average consituency size:
  #TODO: Repeat this process until largest average consituency can't be optimized...
  ranks <- order(data[, "avg"])
  
  #Wiederholtes addieren, bis es nichtmehr kleiner wird
  
  max3 <- .Machine$integer.max
  
  while (max1 < max3) {
    max3 <- max1
    
    data[ranks[length(ranks)], "allotment"]  <- data[ranks[length(ranks)], "allotment"] + 1
    
    max2 <- 0
    bool <- 0
    
    #Achtung: Hier ist die Stelle hardcoded
    #Bei der ersten kleineren disparty wird abgebrochen!
    #Frage: Bleibt man bei mehrfachem durchlaufen hier bei der ursprünglichen Ordnung?
    for (i in 1:length(data[1, ]))  {
      if (i != ranks[length(ranks)]) {
        data[i, "allotment"] <- data[i, "allotment"] - 1
        max2 <- MaxDisparty(data)
        print("max2",  max2)
        print("max1",  max1)
        if (max2 >= max1) {
          data[i, "allotment"] <- data[i, "allotment"] + 1
          
        } else {
          max1 <- max2
          bool <- bool + 1
          break
        }
      }
    }
    if(bool == 0){
      data[ranks[length(ranks)], "allotment"]  <- data[ranks[length(ranks)], "allotment"] - 1
    }
  }
  
  return(data)

}

#Kann man evtl. optimieren!
# Größtes und kleinstes Element raussuchen und nur damit rechnen!
MaxDisparty <- function(data) {
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