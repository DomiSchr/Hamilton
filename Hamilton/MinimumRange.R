MinimumRange <- function(p, a) {
  data <- data.frame("population" = p, "allotment" = a)
  data["avg"] <- array(0, length(data[, 1]))
  
  for (i in 1:length(data[, 1])) {
    data[i, "avg"] <- p[i] / a[i]
  }
  
  max1 <- MaxDisparty(data)
  
  #Optimization by adding one seat the the state with the largest average consituency size:
  #TODO: Repeat this process until largest average consituency can't be optimized...
  ranks <- order(data[, "avg"])
  
  data[ranks[length(ranks)], "allotment"]  <- data[ranks[length(ranks)], "allotment"] + 1
  
  max2 <- 0
  bool <- 0
  
  for (i in 1:length(data[, 1]) - 1) {
    if (i != ranks[1]) {
      data[i, "allotment"] <- data[i, "allotment"] - 1
      max2 <- MaxDisparty(data)
      if (max1 < max2) {
        bool <- 1
        break
      }
      data[i, "allotment"] <- data[i, "allotment"] + 1
    }
  }
  
  if(bool == 0){
    return(data)
  }
  
  #Optimization by adding one seat the the state with the largest average consituency size:
  ranks <- order(data[, "avg"])
  
  data[ranks[1], "allotment"]  <- data[ranks[1], "allotment"] + 1
  
  max2 <- 0
  bool <- 0
  
  for (i in 1:length(data[, 1]) - 1) {
    if (i != ranks[1]) {
      data[i, "allotment"] <- data[i, "allotment"] - 1
      max2 <- MaxDisparty(data)
      if (max1 < max2) {
        bool <- 1
        break
      }
      data[i, "allotment"] <- data[i, "allotment"] + 1
    }
  }
  
  if(bool == 0){
    return(max1)
  }
  
  

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