MinimumRange <- function(p, a, h){
  
  data <- data.frame("population" = p, "allotment" = a)
  data["avg"] <- array(0, length(data[1,]))
  
  for(i in 1:length(data[, 1])){
    data[i, "avg"] <- p[i] / a[i]
  }
  
  #Caclulate the maximum disparty:
  max.disparty <- 0
  for(i in 1:length(data[, 1])){
    for(j in 1:length(data[, 1])){
      tmp <-  data[i, "avg"] /data[j,"avg"]
      if(tmp > max.disparty){
        max.disparty <- tmp
      }
    }
  }
    
  ranks <- order(data[, "avg"])
  
  
  return(max.disparty)
}