MinimumRange <- function(p, a, h){
  
  data <- data.frame("population" = p, "allotment" = a)
  data["avg"] <- array(0, length(data[1,]))
  
  for(i in 1:length(data[, 1])){
    data[i, "avg"] <- p[i] / a[i]
  }
  
  max1 <- MaxDisparty(data)
  ranks <- order(data[, "avg"])
  
  data[ranks[1], "allotment"]  <- data[ranks[1], "allotment"] + 1
  
  max2 <- 0
  tmp <-0

  #Danach wieder eins draufrechnen, damit es insgesamt stimmt, so ziehe ich bei jeder Runde einen Sitz ab!  
  for(i in 1:length(data[, 1]) - 1){
    if(i != ranks[1]){
      data[i, "allotment"] <- data[i, "allotment"] - 1
      tmp <- MaxDisparty(data)
      if(tmp < max2){
        max2 <- tmp
      }
    }
  }
    
  max2 <- MaxDisparty(data)
  if(max)
  return()
}

MaxDisparty <- function(data){
  max.disparty <- 0
  for(i in 1:length(data[, 1])){
    for(j in 1:length(data[, 1])){
      tmp <-  data[i, "avg"] /data[j,"avg"]
      if(tmp > max.disparty){
        max.disparty <- tmp
      }
    }
  }
  return(max.disparty)
}