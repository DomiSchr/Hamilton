#Hamilton-Method for the Apportionment Problem

hamilton <- function(p, H){
  
  P <- sum(p)
  A <- P/H
  
  share <- array(0, length(p))
  share_floor <- array(0, length(p))
  fraction <- array(0, length(p))
  
  
  for(i in 1:length(p)){
    share[i] <- p[i]/A
    share_floor[i] <- floor(share[i])
    fraction[i] <- share[i] - share_floor[i]
  }
  
  
  
  
  return(sum(share_floor))
}


p <- c(630, 480, 390, 500)

hamilton(p, 10)
