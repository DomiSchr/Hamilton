#Hamilton-Method for the Apportionment Problem

hamilton <- function(p, H){
  P <- sum(p2[,"population"])
  A <- P/H
  
  share <- array(0, length(p))
  share_floor <- array(0, length(p))
  fraction <- array(0, length(p))
  
  p[["share"]] <- share
  p[["share_floor"]] <- share_floor
  p[["fraction"]] <- fraction
  
  for(i in 1:length(p[["population"]])){
    p[i,"share"] <- p[i,"population"]/A
    p[i, "share_floor"] <- floor(p[i,"share"])
    p[i, "fraction"] <-  p[i,"share"] - p[i, "share_floor"]
    
  }
  return(p)
  
  if(sum(p[, "share_floor"]) == H){
    return(p)
  } else {
   #TODO: Hier den Spaß weiter!! Sortieren und so!
  }
  
}

p2 <- data.frame("state"=c("A", "B", "C", "D"), "population" = c(630, 480, 390, 500))

hamilton(p2, 10)
