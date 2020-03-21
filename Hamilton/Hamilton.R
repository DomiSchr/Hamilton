#Hamilton-Method for the Apportionment Problem

hamilton <- function(p, H) {
  P <- sum(p2[, "population"])
  A <- P / H
  
  share <- array(0, length(p))
  share_floor <- array(0, length(p))
  fraction <- array(0, length(p))
  
  p[["share"]] <- share
  p[["share_floor"]] <- share_floor
  p[["fraction"]] <- fraction
  
  for (i in 1:length(p[["population"]])) {
    p[i, "share"] <- p[i, "population"] / A
    p[i, "share_floor"] <- floor(p[i, "share"])
    p[i, "fraction"] <-  p[i, "share"] - p[i, "share_floor"]
    
  }
  
  if (sum(p[, "share_floor"]) == H) {
    return(p)
  }
  
  p[["result"]] <- replicate(1, p[["share_floor"]])
  
  ranks <- order(p$fraction, decreasing = TRUE)
  for (i in 1:(H - sum(p[, "share_floor"]))) {
    p[[ranks[i], "result"]] <-  p[[ranks[i], "result"]] + 1
  }
  return(p)
}

p2 <-
  data.frame("state" = c("A", "B", "C", "D"),
             "population" = c(630, 480, 390, 500))

hamilton(p2, 10)
