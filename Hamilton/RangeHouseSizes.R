RangeHouseSizes <- function(FUN, p, h1, h2) {
  output <- data.frame()
  for(i in h1:h2){
    tmp <- FUN(p, i, mode)
    output[paste(i)]  <- tmp
  }
}