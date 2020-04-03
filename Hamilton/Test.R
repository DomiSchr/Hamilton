
states <- c("a", "b", "c")
population <- c(630, 480, 390)

data <- data.frame(states, population)

test <- function(x, y = 0){
  if(!missing(y)){
    x <- 0
    for(i in x:y){
      x <- x + 1
    } 
  }
  return(x)
}


test(3)

h <- 10
h2 <- 12

Hamilton(c(-630, 480, 390, 500),10, 1)
