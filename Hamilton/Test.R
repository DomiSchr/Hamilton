
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

Hamilton(c(630, 480, 390, 500),10, 3)


t <- data.frame("a" = c("a", "b", "c"), "b" = c(1, 2, 3))
t

t["c"] <- cbind(replicate(1, t[["b"]]))
as.vector(t[["c"]])
