
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

popu <- c(630, 480, 390, 500)
allot <- Hamilton(popu,10)
MinimumRange(popu, allot, 10)


t <- data.frame("a" = c("a", "b", "c"), "b" = c(1, 2, 3))
t

t["c"] <- cbind(replicate(1, t[["b"]]))
as.vector(t[["c"]])


pop <- c(
  1328726,
  309694,
  424057,
  291425,
  539907,
  335810,
  344872,
  360048,
  435284,
  245305,
  309838,
  248455,
  160796,
  1008073,
  255831,
  459677,
  186034,
  207573,
  288071,
  228583
)

Hamilton(pop,106, 3)
