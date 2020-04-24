population <- c(282000, 566399, 623966, 193719, 187254, 269003, 238748, 170902, 112772, 176353, 452159, 498135, 108700, 259404, 302755, 134443, 239611, 160418, 74534)

data <- MinimumRangeNeu(population, 169)
data
sum(data[, "allotment"])

population2 <- c(
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

data <- MinimumRange(population2, 106)
# ranks <- order(data[, "avgconstituency"])
# data[ranks[length(ranks)],"avgconstituency"] / data[ranks[1],"avgconstituency"]
data

sum(data2[, "allotment"])

test <- Hamilton(population, 160)
test

RangeHouseSizes(Utilitarian, population, 160, 165, 2)


max(allotment)


a <- 1
b <- "Test"  

test <- function(a, b){
tryCatch({
    c <- a + b
    return(c)
    },
    
    error = function(e){
      c <- "Geht nicht"
      return(c)
    }
)
}

test(a, b)
  
  
  



