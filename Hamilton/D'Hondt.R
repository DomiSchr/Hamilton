# function with two parameers
# first accept with 4 colums State Poplation Seats Def
# secound the number of seats available 
DHondt <- function(dat,seats) {
  # Loop which loop for the nummer of seats avalibale 
  for (i in 1:seats) 
  {
    # Re/set of 
    nstate <- 1
    # Loop which gos over all States
    for (j in 2:nrow(dat)) 
    {
      # compers the number 
      if (dat$Def[nstate]<dat$Def[j]) 
      {
        nstate <- j
      }
    }
    #increasing the seat with the highes number
    dat$Seats[nstate] <- dat$Seats[nstate] +1 
    #defding the Population by already resived nummber of seats +1
    dat$Def[nstate] <- dat$Population[nstate]/(dat$Seats[nstate]+1)
  }
  return(dat)
}

#DHondtcv2('Test1.csv',1,7)

