divisor.methods  <- function(vector.population,integer.housesize,expr) 
{
  #  Author: Philipp Maruhn
  #
  # Args:
  #   vector.population: a vector containing the population of each state per column.
  #   integer.housesize: the house size as variable.
  #   expr: is the expration which for under the fracture
  #         requires the varible 'a' it is equel to the vector.population in the calculation
  #
  # Returns:
  #   A Vector containing the allotment of seats.
  
  CheckInput(vector.population,integer.housesize)
  
  # creates the dataframe for the function
  a <- replicate(length(vector.population), 0)
  collum.underFracture <- replicate(length(vector.population), 0)
  intrem <- replicate(length(vector.population), 0)
  dataframe.calculation <- data.frame(vector.population,a,collum.underFracture,intrem)
  # first caltulation of the expration
  dataframe.calculation[["collum.underFracture"]] <- eval(substitute(expr),dataframe.calculation,parent.frame())
  # Divieds the number of vector.population thrue the result of the expration 
  dataframe.calculation$intrem <- dataframe.calculation$vector.population / dataframe.calculation$collum.underFracture
  # Loop which loop for the nummer of integer.housesize avalibale 
  for (i in 1:integer.housesize) 
  {
    # Re/set of 
    integer.Cerrent.max <- 1
    # Loop which gos over all States
    for (j in 2:length(vector.population)) 
    {
      # compers the number form interims resuts 
      if (dataframe.calculation$intrem[integer.Cerrent.max]<dataframe.calculation$intrem[j]) 
      {
        # sets the nummer of state the the current won 
        integer.Cerrent.max <- j
      }
    }
    #increasing the seat with the highes number
    dataframe.calculation$a[integer.Cerrent.max] <- dataframe.calculation$a[integer.Cerrent.max] +1 
    #writes and caltulates the new collum.underFracture result for the State how has resived the steat
    dataframe.calculation$collum.underFracture <- eval(substitute(expr),dataframe.calculation,parent.frame())
    # Divieds the number of vector.population thrue the result of the expration 
    dataframe.calculation$intrem[integer.Cerrent.max] <- dataframe.calculation$vector.population[integer.Cerrent.max] / dataframe.calculation$collum.underFracture[integer.Cerrent.max]

  }
  return(dataframe.calculation$a)
}
  
Imperali(vector.population,106)

Imperali <-function(vector.population,integer.housesize)
{
  # Imperali method
  #
  # Args:
  #   vector.population: a vector containing the population of each state per column.
  #   integer.housesize: the house size as variable.
  # Returns:
  #   A Vector containing the allotment of seats.
  return(divisor.methods(vector.population,integer.housesize,a+2))
}
dHondt <-function(vector.population,integer.housesize)
{
  # d'Hondt method
  #
  # Args:
  #   vector.population: a vector containing the population of each state per column.
  #   integer.housesize: the house size as variable.
  # Returns:
  #   A Vector containing the allotment of seats.
  return(divisor.methods(vector.population,integer.housesize,a+1))
}
Macau <-function(vector.population,integer.housesize)
{
  # Macau method
  #
  # Args:
  #   vector.population: a vector containing the population of each state per column.
  #   integer.housesize: the house size as variable.
  # Returns:
  #   A Vector containing the allotment of seats.
  return(divisor.methods(vector.population,integer.housesize,2^a))
}
SainteLague <-function(vector.population,integer.housesize)
{
  # SainteLague method
  #
  # Args:
  #   vector.population: a vector containing the population of each state per column.
  #   integer.housesize: the house size as variable.
  # Returns:
  #   A Vector containing the allotment of seats.
  return(divisor.methods(vector.population,integer.housesize,(2*a)+1))
}
EqualProportions <-function(vector.population,integer.housesize)
{
  # EqualProportions method
  #
  # Args:
  #   vector.population: a vector containing the population of each state per column.
  #   integer.housesize: the house size as variable.
  # Returns:
  #   A Vector containing the allotment of seats.
  return(divisor.methods(vector.population,integer.housesize,'sqrt'(a*(a+1))))
}
Adams <-function(vector.population,integer.housesize)
{
  # Adams method
  #
  # Args:
  #   vector.population: a vector containing the population of each state per column.
  #   integer.housesize: the house size as variable.
  # Returns:
  #   A Vector containing the allotment of seats.
  return(divisor.methods(vector.population,integer.housesize,a))
}

