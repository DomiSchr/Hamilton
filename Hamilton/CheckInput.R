CheckInput <- function(vector.population, integer.housesize) {
  #  Validates the input for population vectors and the housesize
  #  Author: Dominik Schröder
  #
  # Args:
  #   vector.population: a vector containing the population of each state per column.
  #   integer.housesize: the house size as variable.
  #
  
  if (typeof(integer.housesize) != "double" ||
      integer.housesize %% 1 != 0) {
    stop("House size must be an integer value!")
  }
  
  if (integer.housesize < 1) {
    stop("House size cannot be less than 1!")
  }
  
  if (integer.housesize < length(vector.population)) {
    stop("House size cannot be smaller than number of states!")
  }
  
  for (i in 1:length(vector.population)) {
    tmp <- vector.population[i]
    
    if (is.null(tmp)) {
      stop("empty row")
    }
    
    if (typeof(tmp) != "double" || tmp %% 1 != 0) {
      stop("Population size must be an integer value!")
    }
    
    if (tmp < 0) {
      stop("Population size cannot be a negative value!")
    }
  }
}