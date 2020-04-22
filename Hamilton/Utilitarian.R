Utilitarian <- function(p,h){
  if (h <= 0){
    stop("house size cannot be smaller than 1")
  }
  if (length(p) < 2){
    stop("Population Vector must hold at least 2 Populations")
  }
  # total Population
  integer.P <- sum(p)
  # avarage integer.houseSize
  double.A <- integer.P / h     
  # vector of computed shares
  Vector.Share <- vector()
  vector.departure <- vector()
  for (value in p) {
    double.share <- value / double.A
    integer.helper <- ceiling(double.share)
    double.fractionalPart <- double.share - floor(double.share)
    if (double.fractionalPart >= 0.5){
      Vector.Share <- c(Vector.Share, ceiling(double.share))
      vector.departure <- c(vector.departure, (ceiling(double.share) - double.share))
    }else{
      Vector.Share <- c(Vector.Share, floor(double.share))
      vector.departure <- c(vector.departure, (floor(double.share) - double.share))
    }
  }
  vector.alreadyChanged <- vector()
  
  for (value in Vector.Share){
    vector.alreadyChanged <- c(vector.alreadyChanged, FALSE)
  }
  if (sum(Vector.Share) == h){
    return (Vector.Share)
  }else if (sum(Vector.Share) < h){
    # add seats which increase T the least
    while(sum(Vector.Share) < h){
      #add Seats to share which increases Summ of departure the least
      
      #search for smallest departure and add one seat
      integer.smallestDeparture <- 1.0
      index.smallestDeparture <- 1
      for (i in 1:length(vector.departure)){
        if (vector.departure[i] <= integer.smallestDeparture){
          index.smallestDeparture <- i
          integer.smallestDeparture <- vector.departure[i]
        }
      }
      vector.departure[index.smallestDeparture] <- vector.departure[index.smallestDeparture] + 1.0
      Vector.Share[index.smallestDeparture] <- Vector.Share[index.smallestDeparture] + 1
    }
    return (Vector.Share)
  }else{
    # remove seats which increase T the least
    while(sum(Vector.Share) > h){
     #remove seats from share which increases Summ of departure the least
      
      # search for biggest departure and remove one seat
      integer.biggestDeparture <- 0
      index.biggestDeparture <- 1
      for (i in 1:length(vector.departure)){
        if (vector.departure[i] >= integer.biggestDeparture){
          index.biggestDeparture <- i
          integer.biggestDeparture <- vector.departure[i]
        }
      }
      vector.departure[index.biggestDeparture] <- vector.departure[index.biggestDeparture] - 1.0
      Vector.Share[index.biggestDeparture] <- Vector.Share[index.biggestDeparture] - 1
      
    }
    return (Vector.Share)
  }
}