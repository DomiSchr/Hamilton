# Start the Function with LeximinMethod(p, H)
# p is a vector, which represents the population sizes for each stateLe
# H is the House Size
# The function returns a vector, which represents the seat count for each state

Leximin <- function(p, H, quota) {
  A <- sum(p) / H
  a.share <- p / A  
  a <- p - p
  
  # sets a to lower quota
  for (i in 1:length(a)) {
    a[i] <- floor(a.share[i])
  }
  
  # Calculates the depature of the apportionment a
  CalcD <- function(p, a, A) {
    delta <- ((p / a) - A) / A
    d <- ifelse(delta < 0, delta * (-1), delta)
  }
  a <- ifelse( CalcD(p, a, A) > CalcD(p, a + 1, A), a + 1, a)
  
  # Adds Seats until sum(a) euqals the House size
  if (sum(a) < H) {
    while (sum(a) < H) {
      d <- CalcD(p, a + 1, A)
      
      for(i in 1:length(d)) {
        if (d[i] == min(d)) {
          pos <- i
          break
        }
      }
      a[pos] <- a[pos] + 1
    }
  } 
  # Removes Seats until sum(a) euqals the House size
  else {
    while (sum(a) > H) {
      d <- CalcD(p, a - 1, A)
      
      for(i in 1:length(d)) {
        if (d[i] == min(d)) {
          pos <- i
          break
        }
      }
      a[pos] <- a[pos] - 1
    }
  }
  return (a)
}
