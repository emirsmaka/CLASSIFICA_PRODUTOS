out_total <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

out_superior <- function(x){
  return(x > quantile(x, 0.75) + 1.5 * IQR(x))
}

out_inferior <- function(x){
  x < quantile(x, 0.25) - 1.5 * IQR(x)
}
