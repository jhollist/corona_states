seq_double <- function(start, length){
  if(length <= 0){stop("length must be a positive number")}
  out <- vector("numeric", length)
  out[1] <- start
  for(i in seq_along(out)){
    if(i > 1){
      out[i] <- out[i-1] * 2
    }
  }
  out
}