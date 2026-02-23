# Kernel functions 
bartlett <- function(u){
  if(abs(u)<=1){
    w <- 1 - abs(u)
  } else{
    w <-0
  }
  return(w)
}

epanechnikov <- function(u) {
  if(abs(u)<=1){
    w <- exp(-u^2/2)/sqrt(2*pi)
  } else{
    w <-0
  }
  return(w)
}


normal <- function(u) {
  w <- exp(-u^2/2)/sqrt(2*pi)
  return(w)
}

rectangular <- function(u) {
  if(abs(u)<=1){
    w <- 1
  } else{
    w <-0
  }
  return(w)
}



# Main Function 
ts_kernel_smoother <- function(x, b, kernel, ...) {
  time_points <- time(x)
  
  if(kernel == "Bartlett"){
    my_kernel = bartlett
  } else if(kernel == "Epanechnikov"){
    my_kernel = epanechnikov
  } else if(kernel == "Normal"){
    my_kernel = normal
  } else if(kernel == "Rectangular"){
    my_kernel = rectangular
  } else {
    my_kernel = bartlett
    warning("Please supply kernel as: Bartlett, Epanechniov, Normal, or Rectangular. Input not valid, Bartlett was used. ")
  }
  
  smooth <- sapply(time_points, function(one_point) {
    weights <- my_kernel((time_points - one_point) /b)
    sum(weights *x) / sum(weights)
  })
  
  plot(x, ...)
  lines(time_points, smooth, col = "blue", lwd = 2)
}
