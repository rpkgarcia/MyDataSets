# Kernel functions 
bartlett <- function(u){
  ifelse(abs(u)<=1, 1 - abs(u), 0)
}

epanechnikov <- function(u) {
  ifelse(abs(u)<=1,0.75*(1-x^2), 0)
}


normal <- function(u) {
  w <- exp(-u^2/2)/sqrt(2*pi)
  return(w)
}

rectangular <- function(u) {
  ifelse(abs(u)<=1, 1, 0)
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
    warning("Please supply kernel as: 'Bartlett', 'Epanechniov', 'Normal', or 'Rectangular'. Input not valid, Bartlett was used. ")
  }
  
  if(missing(b)){
    warning('Argument b is missing. Selected b = 1.')
    b <- 1
  } else if (b <=0){
    warning('Argument b should be larger than 0. Used |b|.')
    b <- abs(b)
  } 
   
  smooth <- sapply(time_points, function(one_point) {
    weights <- my_kernel((time_points - one_point) /b)
    sum(weights *x) / sum(weights)
  })
  
  plot(time_points, x, ...)
  lines(time_points, smooth, type = "l", col = "blue", lwd = 2)
}

