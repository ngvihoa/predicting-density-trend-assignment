
source('R/kernel.functions.R')

kde <- function(kernel = 'gaussian', h, y){
  # Ép về vector nếu không phải
  y <- as.vector(y)
  data <- as.vector(data)
  
  kernel_fun <- get_kernel_fun(name = kernel)
  
  colMeans(kernel_fun(outer(y, data, "-")/h)) / h
}