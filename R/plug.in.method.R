## Plug-in rule of thumb >> R/plug.in.method.R
ROT <- function(x, kernel = c("gaussian", "epanechnikov")) {
  n <- length(x)
  kernel <- match.arg(kernel)
  
  # hệ số theo kernel
  Ck <- switch(kernel,
               gaussian = 1.06,
               epanechnikov = 2.34)
  
  # tính bandwidth
  h <- Ck * sd(x) * n^(-1/5)
  
  return(h)
}