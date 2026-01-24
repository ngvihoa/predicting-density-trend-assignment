
gaussian_kernel <- function(u) {
  (1 / sqrt(2 * pi)) * exp(-0.5 * u^2)
}

epanechnikov_kernel <- function(u) {
  ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
}

uniform_kernel <- function(u) {
  ifelse(abs(u) <= 1, 0.5, 0)
}

triangle_kernel <- function(u) {
  ifelse(abs(u) <= 1, 1 - abs(u), 0)
}

quartic_kernel <- function(u) {
  ifelse(abs(u) <= 1, (15 / 16) * (1 - u^2)^2, 0)
}

triweight_kernel <- function(u) {
  ifelse(abs(u) <= 1, (35/ 32) * (1 - u^2)^3, 0)
}

tricube_kernel <- function(u) {
  ifelse(abs(u) <= 1, (70/ 81) * (1 - abs(u)^3)^3, 0)
}

cosine_kernel <- function(u) {
  ifelse(abs(u) <= 1, (pi/ 4) * cospi(u / 2), 0)
}

get_kernel_fun <- function(name = 'gaussian') 
  switch(name,
   "gaussian" = gaussian_kernel,
   "epanechnikov" = epanechnikov_kernel,
   "uniform" = uniform_kernel,
   "triangle" = triangle_kernel,
   "quartic" = quartic_kernel,
   "triweight" = triweight_kernel,
   "tricube" = tricube_kernel,
   "cosine" = cosine_kernel,
   stop("Kernel không hợp lệ"))

