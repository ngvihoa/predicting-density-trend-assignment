# Các functions xác định xu hướng
source('R/kernel.functions.R')

## KNN method ---

## Kernel regression (Nadaraga-Watson)
kernel_regression <- function(x, y, x_eval, h, kernel = 'gaussian') {
  wi <- get_kernel_fun(name = kernel)((x - x_eval) / h)
  wi_sum <- sum(wi)
  
  w <- wi / wi_sum
  m_hat <- sum(y * w)
}

kernel_regression <- Vectorize(FUN=kernel_regression, vectorize.args = 'x_eval')

## Local Linear Regression
local_linear_regression <- function(x, y, x_eval, h, kernel = "gaussian"){
  kernel_fun <- get_kernel_fun(name = kernel)
  
  u <- (x - x_eval)
  kernel_res <- kernel_fun(u / h)
  
  s0 <- mean(kernel_res)
  s1 <- mean(kernel_res * u)
  s2 <- mean(kernel_res * u^2)
  
  w <- ((s2 - s1 * u) * kernel_res)/(s2 * s0 - s1^2)
  m_hat <- mean(w * y)
  return(m_hat)
}

local_linear_regression <- Vectorize(FUN=local_linear_regression, vectorize.args = "x_eval")


## Hàm tổng dự đoán xu hướng
data_trend_fun <- function(fun = c('loclin_reg', 'kernel_reg')){
  fun <- match.arg(fun)
  switch (fun,
    'kernel_reg'= kernel_regression,
    'loclin_reg' = local_linear_regression,
    stop("Method không hợp lệ")
  )
}



