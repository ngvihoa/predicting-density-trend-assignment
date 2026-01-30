## =========================================== ##
## BÀI TẬP 3: HIỆN THỰC HÀM CROSS VALIDATION   ##
## =========================================== ##

source('R/guest.data.trend.R')
source('R/bt2.cross.validation.R')

poly_regression <- function(x, y, p = 0, h = NULL, 
                            kernel = "gaussian",
                            cv_method = c("CV", "GCV"),
                            h_grid = seq(0.1, 4, length.out = 41),
                            m_method = c('loclin_reg', 'kernel_reg', 'locpoly_reg')) {
  
  cv_method <- match.arg(cv_method)
  m_method <- match.arg(m_method)
  
  if (is.null(h)) {
    cv_errors <- cross_validation(x, y, h = h_grid, p = p, kernel = kernel, cv_method = cv_method)   
    h <- h_grid[which.min(cv_errors)]
  }
  
  m_fun <- data_trend_fun(fun = m_method, p = p)
  
  predict_fun <- function(x_eval) {
    m_fun(
      x = x,
      y = y,
      x_eval = x_eval,
      h = h,
      kernel = kernel
    )
  }
  
  return(list(
    bandwidth = h,
    degree = p,
    kernel = kernel,
    cv_method = cv_method,
    predict = predict_fun
  ))
}