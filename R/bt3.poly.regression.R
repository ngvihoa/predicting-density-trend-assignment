source('R/bt2.cross.validation.R')

poly_regression <- function(x, y, p = 1, h = NULL, method = c("CV", "GCV"),  m_method = c('kernel_reg', 'loclin_reg')) {
  method <- match.arg(method)
  m_method <- match.arg(m_method)
  
  if (is.null(h)) {
    h_plot <- seq(0.1, 4, length.out = 41)
    cv_errors <- cross_validation(x, y, h = h_plot, cv_method = method, m_method = m_method)   
    h <- h_plot[which.min(cv_errors)]
  }
  
  X <- sapply(0:p, function(k) x^k)
  beta_hat <- solve(t(X) %*% X) %*% (t(X) %*% y)
  
  predict_fun <- function(newx) {
    newX <- sapply(0:p, function(k) newx^k)
    as.vector(newX %*% beta_hat)
  }
  
  return(list(
    coefficients = beta_hat,
    bandwidth = h,
    degree = p,
    method = method,
    predict = predict_fun
  ))
}