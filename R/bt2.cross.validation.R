source('R/guest.data.trend.R')
source('R/kernel.functions.R')

## Cross-validation
### CV with kernel regression
CV_kernel <- function(x, y, h, kernel = 'gaussian'){
  m_fun = data_trend_fun(fun = 'kernel_reg')
  n <- length(x)
  err <- numeric(n)
  for(i in 1:n) {
    x_new <- x[-i]
    y_new <- y[-i]
    y_i <- m_fun(x = x_new, y = y_new, x_eval = x[i], h = h, kernel = kernel)
    
    err[i] <- (y[i] - y_i)^2
  }
  
  cv_errors <- mean(err)
  return(cv_errors)
}

CV_kernel <- Vectorize(FUN=CV_kernel, vectorize.args = "h")

### GCV with kernel regression
GCV_kernel <- function(x, y, h, kernel = "gaussian") {
  m_fun = data_trend_fun(fun = 'kernel_reg')
  kernel_fun = get_kernel_fun(name = kernel)
  n <- length(y)
  W_diag <- numeric(n)
  
  for (i in 1:n) {
    u <- (x - x[i]) / h
    kernel_res <- kernel_fun(u)
    W_diag[i] <- kernel_res[i] / sum(kernel_res)
  }
  
  m_hat <- m_fun(x = x, y = y, x_eval = x, h = h, kernel = kernel)
  gcv <- sum((y - m_hat)^2) / (n - sum(W_diag))^2
  return(gcv)
}

GCV_kernel <- Vectorize(FUN = GCV_kernel, vectorize.args = "h")


### CV with local linear regression
CV_loclin <- function(x, y, h, kernel = "gaussian") {
  m_fun = data_trend_fun(fun = 'loclin_reg')
  kernel_fun = get_kernel_fun(name = kernel)
  n <- length(y)
  W_diag <- numeric(n)
  for (i in 1:n) {
    u <- x - x[i]
    kernel_res <- kernel_fun(u/h)
    a0 <- mean(kernel_res)
    a1 <- mean(kernel_res * u)
    a2 <- mean(kernel_res * u^2)
    W_diag[i] <- (a2 * kernel_res[i])/(a2 * a0 - a1^2)/n
  }
  m_hat <- m_fun(x = x, y = y, x_eval = x, h = h, kernel = kernel)
  cv <- mean(((y - m_hat)/(1 - W_diag))^2)
  return(cv)
}

CV_loclin <- Vectorize(FUN = CV_loclin, vectorize.args = "h")


### GCV with local linear regression
GCV_loclin <- function(x, y, h, kernel = "gaussian") {
  m_fun = data_trend_fun(fun = 'loclin_reg')
  kernel_fun = get_kernel_fun(name = kernel)
  n <- length(y)
  W_diag <- numeric(n)
  for (i in 1:n) {
    u <- x - x[i]
    kernel_res <- kernel_fun(u/h)
    a0 <- mean(kernel_res)
    a1 <- mean(kernel_res * u)
    a2 <- mean(kernel_res * u^2)
    W_diag[i] <- (a2 * kernel_res[i])/(a2 * a0 - a1^2)/n
  }
  m_hat <- m_fun(x = x, y = y, x_eval = x, h = h, kernel = kernel)
  gcv <- sum((y - m_hat)^2)/(n - sum(W_diag))^2
  return(gcv)
}

GCV_loclin <- Vectorize(FUN = GCV_loclin, vectorize.args = "h")

match_cv <- function(cv_method = c("CV", "GCV"), m_method = c('loclin_reg', 'kernel_reg')) {
  cv_method <- match.arg(cv_method)
  m_method <- match.arg(m_method)
  
  if(cv_method == "CV" && m_method == 'kernel_reg'){
    return(CV_kernel)
  }
  if(cv_method == "GCV" && m_method == 'kernel_reg'){
    return(GCV_kernel)
  }
  if(cv_method == "CV" && m_method == 'loclin_reg'){
    return(CV_loclin)
  }
  return(GCV_loclin)
}

### Main cross-validation
cross_validation <- function(x, y, h, 
                             kernel = "gaussian", 
                             cv_method = c("CV", "GCV", "Both"), 
                             m_method = c('loclin_reg', 'kernel_reg')) {
  
  if (cv_method == "CV" || cv_method == "Both") {
    cv_function <- match_cv(cv_method='CV', m_method=m_method)
    cv_error <- cv_function(x, y, h, kernel) 
  }
  
  if (cv_method == "GCV" || cv_method == "Both") {
    gcv_function <- match_cv(cv_method='GCV', m_method=m_method)
    gcv_error <- gcv_function(x, y, h, kernel)  
  }
  
  if (cv_method == "CV") return(cv_error)
  if (cv_method == "GCV") return(gcv_error)
  if (cv_method == "Both") return(list(CV = cv_error, GCV = gcv_error))
  
}

cross_validation <- Vectorize(FUN=cross_validation, vectorize.args = 'h')



