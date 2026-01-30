## ================ ##
## BÀI TẬP 5D       ##
## ================ ##

source("R/bt2.cross.validation.R")

n_sim <- 100

# chuẩn bị output dataframe
results <- data.frame(
  iter = 1:n_sim,
  h_rot_epan = NA_real_,
  h_cv_epan  = NA_real_,
  h_gcv_epan = NA_real_,
  h_rot_gau  = NA_real_,
  h_cv_gau   = NA_real_,
  h_gcv_gau  = NA_real_
)

# dải h bạn đã dùng trước
h_plot_gau   <- seq(0.1, 1, length.out = 41)
h_plot_epan  <- seq(0.1, 1, length.out = 41)

pb <- txtProgressBar(min = 1, max = n_sim, style = 3)
for (i in 1:n_sim) {
  tryCatch({
    ## --- (a) Sinh dữ liệu như bạn mô tả ---
    n <- 100
    x <- runif(n, min = 1e-6, max = pi)
    m_fun <- function(x) 1 + sin(x^2) / (x^2)
    mu <- m_fun(x)
    sd_eps <- sqrt(mu^2 / 68)
    eps <- rnorm(n, mean = 0, sd = sd_eps)
    y <- mu + eps
    
    ## --- (b) Tính các h cho cả 2 kernel ---
    # ROT
    h_rot_gau  <- tryCatch(ROT(x, kernel = "gaussian"), error = function(e) NA_real_)
    h_rot_epan <- tryCatch(ROT(x, kernel = "epanechnikov"), error = function(e) NA_real_)
    
    # CV (chọn h trong h_plot)
    cv_gau   <- tryCatch(CV_loclin(x = x, y = y, h = h_plot_gau), error = function(e) rep(NA_real_, length(h_plot_gau)))
    h_cv_gau <- if (all(is.na(cv_gau))) NA_real_ else h_plot_gau[which.min(cv_gau)]
    
    cv_epan  <- tryCatch(CV_loclin(x = x, y = y, h = h_plot_epan, kernel = "epanechnikov"),
                         error = function(e) rep(NA_real_, length(h_plot_epan)))
    h_cv_epan <- if (all(is.na(cv_epan))) NA_real_ else h_plot_epan[which.min(cv_epan)]
    
    # GCV
    gcv_gau   <- tryCatch(GCV_loclin(x = x, y = y, h = h_plot_gau), error = function(e) rep(NA_real_, length(h_plot_gau)))
    h_gcv_gau <- if (all(is.na(gcv_gau))) NA_real_ else h_plot_gau[which.min(gcv_gau)]
    
    gcv_epan  <- tryCatch(GCV_loclin(x = x, y = y, h = h_plot_epan, kernel = "epanechnikov"),
                          error = function(e) rep(NA_real_, length(h_plot_epan)))
    h_gcv_epan <- if (all(is.na(gcv_epan))) NA_real_ else h_plot_epan[which.min(gcv_epan)]
    
    # lưu kết quả
    results[i, "h_rot_epan"] <- h_rot_epan
    results[i, "h_cv_epan"]  <- h_cv_epan
    results[i, "h_gcv_epan"] <- h_gcv_epan
    results[i, "h_rot_gau"]  <- h_rot_gau
    results[i, "h_cv_gau"]   <- h_cv_gau
    results[i, "h_gcv_gau"]  <- h_gcv_gau
    
  }, error = function(e){
    # Nếu lỗi không mong muốn xảy ra, ghi NA cho dòng đó (vòng lặp vẫn tiếp tục)
    results[i, 2:ncol(results)] <<- NA_real_
    message(sprintf("Warning: iteration %d failed: %s", i, e$message))
  })
  setTxtProgressBar(pb, i)
}
close(pb)

# Hiển thị 1 vài dòng đầu
head(results)

# ------- VẼ đồ thị: 6 đường thể hiện sự thay đổi của các bandwidth -------
cols <- c("red", "blue", "green", "purple", "orange", "brown")
matplot(
  results$iter,
  results[, -1],
  type = "l",
  lty = 1,
  lwd = 2,
  col = cols,
  xlab = "Lần xuất hiện (1-100)",
  ylab = "Băng thông h",
  main = "Sự thay đổi các băng thông tối ưu qua 100 mô phỏng"
)
grid()
legend("topright",
       legend = c("h_rot_epan", "h_cv_epan", "h_gcv_epan",
                  "h_rot_gau", "h_cv_gau", "h_gcv_gau"),
       col = cols, lty = 1, lwd = 2, cex = 0.8)



# Kết
summary_stats <- data.frame(
  method = colnames(results)[-1],
  mean = sapply(results[, -1], mean, na.rm = TRUE),
  sd   = sapply(results[, -1], sd, na.rm = TRUE),
  median = sapply(results[, -1], median, na.rm = TRUE)
)
print(summary_stats)
