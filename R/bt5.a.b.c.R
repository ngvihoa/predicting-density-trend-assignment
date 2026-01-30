## ================ ##
## BÀI TẬP 5 A-B-C  ##
## ================ ##

source('R/plug.in.method.R')

##================##
## Câu a          ##
##================##
n <- 100

# Sinh X_i ~ U(0, pi), tránh đúng bằng 0
x <- runif(n, min = 1e-6, max = pi)

# Định nghĩa hàm m(x)
m_fun <- function(x) 1 + sin(x^2) / (x^2)

# Giá trị thực m(X_i)
mu <- m_fun(x)

# Sai số chuẩn theo đề: sd = sqrt(m^2 / 64) = m / 8
sd_eps <- sqrt(mu^2/68)   # abs để chắc chắn dương (mu gần luôn >0 ở vùng này)

# Sinh epsilon (rnorm chấp nhận vector mean/sd, qsẽ recycle theo n)
eps <- rnorm(n, mean = 0, sd = sd_eps)

# Quan sát Y
y <- mu + eps

# Lưu vào data.frame và xem kết quả
df <- data.frame(x = x, y = y, mu = mu, sd_eps = sd_eps, eps = eps)
head(df)


# vẽ điểm
plot(df$x, df$y, pch = 19, cex = 0.6, xlab = "X", ylab = "Y",
     main = "Mẫu mô phỏng: Y = m(X) + eps")

# vẽ đường m(x) mịn theo lưới
x_grid <- seq(1e-6, pi, length.out = 400)
lines(x_grid, m_fun(x_grid), lwd = 2)
legend("topright", legend = "m(x)", lwd = 2, bty = "n")

  
##================##
## Câu b          ##
##================##
## Với x, y được khởi tạo như trên tìm băng thông h theo *plug-in, cv, gcv bằng locallin

source(file = "R/bt2.cross.validation.R")
## Gaussian
### ROT
h_rot_gau <- ROT(x, kernel =  "gaussian")

h_plot <- seq(0.1, 1, length.out = 41)
### CV
cv_gau <- CV_loclin(x = x, y = y, h = h_plot)
plot(x = h_plot, y = cv_gau, type = "b", pch = 16)
h_CV_gau <- h_plot[which.min(cv_gau)]

### GCV
gcv_gau <- GCV_loclin(x = x, y = y, h = h_plot)
plot(x = h_plot, y = gcv_gau, type = "b", pch = 16)
h_GCV_gau <- h_plot[which.min(gcv_gau)]


## Epanechnikov
### ROT
h_rot_epan <- ROT(x, kernel =  "epanechnikov")

### CV
cv_epan <- CV_loclin(x = x, y = y, h = h_plot, kernel = "epanechnikov")
plot(x = h_plot, y = cv_epan, type = "b", pch = 16)
h_CV_epan <- h_plot[which.min(cv_epan)]

### GCV
gcv_epan <- GCV_loclin(x = x, y = y, h = h_plot, kernel = "epanechnikov")
plot(x = h_plot, y = gcv_epan, type = "b", pch = 16)
h_GCV_epan <- h_plot[which.min(gcv_epan)]


##================##
## Câu c          ##
##================##

#Tính tại 1 điểm 0.5
x0 <- 0.5

result_df <- data.frame(
  bandwidth = c(
    "h_rot_epan", "h_CV_epan", "h_GCV_epan",
    "h_rot_gau",  "h_CV_gau",  "h_GCV_gau"
  ),
  m_hat = c(
    local_linear_regression(x, y, x0, h_rot_epan, "epanechnikov"),
    local_linear_regression(x, y, x0, h_CV_epan,  "epanechnikov"),
    local_linear_regression(x, y, x0, h_GCV_epan, "epanechnikov"),
    local_linear_regression(x, y, x0, h_rot_gau,  "gaussian"),
    local_linear_regression(x, y, x0, h_CV_gau,   "gaussian"),
    local_linear_regression(x, y, x0, h_GCV_gau,  "gaussian")
  )
)

result_df

# Câu d lặp 100 lần câu a và b