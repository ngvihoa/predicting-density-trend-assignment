## ================================================ ##
## BÀI TẬP 4A: DỰ ĐOÁN NHIỆT ĐỘ MARS THEO BÁN KÍNH  ##
## ================================================ ##

source('R/bt3.poly.regression.R')

## Tạo thư mục lưu kết quả nếu chưa có
if (!dir.exists("Result")) dir.create("Result")
if (!dir.exists("Result/4a")) dir.create("Result/4a")

## Đọc dữ liệu
mars <- read.table("data/mars.dat", header = TRUE)
x <- mars$radius
y <- mars$temperature

x_grid <- seq(min(x), max(x), length.out = 300)

## model dự đoán với bandwidth tự động
model <- poly_regression(
  x = x,
  y = y,
  p = 2,
  cv_method = "CV",
  kernel = "gaussian",
  m_method = 'locpoly_reg',
  h_grid = seq(0.1, 4, length.out = 50)
)
y_pred <- model$predict(x_grid)

## Tạo đồ thị
png("Result/4a/mars_temperature_prediction.png", width = 1200, height = 800, res = 120)
par(mar = c(5, 5, 4, 2))

plot(x, y,
     pch = 16,
     cex = 1.2,
     col = "black",
     xlab = "Radius (km)",
     ylab = "Temperature (K)",
     main = "Dự đoán nhiệt độ sao Hỏa theo bán kính\n(Local Polynomial Regression)",
     cex.main = 1.3,
     cex.lab = 1.2,
     cex.axis = 1.1)

## Vẽ đường dự đoán
lines(x_grid, y_pred, col = "blue", lwd = 3)

## Legend
legend("topright",
       legend = c(
         "Dữ liệu quan sát",
         paste0("Polynomial degree: p = ", model$degree),
         paste0("Bandwidth: h = ", round(model$bandwidth, 2))
       ),
       col = c("black", "blue", "blue"),
       pch = c(16, NA, NA),
       lty = c(NA, 1, NA),
       lwd = c(NA, 3, NA),
       bty = "n",
       cex = 1.1)

## Grid nền
grid(col = "gray90", lty = 1)

dev.off()



