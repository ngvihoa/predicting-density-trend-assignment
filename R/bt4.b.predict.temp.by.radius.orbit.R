## ================================================================ ##
## BÀI TẬP 4B: DỰ ĐOÁN NHIỆT ĐỘ MARS THEO BÁN KÍNH CHO TỪNG QUỸ ĐẠO ##
## ================================================================##

source('R/bt3.poly.regression.R')

## Đọc dữ liệu
marsbig <- read.table("data/marsbig.dat", header = TRUE)

## Lấy danh sách các quỹ đạo
orbits <- sort(unique(marsbig$orbit))
n_orbits <- length(orbits)

## Tạo thư mục lưu kết quả nếu chưa có
if (!dir.exists("Result")) dir.create("Result")
if (!dir.exists("Result/4b")) dir.create("Result/4b")

## Lưu kết quả cho từng orbit
results <- list()

## ================================================================
## PHÂN TÍCH TỪNG QUỸ ĐẠO VÀ TẠO 7 BIỂU ĐỒ RIÊNG

for (orbit in orbits) {
  ### Lọc dữ liệu cho orbit này
  orbit_data <- marsbig[marsbig$orbit == orbit, ]
  x <- orbit_data$radius
  y <- orbit_data$temperature
  
  ### Fit model với hồi quy đa thức địa phương
  model <- poly_regression(
    x = x,
    y = y,
    p = 2,
    cv_method = "CV",
    kernel = "gaussian",
    m_method = 'locpoly_reg',
    h_grid = seq(1, 20, length.out = 50)
  )
  
  ### Dự đoán
  x_grid <- seq(min(x), max(x), length.out = 300)
  y_pred <- model$predict(x_grid)
  
  ### Lưu kết quả
  results[[orbit]] <- list(
    x = x,
    y = y,
    x_grid = x_grid,
    y_pred = y_pred,
    model = model,
    n = length(x)
  )
  
  ### =================================================
  ### TẠO BIỂU ĐỒ RIÊNG CHO ORBIT NÀY
  
  filename <- sprintf("Result/4b/orbit_%d_temperature_prediction.png", orbit)
  png(filename, width = 1200, height = 800, res = 120)
  par(mar = c(5, 5, 4, 2))
  
  plot(x, y,
       pch = 16,
       cex = 1.2,
       col = "black",
       xlab = "Radius (km)",
       ylab = "Temperature (K)",
       main = sprintf("Quỹ đạo %d: Dự đoán nhiệt độ theo bán kính\n(Local Polynomial Regression, p=%d)", 
                      orbit, model$degree),
       cex.main = 1.3,
       cex.lab = 1.2,
       cex.axis = 1.1)
  #### Vẽ đường dự đoán
  lines(x_grid, y_pred, col = "blue", lwd = 3)
  #### Legend
  legend("topright",
         legend = c(
           sprintf("Số quan sát: n = %d", length(x)),
           sprintf("Polynomial degree: p = %d", model$degree),
           sprintf("Bandwidth: h = %.4f", model$bandwidth)
         ),
         bty = "n",
         cex = 1.1)
  
  grid(col = "gray90", lty = 1)
  dev.off()
}

## ================================================================================
## TẠO BIỂU ĐỒ TỔNG HỢP CHO TẤT CẢ 7 QUỸ ĐẠO

png("Result/4b/all_orbits_comparison.png", width = 1400, height = 1000, res = 120)
par(mar = c(5, 5, 4, 8), xpd = TRUE)  # Mở rộng margin phải cho legend

## Tìm phạm vi cho trục
all_x <- unlist(lapply(results, function(r) range(r$x)))
all_y <- unlist(lapply(results, function(r) range(r$y)))

## Tạo plot trống
plot(NULL,
     xlim = range(all_x),
     ylim = range(all_y),
     xlab = "Radius (km)",
     ylab = "Temperature (K)",
     main = "So sánh xu hướng nhiệt độ theo bán kính\ngiữa 7 quỹ đạo khác nhau",
     cex.main = 1.4,
     cex.lab = 1.3,
     cex.axis = 1.1)

## Màu sắc cho từng orbit
colors <- c("red", "blue", "darkgreen", "purple", "orange", "brown", "pink")

## Vẽ dữ liệu và đường fitted cho từng orbit
for (i in seq_along(orbits)) {
  orbit <- orbits[i]
  res <- results[[orbit]]
  
  ## Vẽ điểm dữ liệu
  points(res$x, res$y, 
         pch = 16, 
         cex = 0.7, 
         col = adjustcolor(colors[i], alpha.f = 0.4))
  
  ## Vẽ đường fitted
  lines(res$x_grid, res$y_pred, 
        col = colors[i], 
        lwd = 2.5)
}

## Legend ở ngoài plot area
legend("topright",
       inset = c(-0.25, 0), 
       legend = sprintf("Orbit %d (n=%d, h=%.2f)", 
                        orbits, 
                        sapply(results, function(r) r$n),
                        sapply(results, function(r) r$model$bandwidth)),
       col = colors,
       lwd = 2.5,
       pch = 16,
       cex = 1.0,
       bg = "white",
       box.lwd = 1)

grid(col = "gray90", lty = 1)
dev.off()

## ================================================================================
## TẠO BIỂU ĐỒ LƯỚi (7 ORBITS)

png("Result/4b/orbits_grid_3x3.png", width = 1800, height = 1200, res = 120)
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))

for (i in seq_along(orbits)) {
  orbit <- orbits[i]
  res <- results[[orbit]]
  
  plot(res$x, res$y,
       pch = 16,
       cex = 0.8,
       col = colors[i],
       xlab = "Radius (km)",
       ylab = "Temperature (K)",
       main = sprintf("Orbit %d: h=%.2f, n=%d", 
                      orbit, res$model$bandwidth, res$n),
       cex.main = 1.1,
       cex.lab = 1.0)
  
  lines(res$x_grid, res$y_pred, col = colors[i], lwd = 3)
  grid(col = "gray80", lty = 1)
}

## 2 ô trống (orbit 8 và 9 không tồn tại)
for (i in (n_orbits + 1):9) {
  plot.new()
}

dev.off()
