## Đọc dữ liệu
marsbig <- read.table("data/marsbig.dat", header = TRUE)

orbits <- sort(unique(marsbig$orbit))


colors <- c("red", "blue", "darkgreen", "purple", "orange", "brown", "pink")
par(bg = "white")  # hoặc "gray95" để dịu mắt

plot(NULL, xlim = range(marsbig$pressure), ylim = range(marsbig$temperature),
     xlab = "Pressure (Pa)", ylab = "Temperature (K)",
     main = "So sánh xu hướng nhiệt độ theo \n bán kính giữa 7 quỹ đạo khác nhau với loess")

for (i in seq_along(orbits)) {
  dat <- subset(marsbig, orbit == orbits[i])
  fit <- loess(temperature ~ pressure, data = dat, degree = 2, span = 0.5)
  x_grid <- seq(min(dat$pressure), max(dat$pressure), length.out = 300)
  y_hat <- predict(fit, newdata = data.frame(pressure = x_grid))
  
  lines(x_grid, y_hat, col = colors[i], lwd = 2)
  points(dat$pressure, dat$temperature, col = colors[i], pch = 16, cex = 0.5)
}

legend("topleft", legend = paste("Orbit", orbits),
       col = colors, lwd = 2, pch = 16, cex = 0.8, bty = "n")

