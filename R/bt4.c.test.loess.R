## Đọc dữ liệu
marsbig <- read.table("data/marsbig.dat", header = TRUE)

orbits <- sort(unique(marsbig$orbit))

## Vẽ từng đồ thị riêng
for (orb in orbits) {
  
  dat <- subset(marsbig, orbit == orb)
  
  x <- dat$pressure
  y <- dat$temperature
  
  ## Fit LOESS
  fit <- loess(temperature ~ pressure,
               data = dat,
               degree = 2,
               span = 0.5)

  x_grid <- seq(min(x), max(x), length.out = 300)
  y_hat <- predict(fit,
                   newdata = data.frame(pressure = x_grid))
  
  ## Vẽ đồ thị
  plot(x, y,
       pch = 16, cex=0.6, col = "black",
       xlab = "Pressure",
       ylab = "Temperature",
       main = paste("Orbit", orb))
  
  lines(x_grid, y_hat,
        col = "blue", lwd = 2)
}
