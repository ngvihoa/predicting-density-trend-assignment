source('R/guest.data.trend.R')
source('R/bt2.cross.validation.R')

# mcycle data

data(mcycle, package = 'MASS')
head(mcycle)

m_fun <- data_trend_fun(fun = 'kernel_reg')
u1 <- m_fun(x = mcycle$times, y = mcycle$accel, x_eval = 30, h=0.5)

plot(x=mcycle$times, y=mcycle$accel, pch=16)
points(x=30, y=u1, col='blue')


range(mcycle$times)
x_plot <- seq(0, 60, length.out = 201)
y_hat <- m_fun(x = mcycle$times, y = mcycle$accel, x_eval = x_plot, h=0.5)

plot(x=mcycle$times, y=mcycle$accel, pch=16)
lines(x=x_plot, y=y_hat, col='blue')


## CV -----
CV_kernel(x=mcycle$times, y=mcycle$accel, h=0.5)
CV_kernel(x=mcycle$times, y=mcycle$accel, h=1.5)
CV_kernel(x=mcycle$times, y=mcycle$accel, h=2.5)

h_plot <- seq(0.1, 4, length.out = 41)
cv1_est <- cross_validation(x=mcycle$times,y=mcycle$accel,h=h_plot, kernel = 'gaussian', cv_method="CV",m_method="loclin_reg")

plot(x=h_plot, y=cv1_est,type='b', pch=16)

system.time({
  cv1_est <-cross_validation(x=mcycle$times,
                             y=mcycle$accel,
                             h=h_plot, 
                             kernel = 'gaussian', 
                             cv_method="CV",
                             m_method="loclin_reg")
})


cv2_est <- GCV_loclin(x=mcycle$times, y=mcycle$accel, h=h_plot)

plot(x=h_plot, y=cv2_est, type='b', pch=16)

system.time({
  cv2_est <- GCV_loclin(x=mcycle$times, y=mcycle$accel, h=h_plot)
})





