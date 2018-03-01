P1_2 = function(N, sizes, thetas){
  result=c()
  # name of every row in the result
  name=c()
  
  for (size in sizes) {
    for (theta in thetas) {
      arr_theta_hat_1 = c()
      arr_theta_hat_2 = c()
      name = c(name, paste("n=",size, ", theta=", theta))
      for (i in 1 : N) {
        data = runif(size, min = 0, max = theta)
        theta_hat_1 = max(data)
        theta_hat_2 = 2 * mean(data)
        arr_theta_hat_1[i] = (theta_hat_1 - theta)^2
        arr_theta_hat_2[i] = (theta_hat_2 - theta)^2
      }
      mse1 = mean(arr_theta_hat_1)
      mse2 = mean(arr_theta_hat_2)
      result = rbind(result, c(mse1, mse2))
    }
  }
  colnames(result) = c("MSE_theta_hat_1","MSE_theta_hat_2")
  rownames(result) = name
  return (result)
}
# call the function
sizes = c (1, 2, 3, 5, 10, 30)
thetas = c (1, 5, 50, 100)
result = P1_2(1000, sizes, thetas)

# start to plot
par(mfrow=c(3,2))
y1 = result[1:4]
y2 = result[25:28]
f_name = "n = 1"
y = c(y1, y2)
miny = min(y)
maxy = max(y)
plot(thetas, y1, type = 'l', lty = 1, ylim = c(miny, maxy), xlab = 'Theta', ylab = 'MSE', main = f_name)
lines(thetas, y2, lty = 2)
legend("topleft", c("MLE", "Method of Moment"), lty = c(1,2))

y1 = result[5:8]
y2 = result[29:32]
f_name = "n = 2"
y = c(y1, y2)
miny = min(y)
maxy = max(y)
plot(thetas, y1, type = 'l', lty = 1, ylim = c(miny, maxy), xlab = 'Theta', ylab = 'MSE', main = f_name)
lines(thetas, y2, lty = 2)
legend("topleft", c("MLE", "Method of Moment"), lty = c(1,2))

y1 = result[9:12]
y2 = result[33:36]
f_name = "n = 3"
y = c(y1, y2)
miny = min(y)
maxy = max(y)
plot(thetas, y1, type = 'l', lty = 1, ylim = c(miny, maxy), xlab = 'Theta', ylab = 'MSE', main = f_name)
lines(thetas, y2, lty = 2)
legend("topleft", c("MLE", "Method of Moment"), lty = c(1,2))

y1 = result[13:16]
y2 = result[37:40]
f_name = "n = 5"
y = c(y1, y2)
miny = min(y)
maxy = max(y)
plot(thetas, y1, type = 'l', lty = 1, ylim = c(miny, maxy), xlab = 'Theta', ylab = 'MSE', main = f_name)
lines(thetas, y2, lty = 2)
legend("topleft", c("MLE", "Method of Moment"), lty = c(1,2))

y1 = result[17:20]
y2 = result[41:44]
f_name = "n = 10"
y = c(y1, y2)
miny = min(y)
maxy = max(y)
plot(thetas, y1, type = 'l', lty = 1, ylim = c(miny, maxy), xlab = 'Theta', ylab = 'MSE', main = f_name)
lines(thetas, y2, lty = 2)
legend("topleft", c("MLE", "Method of Moment"), lty = c(1,2))

y1 = result[21:24]
y2 = result[45:48]
f_name = "n = 30"
y = c(y1, y2)
miny = min(y)
maxy = max(y)
plot(thetas, y1, type = 'l', lty = 1, ylim = c(miny, maxy), xlab = 'Theta', ylab = 'MSE', main = f_name)
lines(thetas, y2, lty = 2)
legend("topleft", c("MLE", "Method of Moment"), lty = c(1,2))

