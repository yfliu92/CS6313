########################
# N: the number of experiments
# size: sample size in each experiment
# theta: upper limit of uniform distribution
#######################
P1 = function(N, size, theta){
  result=c()
  arr_theta_hat_1 = c()
  arr_theta_hat_2 = c()
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
  
  colnames(result) = c("MSE_theta_hat_1","MSE_theta_hat_2")
  rownames(result) = paste("n=",size, ", theta=", theta)
  return (result)
}

# call the function
result = P1(1000, 30, 100)
result



