library(boot)
mean.npar = function(x, indices) {
  result = mean(x[indices])
  return(result)
}
conf.int = function (lambda, n, alpha) {
  x = rexp(n, lambda)
  ci= mean(x) + c(-1, 1) * qnorm(1-alpha/2) * sd(x)/sqrt(n)
  mean.npar.boot = boot(x, mean.npar, R=999, sim="ordinary", stype="i")
  cib = boot.ci(boot.out = mean.npar.boot)
  res = cib$percent[4:5]
  ci[3] = res[1]
  ci[4] = res[2]
  return(ci)
}
P2 = function (lambdas, ns) {
  for (lambda in lambdas) {
    for (n in ns) {
      ci.mat = replicate(5000, conf.int(lambda, n, 0.05))
      # z-interval
      prob1 = mean(((1/lambda)>=ci.mat[1,])*((1/lambda)<=ci.mat[2,]))
      # bootstrap percentile interval
      prob2 = mean(((1/lambda)>=ci.mat[3,])*((1/lambda)<=ci.mat[4,]))
      # plot CIs 
      plot(1:100, 
           ci.mat[1, 1:100],
           ylim=c(min(ci.mat[1:2, 1:100]), max(ci.mat[1:2, 1:100])),
           xlab='Sample #', ylab='95% CI', type='p',
           main=paste("Z-interval: n = ", n, ", lambda = ", lambda, ", Coverage Probability = ", prob1))
      points(1:100, ci.mat[2, 1:100])
      for(i in 1:100) {
        segments(i, ci.mat[1, i], i, ci.mat[2,i], lty = 1)
      }
      abline(h=1/lambda)
      # plot CIs
      plot(1:100, 
           ci.mat[3, 1:100],
           ylim=c(min(ci.mat[3:4, 1:100]), max(ci.mat[3:4, 1:100])),
           xlab='Sample #', ylab='95% CI', type='p',
           main=paste("Percentile-interval: n = ", n, ", lambda = ", lambda, ", Coverage Probability = ", prob2))
      points(1:100, ci.mat[4, 1:100])
      for(i in 1:100) {
        segments(i, ci.mat[3, i], i, ci.mat[4,i], lty = 1)
      }
      abline(h=1/lambda)
    }
  }
}
# call the functions
lambdas = c(0.01, 0.1, 1, 10)
ns = c(5, 10, 30, 100)
P2(lambdas, ns)
