time = c(4.79, 10.89, 6.54, 22.15)

neg.loglik.fn = function(theta, dat){
  result = length(dat)*log(theta)-(theta+1)*sum(log(dat))
  return(-result)
}

ml.est=optim(par=c(1), fn=neg.loglik.fn, method = "Brent", lower = 0, upper = 100, hessian = TRUE , dat=time)
ml.est
sqrt(diag(solve(ml.est$hessian)))
