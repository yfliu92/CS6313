########################
# n: array of sample size
# p: array of probability
#######################
P2 = function(n, p){
  # set graphical parameters
  par(mfrow=c(2,2))
  
  for( i in n){
    for(j in p) {
      # simulate 500 for each (n, p)
      p_temp=replicate(500, mean(rbinom(i, 1, j)))
      # draw Q-Q plot
      qqnorm(y=p_temp, main=paste("title: n = ", i, ", p = ", j))
      qqline(p_temp)
    }
  }
}

# call the function
n = c(10, 30, 50, 100)
p = c(0.10, 0.25, 0.50, 0.75, 0.90)
P2(n, p)