n = c(10, 30, 50, 100)
p = c(0.10, 0.25, 0.50, 0.75, 0.90)
count  = 1
par(mfrow=c(2,2))
for( i in n){
  for(j in p) {
    p_temp = c()
    p_temp=replicate(500, mean(rbinom(i, 1, j)))
    
    qqnorm(y=p_temp, main=paste("title: n = ", i, ", p = ", j))
    qqline(p_temp)
    count = count + 1
  }
}