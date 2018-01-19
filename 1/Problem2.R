n = c(10, 30, 50, 100)
p = c(0.10, 0.25, 0.50, 0.75, 0.90)
count  = 1
for( i in n){
  for(j in p) {
    p_temp = c()
    p_temp_x = c()
    for(z in 1 : 500) {
      s = sample(c(0, 1), size = i, replace = TRUE, prob = c(1-j, j))
      p_temp[z] = sum(s)/i
      p_temp_x[z] = z
    }
    print(p_temp)
    qqnorm(y=p_temp, main=paste("title", count))
    count = count + 1
  }
}