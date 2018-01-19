get = function(times, size){
  res = c()
  for(i in 1 : times){
    arr = runif(size) 
    afterMap = c()
    for(j in 1 : size){
      if(arr[j]<=1/2){
        afterMap[j] = 1
      }else if(arr[j]<=5/8){
        afterMap[j] = 2
      }else if(arr[j]<=6/8){
        afterMap[j] = 3
      }else{
        afterMap[j] = 4
      } 
    }
    m = mean(afterMap)
    v = var(afterMap)
    p = length(afterMap[afterMap<=2])/size
    
    x = c(m,v,p)
    table(x)
    
    print(m)
    print(v)
    print(p)
    #return(res)
  }
}

res = get(5,1000)


