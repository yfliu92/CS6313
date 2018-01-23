######################
# times: the number of experiment
# size: the number of draws
######################
P1 = function(times, size){
  # result: store the results of every experiment
  result = c()
  
  # name of every row in the result
  name=c()
  
  for(i in 1 : times){
    # arr: generate random variable U, assign them to arr
    arr = runif(size) 
    # afterMap: store the actual value after mapping from the random number U 
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
    # set the name of experiment
    name = c(name, paste(i, ": "))
    
    # calculate Mean
    m = mean(afterMap)
    
    # calculate Var
    v = var(afterMap)
    
    # calculate the probability P(x<=2)
    p = length(afterMap[afterMap<=2])/size
    
    x = c(m,v,p)
    
    # push an experiment result to 'result'
    result = rbind(result, x)
    
  }
  # set the column and row name for result 
  colnames(result) = c("Mean","Variance","P(X<=2)")
  rownames(result) = name
  
  return(result)
}

# call the function
res = P1(5, 1000)
res

