library(boot)
# read data
Ad = read.csv('Advertising.csv', header = T)

# plot scatterplots
par(mfrow=c(1,2))
plot(Ad$TV, Ad$sales, main = 'TV-sales')
plot(Ad$radio, Ad$sales, main = 'radio-sales')

TV = Ad$TV
sales = Ad$sales
radio = Ad$radio

# length of data
n = length(TV)

TV_sales = data.frame(cbind(TV, sales))
radio_sales = data.frame(cbind(radio, sales))

TV.sales = function(d,i=c(1:n)){
  d2 = d[i,]
  return(cor(d2$TV,d2$sales))
}
radio.sales = function(d, i=c(1:n)){
  d2 = d[i,]
  return(cor(d2$sales, d2$radio))
}

TV.sales.boot = boot(data = TV_sales,statistic=TV.sales,R=999)
radio.sales.boot = boot(data = radio_sales,statistic=radio.sales,R=999)

boot.ci(boot.out=TV.sales.boot)
boot.ci(boot.out=radio.sales.boot)

cor(Ad$TV, Ad$sales, use = "everything", method = c("pearson", "kendall", "spearman"))
cor(Ad$radio, Ad$sales, use = "everything", method = c("pearson", "kendall", "spearman"))
