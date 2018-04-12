df <- data.frame(read.csv("~/Downloads/bodytemp-heartrate(1).csv"));
# Calculate mean body temperature of male
mean(df[df$gender==1,]$body_temperature);
# Calculate mean body temperature of female
mean(df[df$gender==2,]$body_temperature);
# From the calculation result, we can see that the difference of body temperature between male and female is 98.10462 - 98.39385 = -0.28293
# Calculate mean heart rate of male
mean(df[df$gender==1,]$heart_rate);
# Calculate mean heart rate of female
mean(df[df$gender==2,]$heart_rate);
# From the calculation result, we can see that the difference of heart rate between male and female is 73.36923 - 74.15385 = -0.78462
# From the result, we can see that there is some difference between male and female. Below is summary and boxplot and histogram of the distribution
# Boxplot of male body temperature distribution
boxplot(df[df$gender==1,]$body_temperature);
# Boxplot of female body temperature distribution
boxplot(df[df$gender==2,]$body_temperature);
# Boxplot of male heart rate distribution
boxplot(df[df$gender==1,]$heart_rate);
# Boxplot of female heart rate distribution
boxplot(df[df$gender==2,]$heart_rate);
# Histogram of male body temperature distribution
hist(df[df$gender==1,]$body_temperature);
# Histogram of female body temperature distribution
hist(df[df$gender==2,]$body_temperature);
# Histogram of male heart rate distribution
hist(df[df$gender==1,]$heart_rate);
# Histogram of female heart rate distribution
hist(df[df$gender==2,]$heart_rate);
# Summary of male body temperature distribution
summary(df[df$gender==1,]$body_temperature);
# Summary of female body temperature distribution
summary(df[df$gender==2,]$body_temperature);
# Summary of male heart rate distribution
summary(df[df$gender==1,]$heart_rate);
# Summary of female heart rate distribution
summary(df[df$gender==2,]$heart_rate);
# Form the distribution, we can see that for body temperature, both female and male is not skewed. For heart rate, male is left skewed, female if right skewed
# Covariance between body tempearture and heart rate for all people
plot(df[,1], df[,3]);
cor(df[,1], df[,3]);
# Covariance between body tempearture and heart rate among male
plot(df[df$gender==1,1], df[df$gender==1,3]);
cor(df[df$gender==1,1], df[df$gender==1,3]);
# Covariance between body tempearture and heart rate among female
plot(df[df$gender==2,1], df[df$gender==2,3]);
cor(df[df$gender==2,1], df[df$gender==2,3]);
# From the covariance result, we can see that the linearity between body temperature and heart rate among all people is weak (cor = 0.2536564),
# while the linearity between body temperature and heart rate among female (cor = 0.2869312) is a little better than that among male (cor = 0.1955894);