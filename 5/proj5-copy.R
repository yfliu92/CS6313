d = read.csv("bodytemp-heartrate.csv", header = T)
df <- data.frame()
# Calculate mean body temperature of male
m1b = mean(df[df$gender==1,]$body_temperature);
m1b;
# Calculate mean body temperature of female
m2b = mean(df[df$gender==2,]$body_temperature);
m2b;
# From the calculation result, we can see that the difference of body temperature between male and female is 98.10462 - 98.39385 = -0.28293
# Calculate mean heart rate of male
m1h = mean(df[df$gender==1,]$heart_rate);
m1h;
# Calculate mean heart rate of female
m2h = mean(df[df$gender==2,]$heart_rate);
m2h;
# From the calculation result, we can see that the difference of body temperature between male and female is 73.36923 - 74.15385 = -0.78462
# QQ-plot of male's body-tempearture
qqnorm(df[df$gender==1,]$body_temperature);
# QQ-plot of female's body-temperaure
qqnorm(df[df$gender==2,]$body_temperature);
# QQ-plot of male's heart rate
qqnorm(df[df$gender==1,]$heart_rate);
# QQ-plot of female's heart rate
qqnorm(df[df$gender==2,]$heart_rate);
# From the qqplot of the distribution, we can see that the distribution is normal, so we can use t-test to calculate the confidence interval
s1b = sqrt(var(df[df$gender==1,]$body_temperature));
n1b = length(df[df$gender==1,]$body_temperature);
s2b = sqrt(var(df[df$gender==2,]$body_temperature));
n2b = length(df[df$gender==2,]$body_temperature);
vb = (s1b ^ 2 / n1b + s2b ^ 2 / n2b) ^ 2 / (s1b ^ 4 / n1b ^ 2 / (n1b - 1) + s2b ^ 4 / n2b ^ 2 / (n2b - 1));
vbci = (m1b - m2b) + c(-1, 1) * qt(0.975, vb) * sqrt(s1b ^ 2 / n1b + s2b ^ 2 / n2b);
vbci;
s1h = sqrt(var(df[df$gender==1,]$heart_rate));
n1h = length(df[df$gender==1,]$heart_rate);
s2h = sqrt(var(df[df$gender==2,]$heart_rate));
n2h = length(df[df$gender==2,]$heart_rate);
vh = (s1h ^ 2 / n1h + s2h ^ 2 / n2h) ^ 2 / (s1h ^ 4 / n1h ^ 2 / (n1h - 1) + s2h ^ 4 / n2h ^ 2 / (n2h - 1));
vhci = (m1h - m2h) + c(-1, 1) * qt(0.975, vh) * sqrt(s1h ^ 2 / n1h + s2h ^ 2 / n2h);
vhci;
# From the body tempearature confidence interval, we can see that male body temperature is generally lower than female's body temperature because 0 is not in the confidence interval -0.53964856 -0.03881298;
# From the heart rate confidence intervalm we can see that male heart rate is similar to the female heart rate because 0 is in the confidence interval -3.243732  1.674501;
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