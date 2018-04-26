df <- data.frame(read.csv("bodytemp-heartrate.csv", header = T))
male.bt = df[df$gender==1,]$body_temperature
female.bt = df[df$gender==2,]$body_temperature
# Boxplot
par(mfrow=c(1,1))
boxplot(male.bt, female.bt, names = c( 'Male','Female'))
par(mfrow=c(1,2))
# Histogram
hist(male.bt, main = 'Body Temperature of Male')
hist(female.bt, main = 'Body Temperature of Female')
# QQ-plot of male's body-tempearture
qqnorm(male.bt, main = 'Body Temperature of Male')
qqline(male.bt)
# QQ-plot of female's body-temperaure
qqnorm(female.bt, main = 'Body Temperature of Female')
qqline(female.bt)


# Build CI
t.test(male.bt, female.bt, alternative = "two.sided", conf.level = 0.95, 
       var.equal = FALSE)


male.hr = df[df$gender==1,]$heart_rate
female.hr = df[df$gender==2,]$heart_rate
# Boxplot
par(mfrow=c(1,1))
boxplot(male.hr, female.hr, names = c( 'Male','Female'))
par(mfrow=c(1,2))
#Histogram
hist(male.hr, main = 'Heart Rate of Male')
hist(female.hr, main = 'Heart Rate of Female')
# QQ-plot of male's heart rate
qqnorm(male.hr, main = 'Heart Rate of Male');
qqline(male.hr)
# QQ-plot of female's heart rate
qqnorm(female.hr, main = 'Heart Rate of Female');
qqline(female.hr)

# Build CI
t.test(male.hr, female.hr, alternative = "two.sided", conf.level = 0.95, 
       var.equal = FALSE)

par(mfrow=c(1,1))
# Scatter plot of body tempearture against heart rate for all people
plot(df$body_temperature,df$heart_rate)
cor(df$heart_rate, df$body_temperature)

p1.reg = lm(heart_rate~body_temperature, data=df)
summary(p1.reg)
anova(p1.reg)

plot(df$body_temperature, df$heart_rate)
abline(p1.reg)

par(mfrow=c(1,2))
#male
male.reg = lm(heart_rate~body_temperature, data=df[df$gender==1,])
plot(df[df$gender==1,]$body_temperature, df[df$gender==1,]$heart_rate, 
     main = 'Male - heart rate vs body temperature')
abline(male.reg)
# female
female.reg = lm(heart_rate~body_temperature, data=df[df$gender==2,])
plot(df[df$gender==2,]$body_temperature, df[df$gender==2,]$heart_rate, 
     main = 'Female - heart rate vs body temperature')
abline(female.reg)
