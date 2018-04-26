# Read the crime data
crime <- read.csv("crime.csv")
attach(crime)
str(crime)
fit <- lm(formula = murder.rate ~ poverty + high.school + college +
            single.parent + unemployed + metropolitan + region)
summary(fit)
plot(fitted(fit), resid(fit), main = 'Residual Plot of fit')
abline(h=0)
qqnorm(resid(fit), main = 'fit')
qqline(resid(fit), main = 'fit')

# Take log10(murder.rate) as response
y <- log10(murder.rate)
fit1 <- update(fit, y ~ .)
summary(fit1)
plot(fitted(fit1), resid(fit1), main="fit1")
abline(h=0)
par(mfrow=c(1,2))
qqnorm(resid(fit1), main = 'fit1')
qqline(resid(fit1), main = 'fit1')
qqnorm(resid(fit), main = 'fit')
qqline(resid(fit), main = 'fit')

hist(resid(fit), main='fit')
hist(resid(fit2), main='fit1')

# Drop region
fit2 <- lm(formula = y ~ poverty +  high.school + 
             college + single.parent + unemployed + metropolitan)
summary(fit2)
anova(fit2, fit1)

# Add region back, remove poverty
fit3 <- lm(formula = y ~ high.school + college + 
             single.parent + unemployed + metropolitan + region)
summary(fit3)
anova(fit3, fit1)

# Remove high.school
fit4 <- lm(formula = y ~  college + single.parent + unemployed + metropolitan + region)
anova(fit4, fit3)
summary(fit4)

# Remove college
fit5 <- lm(formula = y ~  single.parent + unemployed + metropolitan + region)
anova(fit5, fit4)
summary(fit5)

# Remove unemployed
fit6 <- lm(formula = y ~  single.parent + metropolitan + region)
summary(fit6)
anova(fit6, fit5)

# Forward selection based on AIC
fit7.forward <- step(lm(y ~ 1, data = crime), 
                      scope = list(upper = ~poverty + high.school + college + 
                                     single.parent + unemployed + metropolitan + region),
                      direction = "forward")
# Backward elimination based on AIC
fit8.backward <- step(lm(y ~ poverty + high.school + college + single.parent +
                           unemployed + metropolitan + region, data = crime), 
                       scope = list(lower = ~1), direction = "backward")
# Both
fit9.both <- step(lm(y ~ 1, data = crime), 
                   scope = list(lower = ~1, upper = ~poverty +
                                  high.school + college + single.parent + 
                                  unemployed + metropolitan + region),
                   direction = "both")

anova(fit6, fit9.both)

par(mfrow=c(1,1))
# Residual plot
plot(fitted(fit6), resid(fit6))
abline(h = 0)

# normal QQ plot
qqnorm(resid(fit6))
qqline(resid(fit6))

# show the count
table(unlist(crime$region))
# South is the most

# predict 
x.new = data.frame(single.parent=mean(crime$single.parent),
                   metropolitan=mean(crime$metropolitan),region='South')
predict(fit6, newdata = x.new)
