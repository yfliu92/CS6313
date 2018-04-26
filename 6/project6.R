# Read the crime data
crime <- read.csv("crime.csv")
attach(crime)

fit <- lm(formula = murder.rate ~ poverty + high.school + college + single.parent + unemployed + metropolitan + region)
summary(fit)
plot(fitted(fit), resid(fit), main = 'fit')
abline(h=0)
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
# From the comparasion of the original regression fit and the linearly transformed
# regression fit1, we can see that after transformation, the disttributin of residue is 
# is more close to normal distribution
# From the linearly transformed regresion fit1, we can see that region is categorical predictor,
# so we remove region first and see if it is significant or not
# Drop region
fit2 <- lm(formula = y ~ poverty +  high.school + college + single.parent + unemployed + metropolitan)
anova(fit2, fit1)
# We can see that region is a significant perdicator, so it cannot be removed
# We can also see that poverty is the least significant predicator, so we should try
# to remove poverty and see
# Add region back, remove poverty
fit3 <- lm(formula = y ~ high.school + college + single.parent + unemployed + metropolitan + region)
anova(fit3, fit1)
summary(fit3)
# From the result, we can see that poverty is not significant, so we can remove poverty;
# we can also see that high.school is the least significant after removing poverty
# Remove high.school
fit4 <- lm(formula = y ~  college + single.parent + unemployed + metropolitan + region)
anova(fit4, fit3)
summary(fit4)
# From the result, we can see that high.school is not significant, so we can remove high.school;
# we can also see that college is the least significant after removing high.school
# Remove college
fit5 <- lm(formula = y ~  single.parent + unemployed + metropolitan + region)
anova(fit5, fit4)
summary(fit5)
# From the result, we can see that college is not significant, so we can remove college;
# we can also see that unemployed is the least significant after removing college
# Remove unemployed
fit6 <- lm(formula = y ~  single.parent + metropolitan + region)
summary(fit6)
# Now, all the items left are significant
# Perform a partial F-test to check significance of unemployed
anova(fit6, fit5)
# From the result, we can see that we do not need unemployed. Also, the model in fit6
# does not have any non-significant predictors. Therefore, we take this as our
# preliminary model for the data.
# Forward selection based on AIC
fit10.forward <- step(lm(y ~ 1, data = crime), 
                      scope = list(upper = ~poverty + high.school + college + single.parent + unemployed + metropolitan + region),
                      direction = "forward")
# Backward elimination based on AIC
fit11.backward <- step(lm(y ~ poverty + high.school + college + single.parent + unemployed + metropolitan + region, data = crime), 
                       scope = list(lower = ~1), direction = "backward")
# Both forward/backward
fit12.both <- step(lm(y ~ 1, data = crime), 
                   scope = list(lower = ~1, upper = ~poverty + high.school + college + single.parent + unemployed + metropolitan + region),
                   direction = "both")
# We see that direction = backward and direction = both pick the following model:
# single.parent + metropolitan + region + unemployed
# Whereas, we came up with the following model (fit6):
# single.parent + metropolitan + region
anova(fit6, fit1)
anova(fit6, fit12.both)
# From the result, we can see that fit6 is better
# Residual plot
plot(fitted(fit6), resid(fit6))
abline(h = 0)
# plot of absolute residuals
plot(fitted(fit6), abs(resid(fit6)))
# normal QQ plot
qqnorm(resid(fit6))
qqline(resid(fit6))
# This preliminary model passes the diagnostics. So we can take this
# as our final model.