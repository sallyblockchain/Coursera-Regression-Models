## Quiz 4.
# Problem 1.
library(MASS)
dim(shuttle)
head(shuttle)
shuttle$newUse <- as.numeric(shuttle$use == "auto")
fit <- glm(newUse ~ as.factor(wind) - 1, data=shuttle, family="binomial")
odds <- exp(summary(fit)$coef)
odds[1] / odds[2] # 0.9686888

# Problem 2.
fit <- glm(newUse ~ as.factor(wind) + factor(magn) - 1, 
           family="binomial", data=shuttle)
summary(fit)$coef
exp(coef(fit))
odds <- exp(cbind(OddsRatio = coef(fit), confint(fit)))
odds[1] / odds[2] # 0.9684981

# Problem 3.
## If you fit a logistic regression model to a binary variable, 
# for example use of the autolander, then fit a logistic regression 
# model for one minus the outcome (not using the autolander) what 
# happens to the coefficients?
fit3 <- glm(I(1 - newUse) ~ as.factor(wind) - 1, 
            data=shuttle, family="binomial")
summary(fit3)$coef # -0.2513144 -0.2831263
summary(fit)$coef # 0.2513144 0.2831263
# The coefficients reverse their signs. 

# Problem 4.
fit <- glm(count ~ spray - 1, data=InsectSprays, family="poisson")
summary(fit)$coef
rate <- exp(coef(fit))
rate[1] / rate[2] # 0.9456522

# Problem 5.
fit <- glm(count ~ as.factor(spray) + offset(log(count+1)), 
         family="poisson", data=InsectSprays)
fit2 <- glm(count ~ as.factor(spray) + offset(log(10)+log(count+1)), 
            family="poisson", data=InsectSprays)
summary(fit)$coef
summary(fit2)$coef  
# as.factor(spray)B  0.003512473
# The coefficient estimate is unchanged

# Problem 6.

