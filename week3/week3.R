## Quiz 3.
# Problem 1.
data(mtcars)
attach(mtcars)
fit <- lm(mpg ~ as.factor(cyl) + wt, data=mtcars)
summary(fit) # as.factor(cyl)8  -6.0709 

# Problem 2.
fit2 <- lm(mpg ~ as.factor(cyl), data=mtcars)
summary(fit2)$coef[3] # -11.56364
summary(fit)$coef[3] # -6.07086
# Holding weight constant, cylinder appears to have less of an 
# impact on mpg than if weight is disregarded.

# Problem 3.
summary(fit)
fit3 <- lm(mpg ~ as.factor(cyl)*wt, data=mtcars)
# OR another way
# fit32 <- lm(mpg ~ factor(cyl) + wt + factor(cyl):wt, data=mtcars)
summary(fit3)
result <- anova(fit, fit3, test="Chi")
result$Pr # 0.1037502
# The P-value is larger than 0.05. So, according to our criterion, 
# we would fail to reject, which suggests that the interaction 
# terms may not be necessary.

# Problem 4.
fit4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data=mtcars)
summary(fit4)
# wt coef: The estimated expected change in MPG per one ton increase in 
# weight for a specific number of cylinders (4, 6, 8).

# Problem 5.
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit5 <- lm(y ~ x)
lm.influence(fit5)$hat[5] # 0.9945734
# Or another way
# hatvalues(fit5)

# Problem 6.
dfbetas(fit5)[5, 2] # -133.8226

# Problem 7.
# Q: Consider a regression relationship between Y and X with and 
# without adjustment for a third variable Z. Which of the 
# following is true about comparing the regression coefficient
# between Y and X with and without adjustment for Z?

# A: It is possible for the coefficient to reverse sign after 
# adjustment. For example, it can be strongly significant and 
# positive before adjustment and strongly significant and negative 
# after adjustment.
