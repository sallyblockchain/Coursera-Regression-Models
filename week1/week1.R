## Quiz 1.
# Problem 1.
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
weighted.mean(x, w) # 0.1471
# Or another way
# sum(x*w)/7 # 0.1471

# Problem 2.
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit<- lm( y ~ x - 1 )
summary(fit) # 0.8263

# Problem 3.
data(mtcars)
fit <- lm(mpg ~ wt, mtcars)
summary(fit) # -5.3445

# Problem 4.
corOfYandX <- 0.5
sdYoverX <- 2
beta1 <- corOfYandX*sdYoverX
beta1 # 1

# Problem 5.
corOfYandX <- 0.4
quiz1 <- 1.5
quiz2 <- quiz1*corOfYandX*1 + 0
quiz2 # 0.6

# Problem 6.
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
mean <- mean(x)
sd <- sd(x)
(x[1] - mean)/sd # -0.9718658

# Problem 7.
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit <- lm(y ~ x)
summary(fit) # 1.567

# Problem 8.
# You know that both the predictor and response have mean 0. 
# What can be said about the intercept when you fit a linear 
# regression?
# It must be identically 0.

# Problem 9.
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x) # 0.573

# Problem 10.
# cor(X, Y)*sd(Y)/sd(X) / (cor(X,Y)*sd(X)/sd(Y)) = sd(Y)^2/(sd(X)^2)
# = var(Y)/var(X)
