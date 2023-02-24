
library(kknn) ## knn library


x <- runif(100, min=-1, max=1)
eps <- rnorm(100, mean=0, sd=0.1)

y <- 1.8 * x + 2 + eps
y



x_test <- runif(10000, min=-1, max=1)
eps_test <- rnorm(10000, mean=0, sd=0.1)

y_test <- 1.8 * x_test + 2 + eps_test
y_test

plot(x, y)
abline(y/x)
abline(lm(y ~ x))
summary(lm(y ~ x))