#####################################
#Libraries
#####################################
library(ggplot2)
library(kknn)

#####################################
#Creating data
#####################################
#Real relationship
x_real = c(-1:1)
real <- data.frame(x_real = x_real)
real <- transform(real, y_real = sin(2 * x_real) + 2)

#Generating x values
x_train = runif(100, -1, 1)
x_test = runif(10000, -1, 1)

#Generating epislon values
eps_train = rnorm(100, mean = 0, sd = 0.1)
eps_test = rnorm(10000, mean = 0, sd = 0.1)

#Creating training set
train <- data.frame(
  x = x_train,
  error = eps_train
)

train <- transform(train, y_train = sin(2 * x) + 2 + error)

#Creating test set
test <- data.frame(
  x = x_test,
  error = eps_test
)

test <- transform(test, y_test = sin(2 * x) + 2 + error)

#####################################
#Linear regression
#####################################

#OLS
ols <- lm(y_train ~ x_train, data = train)
f_ols <- function(x) coefficients(ols)[2] * x + coefficients(ols)[1]

#Plot scatter plot
f <- function(x) sin(2 * x) + 2
ggplot(train, aes(x = x, y = y_train)) + geom_point() + geom_function(fun = f) + geom_function(fun = f_ols, col = "blue", linetype = "dashed")

#####################################
#KNN
#####################################
layout(matrix(1:2, ncol=2, byrow=TRUE))
par(oma=c(0, 0, 0, 0), mar=c(1, 1, 1, 1))

knn_test = test
ind = order(knn_test[,1])
knn_test = knn_test[ind,]

#k = 2
k_near2 = kknn(y_train ~ x, train, knn_test, k = 2, kernel = "rectangular")
plot(train$x, train$y_train, main = paste("k = ", 2), col = "black")
lines(knn_test[,1], k_near2$fitted, lwd = 2, col = "blue")
lines(real$x_real, real$y_real, col = "black")


#k = 12
k_near12 = kknn(y_train ~ x, train, knn_test, k = 12, kernel = "rectangular")
plot(train$x, train$y_train, main = paste("k = ", 12), col = "black")
lines(knn_test[,1], k_near12$fitted, lwd = 2, col = "blue")
lines(real$x_real, real$y_real, col = "black")

#####################################
#Test set MSE
#####################################
k_array = c(2:15)
mse_test = test

kNN_MSE = NULL
log_k = NULL
OLS_MSE = NULL

#MSE for linear
test <- transform(test, y_predict = f_ols(x))
mse_ols = mean((test$y_test - test$y_predict)^2)

#MSE for kNN
for (i in k_array){
  k_near = kknn(y_train ~ x, train, mse_test, k = i, kernel = "rectangular")
  aux = mean((test[,3] - k_near$fitted)^2)
  kNN_MSE = c(kNN_MSE, aux)
  
  log_k = c(log_k, log(1/i))
  
  OLS_MSE = c(OLS_MSE, mse_ols)
}

plot(log_k, kNN_MSE, ylim = c(0, 0.04))
lines(log_k, OLS_MSE, col = "black", lty = "dashed")

