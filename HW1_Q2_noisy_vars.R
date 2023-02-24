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
  x1 = x_train,
    error = eps_train
)

train <- transform(train, y_train = sin(2 * x1) + 2 + error)
train = subset(train, select = -c(error))

#Creating test set
test <- data.frame(
  x1 = x_test,
  error = eps_test
)

test <- transform(test, y_test = sin(2 * x1) + 2 + error)
test = subset(test, select = -c(error))

#####################################
#Linear regression
#####################################

#OLS
ols <- lm(y_train ~ x1, data = train)
f_ols <- function(x) coefficients(ols)[2] * x + coefficients(ols)[1]

#Plot scatter plot
f <- function(x) sin(2 * x) + 2
ggplot(train, aes(x = x1, y = y_train)) + geom_point() + geom_function(fun = f) + geom_function(fun = f_ols, col = "blue", linetype = "dashed")

#####################################
#Test set MSE
#####################################
#MSE for linear
test <- transform(test, y_predict = f_ols(x1))
mse_ols = mean((test$y_test - test$y_predict)^2)
test = subset(test, select = -c(y_predict))

k_array = c(2:15)

f_mse = NULL

layout(matrix(1:20, ncol=4, byrow=TRUE))
par(oma=c(0, 0, 0, 0), mar=c(1, 1, 1, 1))

#MSE for kNN
for (j in 1:20){
  kNN_MSE = NULL
  log_k = NULL
  OLS_MSE = NULL
  
  for (i in k_array){
      k_near = kknn(y_train ~ ., train, test, k = i, kernel = "rectangular")
      aux = mean((test[,2] - k_near$fitted)^2)
      kNN_MSE = c(kNN_MSE, aux)
      
      log_k = c(log_k, log(1/i))
      
      OLS_MSE = c(OLS_MSE, mse_ols)
  }
  
  plot(log_k, kNN_MSE, ylim = c(0, 0.4), main = paste("# of x's=", j))
  lines(log_k, OLS_MSE, col = "black", lty = "dashed")
  
  new_test <- rnorm(10000)
  test[ , ncol(test) + 1] <- new_test
  colnames(test)[ncol(test)] <- paste0("x", j+1, sep = "")
  
  new_train <- rnorm(100)
  train[ , ncol(train) + 1] <- new_train
  colnames(train)[ncol(test)] <- paste0("x", j+1, sep = "")
}

