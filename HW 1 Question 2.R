library(kknn) ## knn library

#2.1-2.3
x <- runif(100, min=-1, max=1)
eps <- rnorm(100, mean=0, sd=0.1)
y <- 1.8 * x + 2 + eps

x_test <- runif(10000, min=-1, max=1)
eps_test <- rnorm(10000, mean=0, sd=0.1)
y_test <- 1.8 * x_test + 2 + eps_test

plot(x, y)
abline(2, 1.8)
abline(lm(y ~ x), col='blue', lty=2, lwd=1)


#2.4
train = data.frame(x,y) 
test = data.frame(x_test,y_test)

kval=12
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
plot(x, y)
abline(2, 1.8)
#lines(train[,1],near$fitted.values,col=2,lwd=2)
points(train[,1],near$fitted.values, pch=19, col=3)




#2.5 MSE NOT DONE

train = data.frame(x,y) 
test = data.frame(x_test,y_test)
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test =test[ind,]

#kval=1
#for (kval < 13){
 # kval = kval+1
  #near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
  #MSE2 = mean(train[,1] - near$fitted.values)^2

# }

kval=2
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE2 = mean(train[,1] - near$fitted.values)^2

kval=3
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE3 = mean(train[,1] - near$fitted.values)^2

kval=4
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE4 = mean(train[,1] - near$fitted.values)^2

kval=5
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE5 = mean(train[,1] - near$fitted.values)^2

kval=6
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE6 = mean(train[,1] - near$fitted.values)^2

kval=7
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE7 = mean(train[,1] - near$fitted.values)^2

kval=8
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE8 = mean(train[,1] - near$fitted.values)^2

kval=9
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE9 = mean(train[,1] - near$fitted.values)^2

kval=10
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE10 = mean(train[,1] - near$fitted.values)^2

kval=11
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE11 = mean(train[,1] - near$fitted.values)^2

kval=12
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE12 = mean(train[,1] - near$fitted.values)^2

kval=13
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE13 = mean(train[,1] - near$fitted.values)^2

kval=14
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE14 = mean(train[,1] - near$fitted.values)^2

kval=15
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE15 = mean(train[,1] - near$fitted.values)^2

totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
             MSE12, MSE13, MSE14, MSE15)

totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
              log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
              log(1/12), log(1/13), log(1/14), log(1/15))

plot(totalKVAL, totalMSE)
abline(lm(totalMSE~totalKVAL), col=3)
y_hat = lm(y~x)
lmMSE = mean(train[,1] - y_hat$fitted.values)^2
abline(h=lmMSE, col='blue', lty=2, lwd=1)




#2.6

x <- runif(100, min=-1, max=1)
eps <- rnorm(100, mean=0, sd=0.1)
y <- tanh(1.1 * x) + 2 + eps

x_test <- runif(10000, min=-1, max=1)
eps_test <- rnorm(10000, mean=0, sd=0.1)
y_test <- tanh(1.8 * x_test) + 2 + eps_test

plot(x, y)
abline(2, 1.1)
abline(lm(y ~ x), col='blue', lty=2, lwd=1)


train = data.frame(x,y) 
test = data.frame(x_test,y_test)

kval=12
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
plot(x, y)
abline(2, 1.1)
#lines(train[,1],near$fitted.values,col=2,lwd=2)
points(train[,1],near$fitted.values, pch=19, col=3)

#STILL NEED TO ADD THE 2.5 PART OF THE QUESTION

train = data.frame(x,y) 
test = data.frame(x_test,y_test)
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test =test[ind,]

kval=2
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE2 = mean(train[,1] - near$fitted.values)^2

kval=3
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE3 = mean(train[,1] - near$fitted.values)^2

kval=4
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE4 = mean(train[,1] - near$fitted.values)^2

kval=5
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE5 = mean(train[,1] - near$fitted.values)^2

kval=6
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE6 = mean(train[,1] - near$fitted.values)^2

kval=7
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE7 = mean(train[,1] - near$fitted.values)^2

kval=8
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE8 = mean(train[,1] - near$fitted.values)^2

kval=9
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE9 = mean(train[,1] - near$fitted.values)^2

kval=10
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE10 = mean(train[,1] - near$fitted.values)^2

kval=11
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE11 = mean(train[,1] - near$fitted.values)^2

kval=12
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE12 = mean(train[,1] - near$fitted.values)^2

kval=13
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE13 = mean(train[,1] - near$fitted.values)^2

kval=14
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE14 = mean(train[,1] - near$fitted.values)^2

kval=15
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE15 = mean(train[,1] - near$fitted.values)^2

totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
             MSE12, MSE13, MSE14, MSE15)

totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
              log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
              log(1/12), log(1/13), log(1/14), log(1/15))

plot(totalKVAL, totalMSE)
abline(lm(totalMSE~totalKVAL), col=3)
y_hat = lm(y~x)
lmMSE = mean(train[,1] - y_hat$fitted.values)^2
abline(h=lmMSE, col='blue', lty=2, lwd=1)






#2.7
x <- runif(100, min=-1, max=1)
eps <- rnorm(100, mean=0, sd=0.1)
y <- sin(2*x) + 2 + eps

x_test <- runif(10000, min=-1, max=1)
eps_test <- rnorm(10000, mean=0, sd=0.1)
y_test <- sin(2*x_test) + 2 + eps_test

plot(x, y)
abline(2, 2)
abline(lm(y ~ x), col='blue', lty=2, lwd=1)


train = data.frame(x,y) 
test = data.frame(x_test,y_test)

kval=15
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
plot(x, y)
abline(2, 2)
#lines(train[,1],near$fitted.values,col=2,lwd=2)
points(train[,1],near$fitted.values, pch=19, col=3)

train = data.frame(x,y) 
test = data.frame(x_test,y_test)
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test =test[ind,]

kval=2
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE2 = mean(train[,1] - near$fitted.values)^2

kval=3
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE3 = mean(train[,1] - near$fitted.values)^2

kval=4
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE4 = mean(train[,1] - near$fitted.values)^2

kval=5
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE5 = mean(train[,1] - near$fitted.values)^2

kval=6
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE6 = mean(train[,1] - near$fitted.values)^2

kval=7
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE7 = mean(train[,1] - near$fitted.values)^2

kval=8
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE8 = mean(train[,1] - near$fitted.values)^2

kval=9
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE9 = mean(train[,1] - near$fitted.values)^2

kval=10
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE10 = mean(train[,1] - near$fitted.values)^2

kval=11
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE11 = mean(train[,1] - near$fitted.values)^2

kval=12
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE12 = mean(train[,1] - near$fitted.values)^2

kval=13
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE13 = mean(train[,1] - near$fitted.values)^2

kval=14
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE14 = mean(train[,1] - near$fitted.values)^2

kval=15
near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
MSE15 = mean(train[,1] - near$fitted.values)^2

totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
             MSE12, MSE13, MSE14, MSE15)

totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
              log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
              log(1/12), log(1/13), log(1/14), log(1/15))

plot(totalKVAL, totalMSE)
abline(lm(totalMSE~totalKVAL), col=3)
y_hat = lm(y~x)
lmMSE = mean(train[,1] - y_hat$fitted.values)^2
abline(h=lmMSE, col='blue', lty=2, lwd=1)


#2.8
#20
x <- runif(100, min=-1, max=1)
eps <- rnorm(100, mean=0, sd=0.1)
y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
  0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
  0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
  0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps

x_test <- runif(10000, min=-1, max=1)
eps_test <- rnorm(10000, mean=0, sd=0.1)
y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
  0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
  0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
  0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) + eps_test

plot(x, y)
abline(2, 2)
abline(lm(y ~ x), col='blue', lty=2, lwd=1)

      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      

      
      
      
      #19
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
#18

      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
#17
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      

#16
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
      
#15
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
#14
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
#13
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
#12
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
       
      
      
#11
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
      
#10
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 + 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
      
#9
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
      
#8
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 +0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
#7
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
      
      
#6
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2+ 0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
      
      
#5
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 +
        0*sin(2*x) +
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2 +
        0*sin(2*x) +0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)
      
      
      
#4
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 +
        
        0*sin(2*x) +0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2 +
        0*sin(2*x) +
        0*sin(2*x) +
        0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1) 
      
      
      
#3
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 +
        0*sin(2*x) +0*sin(2*x) +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2 +
        0*sin(2*x) +
        0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)      
      
      
      
      
#2
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 +
        0*sin(2*x)  +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2 +
        0*sin(2*x) + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)      
      
      
      
      
#1
      
      x <- runif(100, min=-1, max=1)
      eps <- rnorm(100, mean=0, sd=0.1)
      y <- sin(2*x) + 2 +eps
      
      x_test <- runif(10000, min=-1, max=1)
      eps_test <- rnorm(10000, mean=0, sd=0.1)
      y_test <- sin(2*x_test) + 2 + eps_test
      
      plot(x, y)
      abline(2, 2)
      abline(lm(y ~ x), col='blue', lty=2, lwd=1)
      
      train = data.frame(x,y) 
      test = data.frame(x_test,y_test)
      ind = order(test[,1]) #sorting test makes the plots below easier to make.
      test =test[ind,]
      
      kval=2
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE2 = mean(train[,1] - near$fitted.values)^2
      
      kval=3
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE3 = mean(train[,1] - near$fitted.values)^2
      
      kval=4
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE4 = mean(train[,1] - near$fitted.values)^2
      
      kval=5
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE5 = mean(train[,1] - near$fitted.values)^2
      
      kval=6
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE6 = mean(train[,1] - near$fitted.values)^2
      
      kval=7
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE7 = mean(train[,1] - near$fitted.values)^2
      
      kval=8
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE8 = mean(train[,1] - near$fitted.values)^2
      
      kval=9
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE9 = mean(train[,1] - near$fitted.values)^2
      
      kval=10
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE10 = mean(train[,1] - near$fitted.values)^2
      
      kval=11
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE11 = mean(train[,1] - near$fitted.values)^2
      
      kval=12
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE12 = mean(train[,1] - near$fitted.values)^2
      
      kval=13
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE13 = mean(train[,1] - near$fitted.values)^2
      
      kval=14
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE14 = mean(train[,1] - near$fitted.values)^2
      
      kval=15
      near = kknn(y~x,train,test,k=kval,kernel = "rectangular")
      MSE15 = mean(train[,1] - near$fitted.values)^2
      
      totalMSE = c(MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, 
                   MSE12, MSE13, MSE14, MSE15)
      
      totalKVAL = c(log(1/2),log(1/3), log(1/4), log(1/5), log(1/6), 
                    log(1/7), log(1/8), log(1/9), log(1/10), log(1/11),
                    log(1/12), log(1/13), log(1/14), log(1/15))
      
      plot(totalKVAL, totalMSE)
      abline(lm(totalMSE~totalKVAL), col=3)
      y_hat = lm(y~x)
      lmMSE = mean(train[,1] - y_hat$fitted.values)^2
      abline(h=lmMSE, col='blue', lty=2, lwd=1)      
      
      