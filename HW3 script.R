library(dplyr)
library(rpart)
library(ranger)
library(iml)
library(ggplot2)
library(mlr)
library(xgboost)
library(tictoc)
library(iml)
library(pdp)
library(Matrix)
library(randomForest)
library(vip) 
library(kknn)
library(tidymodels)
library(magrittr)
library(gbm)
library(rpart.plot)
library(caret)
library(boot)
library(MASS)




set.seed(123)


bike_train = read.csv('Bike_train.csv')
bike_test  = read.csv('Bike_test.csv')

#600 atemp 
#


plot(bike_train$daylabel,bike_train$humidity)

#1.a
par(mfrow = c(1, 4))
plot(bike_train$windspeed,bike_train$count)
plot(bike_train$humidity,bike_train$count)
plot(bike_train$temp,bike_train$count)
plot(bike_train$atemp,bike_train$count)
dev.off()

#1.b  
boxplot(bike_train$count ~ bike_train$season)


#1.c 
###############################################################################
#NOT DONE

par(mfrow = c(1, 3))
plot(bike_train$hour, bike_train$count)
points(bike_train$temp, bike_train$count, col='blue')
plot(bike_train$workingday, bike_train$count)
points(bike_train$temp, bike_train$count, col='blue')
plot(bike_train$workingday, bike_train$hour)
dev.off()



#1.d Does the relationship between count and hour change by season?
#NOT DONE

par(mfrow = c(1, 3))
plot(bike_train$hour, bike_train$count)
points(bike_train$temp, bike_train$count, col='blue')
plot(bike_train$workingday, bike_train$count)
points(bike_train$temp, bike_train$count, col='blue')
plot(bike_train$workingday, bike_train$hour)

dev.off()




#1.e Does the distribution of hourly number of rentals change between 2011 and 2012? What does this
#tell you about the rental business?
#NOT DONE

par(mfrow = c(2, 2))
dev.off()



par(mfrow = c(1, 3))
plot(bike_train$hour, bike_train$count)
points(bike_train$temp, bike_train$count, col='blue')
plot(bike_train$workingday, bike_train$count)
points(bike_train$temp, bike_train$count, col='blue')
plot(bike_train$workingday, bike_train$hour)

dev.off()



#2.a  Create a partial dependence plot for predicted count vs each one of the following variables:
#windspeed, humidity, temp, and atemp. Do this for both the random forest and the boosting
#model.


#random forest 
ca.rf <- randomForest(bike_train$count ~ ., 
                      data = bike_train, mtry=9, ntree=500, nodesize=25, 
                      keep.inbag=TRUE,importance = TRUE)

mod <- Predictor$new(ca.rf, data = bike_train)
eff <- FeatureEffect$new(mod,
                         feature = "windspeed", 
                         method = "pdp",
                         grid.size = 30
)

p1 <- plot(eff)
eff$set.feature("humidity")
p2 <- plot(eff)
eff$set.feature("temp")
p3 <- plot(eff)
eff$set.feature("atemp")
p4 <- plot(eff)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)


#boosting
boost.count = gbm(bike_train$count ~ ., data = bike_train, distribution = "gaussian",
                  n.trees = 2000, interaction.depth = 2)

mod <- Predictor$new(boost.count, data = bike_train)
eff <- FeatureEffect$new(mod,
                         feature = "windspeed", 
                         method = "pdp",
                         grid.size = 30
)

p1 <- plot(eff)
eff$set.feature("humidity")
p2 <- plot(eff)
eff$set.feature("temp")
p3 <- plot(eff)
eff$set.feature("atemp")
p4 <- plot(eff)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)



#2.2 Use k-NN 

big.tree.windspeed = rpart(bike_train$count~bike_train$windspeed, data=bike_train,
                 control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.humidity = rpart(bike_train$count~bike_train$humidity, data=bike_train,
                           control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.temp = rpart(bike_train$count~bike_train$temp, data=bike_train,
                           control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.atemp = rpart(bike_train$count~bike_train$atemp, data=bike_train,
                           control=rpart.control(minsplit=5,cp=0.0001,xval=10))



cptablewind = printcp(big.tree.windspeed)
bestcpwind = cptablewind[ which.min(cptablewind[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptablehum = printcp(big.tree.humidity)
bestcphum = cptablehum[ which.min(cptablehum[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptabletemp = printcp(big.tree.temp)
bestcptemp = cptabletemp[ which.min(cptabletemp[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptableatemp = printcp(big.tree.atemp)
bestcpatemp = cptableatemp[ which.min(cptableatemp[,"xerror"]), "CP" ]   # this is the optimal cp parameter



oowind = order(bike_train$windspeed)
cpvecwind = c(bestcpwind / 2, bestcpwind,.0157)

oohum = order(bike_train$humidity)
cpvechum = c(bestcphum / 2, bestcphum,.0157)

ootemp = order(bike_train$temp)
cpvectemp = c(bestcptemp / 2, bestcptemp,.0157)

ooatemp = order(bike_train$atemp)
cpvecatemp = c(bestcpatemp / 2, bestcpatemp,.0157)


for(i in 1:3) {
  ptreewind = prune(big.tree.windspeed,cp=cpvecwind[i])
  pfitwind = predict(ptreewind)
}

for(i in 1:3) {
  ptreehum = prune(big.tree.humidity,cp=cpvechum[i])
  pfithum = predict(ptreehum)
}

for(i in 1:3) {
  ptreetemp = prune(big.tree.temp,cp=cpvectemp[i])
  pfittemp = predict(ptreetemp)
}

for(i in 1:3) {
  ptreeatemp = prune(big.tree.atemp,cp=cpvecatemp[i])
  pfitatemp = predict(ptreeatemp)
}


best.tree.windspeed = prune(big.tree.windspeed,cp=bestcpwind)
best.tree.humidity = prune(big.tree.humidity,cp=bestcphum)
best.tree.temp = prune(big.tree.temp,cp=bestcptemp)
best.tree.atemp = prune(big.tree.atemp,cp=bestcpatemp)



par(mfrow = c(2, 2))
plot(bike_train$windspeed, bike_train$count, xlab = "windspeed", ylab = "count", pch = 1, cex = 0.1)
lines(sort(bike_train$windspeed),predict(best.tree.windspeed)[oowind],col="red",lwd=2,cex.lab=2)

plot(bike_train$humidity, bike_train$count, xlab = "humidity", ylab = "count", pch = 1, cex = 0.1)
lines(sort(bike_train$humidity),predict(best.tree.humidity)[oohum],col="red",lwd=2,cex.lab=2)

plot(bike_train$temp, bike_train$count, xlab = "temp", ylab = "count", pch = 1, cex = 0.1)
lines(sort(bike_train$temp),predict(best.tree.temp)[ootemp],col="red",lwd=2,cex.lab=2)

plot(bike_train$atemp, bike_train$count, xlab = "atemp", ylab = "count", pch = 1, cex = 0.1)
lines(sort(bike_train$atemp),predict(best.tree.atemp)[ooatemp],col="red",lwd=2,cex.lab=2)

dev.off()










big.tree.daylabel = rpart(bike_train$count~bike_train$daylabel, data=bike_train,
                           control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.year = rpart(bike_train$count~bike_train$year, data=bike_train,
                          control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.month = rpart(bike_train$count~bike_train$month, data=bike_train,
                      control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.day = rpart(bike_train$count~bike_train$day, data=bike_train,
                       control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.hour = rpart(bike_train$count~bike_train$hour, data=bike_train,
                      control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.season = rpart(bike_train$count~bike_train$season, data=bike_train,
                       control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.holiday = rpart(bike_train$count~bike_train$holiday, data=bike_train,
                      control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.workingday = rpart(bike_train$count~bike_train$workingday, data=bike_train,
                       control=rpart.control(minsplit=5,cp=0.0001,xval=10))

big.tree.weather = rpart(bike_train$count~bike_train$weather, data=bike_train,
                      control=rpart.control(minsplit=5,cp=0.0001,xval=10))






cptabledaylabel = printcp(big.tree.daylabel)
bestcpdaylabel = cptabledaylabel[ which.min(cptabledaylabel[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptableyear = printcp(big.tree.year)
bestcpyear = cptableyear[ which.min(cptableyear[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptablemonth = printcp(big.tree.month)
bestcpmonth = cptablemonth[ which.min(cptablemonth[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptableday = printcp(big.tree.day)
bestcpaday = cptableday[ which.min(cptableday[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptablehour = printcp(big.tree.hour)
bestcphour = cptablehour[ which.min(cptablehour[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptableseason = printcp(big.tree.season)
bestcpseason = cptableseason[ which.min(cptableseason[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptableholiday = printcp(big.tree.holiday)
bestcpholiday = cptableholiday[ which.min(cptableholiday[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptableaworkingday = printcp(big.tree.workingday)
bestcpaworkingday = cptableaworkingday[ which.min(cptableaworkingday[,"xerror"]), "CP" ]   # this is the optimal cp parameter

cptableaweather = printcp(big.tree.weather)
bestcpaweather = cptableaweather[ which.min(cptableaweather[,"xerror"]), "CP" ]   # this is the optimal cp parameter











oodaylabel = order(bike_train$daylabel)
cpvecdaylabel = c(bestcpdaylabel / 2, bestcpdaylabel,.0157)

ooyear = order(bike_train$year)
cpvecyear = c(bestcpyear / 2, bestcpyear,.0157)

oomonth = order(bike_train$month)
cpvecmonth = c(bestcpmonth / 2, bestcpmonth,.0157)

ooaday = order(bike_train$day)
cpvecaday = c(bestcpaday / 2, bestcpaday,.0157)

oohour = order(bike_train$hour)
cpvechour = c(bestcphour / 2, bestcphour,.0157)

ooseason = order(bike_train$season)
cpvecseason = c(bestcpseason / 2, bestcpseason,.0157)

ooholiday = order(bike_train$holiday)
cpvecholiday = c(bestcpholiday / 2, bestcpholiday,.0157)

ooworkingday = order(bike_train$workingday)
cpvecworkingday = c(bestcpaworkingday / 2, bestcpaworkingday,.0157)

ooweather = order(bike_train$weather)
cpvecweather = c(bestcpaweather / 2, bestcpaweather,.0157)









for(i in 1:3) {
  ptreedaylabel = prune(big.tree.daylabel,cp=cpvecdaylabel[i])
  pfitdaylabel = predict(ptreedaylabel)
}

for(i in 1:3) {
  ptreeyear = prune(big.tree.year,cp=cpvecyear[i])
  pfityear = predict(ptreeyear)
}

for(i in 1:3) {
  ptreemonth = prune(big.tree.month,cp=cpvecmonth[i])
  pfitmonth = predict(ptreemonth)
}

for(i in 1:3) {
  ptreeday = prune(big.tree.day,cp=cpvecaday[i])
  pfitday = predict(ptreeday)
}

for(i in 1:3) {
  ptreehour = prune(big.tree.hour,cp=cpvechour[i])
  pfithour = predict(ptreehour)
}

for(i in 1:3) {
  ptreeseason = prune(big.tree.season,cp=cpvecseason[i])
  pfitseason = predict(ptreeseason)
}

for(i in 1:3) {
  ptreeholiday = prune(big.tree.holiday,cp=cpvecholiday[i])
  pfitholiday = predict(ptreeholiday)
}

for(i in 1:3) {
  ptreeworkingday = prune(big.tree.workingday,cp=cpvecworkingday[i])
  pfitworkingday = predict(ptreeworkingday)
}

for(i in 1:3) {
  ptreeweather = prune(big.tree.weather,cp=cpvecweather[i])
  pfitweather = predict(ptreeweather)
}








best.tree.daylabel = prune(big.tree.daylabel,cp=bestcpdaylabel)
best.tree.year = prune(big.tree.year,cp=bestcpyear)
best.tree.month = prune(big.tree.month,cp=bestcpmonth)
best.tree.day = prune(big.tree.day,cp=bestcpaday)
best.tree.hour = prune(big.tree.hour,cp=bestcphour)
best.tree.season = prune(big.tree.season,cp=bestcpseason)
best.tree.holiday = prune(big.tree.holiday,cp=bestcpholiday)
best.tree.workingday = prune(big.tree.workingday,cp=bestcpaworkingday)
best.tree.weather = prune(big.tree.weather,cp=bestcpaweather)



# error

sdaylabel = sqrt(mean((bike_train$count - predict(best.tree.daylabel, bike_test))^2))
syear = sqrt(mean((bike_train$count - predict(best.tree.year, bike_test))^2))
smonth = sqrt(mean((bike_train$count - predict(best.tree.month, bike_test))^2))
sday = sqrt(mean((bike_train$count - predict(best.tree.day, bike_test))^2))
shour = sqrt(mean((bike_train$count - predict(best.tree.hour, bike_test))^2))
sseason = sqrt(mean((bike_train$count - predict(best.tree.season, bike_test))^2))
sholiday = sqrt(mean((bike_train$count - predict(best.tree.holiday, bike_test))^2))
sworkingday = sqrt(mean((bike_train$count - predict(best.tree.workingday, bike_test))^2))
sweather = sqrt(mean((bike_train$count - predict(best.tree.weather, bike_test))^2))
stemp = sqrt(mean((bike_train$count - predict(best.tree.temp, bike_test))^2))
satemp = sqrt(mean((bike_train$count - predict(best.tree.atemp, bike_test))^2))
shumidity = sqrt(mean((bike_train$count - predict(best.tree.humidity, bike_test))^2))
swindspeed = sqrt(mean((bike_train$count - predict(best.tree.windspeed, bike_test))^2))































kv = 1:20 * 50   #these are the k values (k as in kNN) we will try
# since the possible k might be very large, we pick k every 50, from 50 to 1000

#does cross-validation for training data (x,y).






kfbest = kknn(bike_train$count~bike_train$humidity,bike_train,bike_test,k=20,kernel = "rectangular")
plot(bike_train$humidity, bike_train$count, xlab = "humidity", ylab = "count", pch = 1, cex = 0.1)
lines(sort(bike_train$count),kfbest$fitted,col="red",lwd=2, cex.lab=2)









cv = docvknn(matrix(train$mileage,ncol=1),train$price,kv,nfold=10)
plot(kv, cv)
kbest = kv[which.min(cv)]
kfbest = kknn(price~mileage,train,data.frame(mileage=sort(train$mileage)),k=kbest,kernel = "rectangular")
plot(train$mileage, train$price, xlab = "mileage", ylab = "price", pch = 1, cex = 0.1)
lines(sort(train$mileage),kfbest$fitted,col="red",lwd=2, cex.lab=2)
# error
kfbest = kknn(price~mileage,train,test,k=kbest,kernel = "rectangular")
sqrt(mean((test$price - kfbest$fitted.values)^2))




















ca.tree_wind <- rpart(bike_train$count ~ bike_train$windspeed, data=bike_train)

ca.tree_hum <- rpart(bike_train$count ~ bike_train$humidity, data=bike_train)

ca.tree_temp <- rpart(bike_train$count ~ bike_train$temp, data=bike_train)

ca.tree_atemp <- rpart(bike_train$count ~ bike_train$atemp, data=bike_train)

rpart.plot(ca.tree_wind)


#set up knn test variables
k_array = c(1:200)
kNN_MSE_wind = NULL
kNN_MSE_hum = NULL
kNN_MSE_temp = NULL
kNN_MSE_atemp = NULL

kNN_test_windspeed = bike_test
ind = order(bike_test[,13])
kNN_test_windspeed =kNN_test_windspeed[ind,]

kNN_test_humidity = bike_test
ind = order(bike_test[,12])
kNN_test_humidity =kNN_test_humidity[ind,]

kNN_test_temp = bike_test
ind = order(bike_test[,11])
kNN_test_temp =kNN_test_temp[ind,]

kNN_test_temp = bike_test
ind = order(bike_test[,10])
kNN_test_temp =kNN_test_temp[ind,]






k_vec=2:15
#near_list <- vector("list", length(k_vec))
fit=matrix(0, nrow=dim(train)[1], ncol=length(k_vec))
for (i in 1:length(k_vec)){
  k=k_vec[i]
  near <- kknn(y~x, train=train, test=train[,.(x)],k=k, kernel='rectangular')
  #near_list[[i]]=near
  fit[,i]=near$fitted
}




#MSE for kNN


for (i in k_array){
  k_near_wind = kknn(bike_train$count ~ bike_train$windspeed, bike_train, kNN_test_windspeed, k = i, kernel = "rectangular")
  aux = mean((bike_train$count - k_near_wind$fitted)^2)
  kNN_MSE_wind = c(kNN_MSE_wind, aux)
}

min_kNN_wind <- which.min(kNN_MSE_wind)
min_kNN_wind #199
k_near_wind = kknn(bike_train$count ~ bike_train$windspeed, bike_train, kNN_test_windspeed, k = min_kNN_wind, kernel = "rectangular")


for (i in k_array){
  k_near_hum = kknn(bike_train$count ~ bike_train$humidity, bike_train, kNN_test_humidity, k = i, kernel = "rectangular")
  aux = mean((bike_train$count - k_near_hum$fitted)^2)
  kNN_MSE_hum = c(kNN_MSE_hum, aux)
}

min_kNN_hum <- which.min(kNN_MSE_hum)
min_kNN_hum #196
k_near_hum = kknn(bike_train$count ~ bike_train$humidity, bike_train, kNN_test_humidity, k = min_kNN_hum, kernel = "rectangular")


for (i in k_array){
  k_near_temp = kknn(bike_train$count ~ bike_train$temp, bike_train, kNN_test_temp, k = i, kernel = "rectangular")
  aux = mean((bike_train$count - k_near_temp$fitted)^2)
  kNN_MSE_temp = c(kNN_MSE_temp, aux)
}

min_kNN_temp <- which.min(kNN_MSE_temp)
min_kNN_temp #197
k_near_temp = kknn(bike_train$count ~ bike_train$temp, bike_train, kNN_test_temp, k = min_kNN_temp, kernel = "rectangular")


for (i in k_array){
  k_near_atemp = kknn(bike_train$count ~ bike_train$atemp, bike_train, kNN_test_atemp, k = i, kernel = "rectangular")
  aux = mean((bike_train$count - k_near_atemp$fitted)^2)
  kNN_MSE_atemp = c(kNN_MSE_atemp, aux)
}

min_kNN_atemp <- which.min(kNN_MSE_atemp)
min_kNN_atemp #195
k_near_atemp = kknn(bike_train$count ~ bike_train$atemp, bike_train, kNN_test_atemp, k = min_kNN_atemp, kernel = "rectangular")


par(mfrow = c(2, 2))
plot(bike_train$windspeed,bike_train$count)
lines(bike_train$count, k_near_wind$fitted, lwd = 2, col = "blue")
plot(bike_train$humidity,bike_train$count)
lines(bike_train$count, k_near_hum$fitted, lwd = 2, col = "blue")
plot(bike_train$temp,bike_train$count)
lines(bike_train$count, k_near_temp$fitted, lwd = 2, col = "blue")
plot(bike_train$atemp,bike_train$count)
lines(bike_train$count, k_near_atemp$fitted, lwd = 2, col = "blue")

dev.off()


#2.3 variable importance plots 
varImpPlot(ca.rf)
summary(boost.count)


#3.1















#3.2

#3.3

#4 

plot(bike_train$hour, bike_train$count)

cv.ct <- rpart(bike_train$count ~ bike_train$hour, data = bike_train, method = "anova",
               cp = 0.00001, minsplit = 5, xval = 5)
#printcp(cv.ct)
#plotcp(cv.ct)
# this is the cp parameter with smallest cv-error
index_cp_min = which.min(cv.ct$cptable[,"xerror"])
# one standard deviation rule 
# need to find first cp value for which the xerror is below horizontal line on the plot 
val_h = cv.ct$cptable[index_cp_min, "xerror"] + cv.ct$cptable[index_cp_min, "xstd"]
index_cp_std = Position(function(x) x < val_h, cv.ct$cptable[, "xerror"])
cp_std = cv.ct$cptable[ index_cp_std, "CP" ]

pruned.ct <- prune(cv.ct, cp = cp_std)
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"]) #11
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)

plot(cv.ct$cptable[,4])
which.min(cv.ct$cptable[index_cp_min, "xerror"])

plot(bike_train$hour, bike_train$count, main = paste("k = ", 11), col = "black")
abline(lm(bike_train$count ~ bike_train$hour), col='red')
lines(bike_train$hour, pruned.ct, col='green')
#(bike_train$hour,pruned.ct, col='green')




##k-NN

k_array = c(1:200)
kNN_MSE_hour = NULL

#kNN_test_windspeed = bike_test
#ind = order(bike_test[,13])
#kNN_test_windspeed =kNN_test_windspeed[ind,]

for (i in k_array){
  k_near_hour = kknn(bike_train$count ~ bike_train$hour, bike_train, bike_test, k = i, kernel = "rectangular")
  aux = mean((bike_train$count - k_near_hour$fitted)^2)
  kNN_MSE_hour = c(kNN_MSE_hour, aux)
}

min_kNN_hour <- which.min(kNN_MSE_hour)
min_kNN_hour #199
k_near_hour = kknn(bike_train$count ~ bike_train$hour, bike_train, bike_test, k = min_kNN_hour, kernel = "rectangular")

plot(bike_train$hour,bike_train$count)
lines(bike_train$count, k_near_hour$fitted, lwd = 2, col = "blue")





#random forest approach


ca.rf <- randomForest(bike_train$count ~ ., 
                      data = bike_train, mtry=7, ntree=500, nodesize=25, 
                      keep.inbag=TRUE,importance = TRUE)







plot(bike_train$hour, bike_train$count)
abline(ca.rf$predicted)


cv.ct <- rpart(bike_train$count ~ bike_train$hour, data = bike_train, method = "anova",
               cp = 0.00001, minsplit = 5, xval = 5)


# this is the cp parameter with smallest cv-error
index_cp_min = which.min(cv.ct$cptable[,"xerror"])

# one standard deviation rule 
# need to find first cp value for which the xerror is below horizontal line on the plot 
val_h = cv.ct$cptable[index_cp_min, "xerror"] + cv.ct$cptable[index_cp_min, "xstd"]
index_cp_std = Position(function(x) x < val_h, cv.ct$cptable[, "xerror"])
cp_std = cv.ct$cptable[ index_cp_std, "CP" ]


pruned.ct <- prune(cv.ct, cp = cp_std)
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)






rf.fit <- ranger(
  formula = bike_train$count~ .,
  data            = bike_train, 
  num.trees       = 500,
  importance      = 'impurity',
  probability = T
)





#Simple boosted model to assess the initial fit 
#bike.train$logcount = log(bike.train$count + 1)
#bike.train = subset(bike.train, select = -count )
#lm.fit = lm(logcount ~ . , bike.train)
yhat = round( exp(predict(rf.fit, bike_test)) - 1 )



#sampleSubmission is a dataframe with columns 'Id' and 'count'
sampleSubmission = data.frame(Id=1:length(yhat), count=yhat)
write.csv(sampleSubmission,
          file = "sampleSubmission.csv",
          row.names = FALSE,
          quote = FALSE)



















































#####################################################################extra code 









#train_boost <- model.matrix(bike_train$count ~., data = bike_train)

#model.XGB <-xgboost(data = data.matrix(train_boost), 
#                         label = bike_train$windspeed,
#                         eta = 0.1,
#                         max_depth = 4,
#                         nround = 200)




# parameter list
params <- list(
  eta = 0.1,
  max_depth = 2
)


xgb.fit.final <- xgboost(
  params = params,
  data = train_boost,
  label = bike_train$count,
  objective = "reg:squarederror",
  nround = 100,
  verbose = 0
)

importance_matrix <- xgb.importance(model = xgb.fit.final)
# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")







#xgb.plot.shap(data = train_boost,
#              model = model.XGB)




## Boosting

#XGBoost only works with matrices that contain all numeric variables; 
#consequently, we need to one hot encode our data. 
#There are different ways to do this in R.

#- `Matrix::sparse.model.matrix`
#- `caret::dummyVars`













n = nrow(bike_train)

bike_train_boost <- mutate_if(bike_train, is.character, as.factor)

train.index = sample(nrow(bike_train), 5000)
uc.train = bike_train[train.index,]


test.index = sample(nrow(bike_test), 5000)
uc.test = bike_test[-test.index,]




X = sparse.model.matrix(bike_train$count ~ ., data = bike_train)
X.train = X[train.index, ]
Y.train = bike_train$count[train.index]
X.test = X[-test.index, ]
Y.test = bike_test$count[-test.index]

dim(X.train)



#First, create a grid.

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),     ## controls the learning rate
  interaction.depth = c(1, 3, 5), ## tree depth
  n.minobsinnode = c(10, 30, 50), ##  minimum number of observations required in each terminal node
  bag.fraction = c(.5, .65, .8),  ##  percent of training data to sample for each tree
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)


#Now we can perform the grid search.

#```{r cache=TRUE}
for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid$shrinkage[i],
    max_depth = hyper_grid$interaction.depth[i],
    min_child_weight = hyper_grid$n.minobsinnode[i],
    subsample = hyper_grid$bag.fraction[i]
  )
  
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = X.train,
    label = Y.train,
    nrounds = 3000,
    nfold = 5,
    objective = "reg:squarederror",     # for regression models
    verbose = 0,                        # silent,
    early_stopping_rounds = 10          # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}


(oo = hyper_grid %>%
    dplyr::arrange(min_RMSE) %>%
    head(10))


# parameter list
params <- list(
  eta = oo[1,]$shrinkage,
  max_depth = oo[1,]$interaction.depth,
  min_child_weight = oo[1,]$n.minobsinnode,
  subsample = oo[1,]$bag.fraction
)

# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = X.train,
  label = Y.train,
  nrounds = oo[1,]$optimal_trees,
  objective = "reg:squarederror",
  verbose = 0
)

yhat.xgb <- predict(xgb.fit.final, newdata=X.test)
sqrt( var(Y.test - yhat.xgb) )






# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit.final)











#ind = order(bike_test[,10])
#bike_test = bike_test[ind,]

#ind = order(bike_test[,11])
#bike_test = bike_test[ind,]

#ind = order(bike_test[,12])
#bike_test = bike_test[ind,]

#ind = order(bike_test[,13])
#bike_test = bike_test[ind,]


#ind = order(bike_train[,10])
#bike_train = bike_train[ind,]

#ind = order(bike_train[,11])
#bike_train = bike_train[ind,]

#ind = order(bike_train[,12])
#bike_train = bike_train[ind,]

#ind = order(bike_train[,13])
#bike_train = bike_train[ind,]



#ind = order(bike_train[,])
#bike_train = bike_train[ind,]
















