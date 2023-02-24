# model files 
library(rpart)
library(rpart.plot)
library(caret)
library(boot)
library(MASS)
library(kknn)
library(xgboost)
library(data.table)
library(mlr)

set.seed(123)

bike_train = read.csv('Bike_train.csv')
bike_test  = read.csv('Bike_test.csv')


xgb_train = xgb.DMatrix(data = bike_train)



#Simple boosted model to assess the initial fit 
#bike.train$logcount = log(bike.train$count + 1)
#bike.train = subset(bike.train, select = -count )
#lm.fit = lm(logcount ~ . , bike.train)
yhat = round( exp(predict(cv.ct, bike_test)) - 1 )



#sampleSubmission is a dataframe with columns 'Id' and 'count'
sampleSubmission = data.frame(Id=1:length(yhat), count=yhat)
write.csv(sampleSubmission,
          file = "sampleSubmission.csv",
          row.names = FALSE,
          quote = FALSE)
