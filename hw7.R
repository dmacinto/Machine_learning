#data preparation

setwd("~/Downloads/Winter Quarter 2022/Machine Learning/Week 8")
load('HumanActivityRecognition.RData')
library(e1071)
library(h2o)
library(ranger)
library(dplyr)
library(caret)
library(ggplot2)
library(Hmisc)
library(fastDummies)
library(DMwR2)
library(ROCR)


#Neural Network
h2o.init(nthreads=-1, max_mem_size="8G")
h2o.no_progress()


Ytrain <- Ytrain%>% rename(y1 = data.y_train)
Ytest <- Ytest %>% rename(y1 = data.y_test)


train.df = data.frame(x=Xtrain, y=Ytrain)
test.df = data.frame(x=Xtest, y=Ytest)


dfh2o = as.h2o(data.frame(x=Xtrain,y=as.factor(Ytrain$y1)), destination_frame = "train")
dftest = as.h2o(data.frame(x=Xtest,y=as.factor(Ytest$y1)), destination_frame = "test")





#data prep

library('recipes')
# Making dummy variables
train.dummies <- train.df %>% recipe(y.y1 ~ .) %>%
  step_dummy(y.y1,
             one_hot = TRUE)  %>% 
  prep() %>% 
  bake(train.df)

test.dummies <- test.df %>% recipe(y.y1 ~ .) %>%
  step_dummy(y.y1,
             one_hot = TRUE)  %>% 
  prep() %>% 
  bake(test.df)



#SVM 
  
test.fit %>%
  group_by(y.y1) %>%
  tally()


library(caret)
library(randomForest)
library(Boruta)
inVars=sample(nrow(train.dummies))

vs.boruta1<-Boruta(train.dummies$y.y1_Laying~.,data=train.dummies[inVars,],maxRuns=100,doTrace=0)
vs.boruta2<-Boruta(train.dummies$y.y1_Sitting~.,data=train.dummies[inVars,],maxRuns=100,doTrace=0)
vs.boruta3<-Boruta(train.dummies$y.y1_Standing~.,data=train.dummies[inVars,],maxRuns=100,doTrace=0)
vs.boruta4<-Boruta(train.dummies$y.y1_Walking~.,data=train.dummies[inVars,],maxRuns=100,doTrace=0)
vs.boruta5<-Boruta(train.dummies$y.y1_WalkingDownstairs~.,data=train.dummies[inVars,],maxRuns=100,doTrace=0)
vs.boruta6<-Boruta(train.dummies$y.y1_WalkingUpstairs~.,data=train.dummies[inVars,],maxRuns=100,doTrace=0)

plot(vs.boruta1,xlab="",xaxt="n")
lz<-lapply(1:ncol(vs.boruta1$ImpHistory),function(i)
  vs.boruta1$ImpHistory[is.finite(vs.boruta1$ImpHistory[,i]),i])
names(lz)<-colnames(vs.boruta1$ImpHistory)
lb<-sort(sapply(lz,median))
axis(side=1,las=2,labels=names(lb),at=1:ncol(vs.boruta1$ImpHistory),cex.axis=0.5,font=4)

plot(vs.boruta2,xlab="",xaxt="n")
lz<-lapply(1:ncol(vs.boruta2$ImpHistory),function(i)
  vs.boruta2$ImpHistory[is.finite(vs.boruta2$ImpHistory[,i]),i])
names(lz)<-colnames(vs.boruta2$ImpHistory)
lb<-sort(sapply(lz,median))
axis(side=1,las=2,labels=names(lb),at=1:ncol(vs.boruta2$ImpHistory),cex.axis=0.5,font=4)

plot(vs.boruta3,xlab="",xaxt="n")
lz<-lapply(1:ncol(vs.boruta3$ImpHistory),function(i)
  vs.boruta3$ImpHistory[is.finite(vs.boruta3$ImpHistory[,i]),i])
names(lz)<-colnames(vs.boruta3$ImpHistory)
lb<-sort(sapply(lz,median))
axis(side=1,las=2,labels=names(lb),at=1:ncol(vs.boruta3$ImpHistory),cex.axis=0.5,font=4)

plot(vs.boruta4,xlab="",xaxt="n")
lz<-lapply(1:ncol(vs.boruta4$ImpHistory),function(i)
  vs.boruta4$ImpHistory[is.finite(vs.boruta4$ImpHistory[,i]),i])
names(lz)<-colnames(vs.boruta4$ImpHistory)
lb<-sort(sapply(lz,median))
axis(side=1,las=2,labels=names(lb),at=1:ncol(vs.boruta4$ImpHistory),cex.axis=0.5,font=4)

plot(vs.boruta5,xlab="",xaxt="n")
lz<-lapply(1:ncol(vs.boruta5$ImpHistory),function(i)
  vs.boruta5$ImpHistory[is.finite(vs.boruta5$ImpHistory[,i]),i])
names(lz)<-colnames(vs.boruta5$ImpHistory)
lb<-sort(sapply(lz,median))
axis(side=1,las=2,labels=names(lb),at=1:ncol(vs.boruta5$ImpHistory),cex.axis=0.5,font=4)

plot(vs.boruta6,xlab="",xaxt="n")
lz<-lapply(1:ncol(vs.boruta6$ImpHistory),function(i)
  vs.boruta6$ImpHistory[is.finite(vs.boruta6$ImpHistory[,i]),i])
names(lz)<-colnames(vs.boruta6$ImpHistory)
lb<-sort(sapply(lz,median))
axis(side=1,las=2,labels=names(lb),at=1:ncol(vs.boruta6$ImpHistory),cex.axis=0.5,font=4)


s_variables1=names(vs.boruta1$finalDecision)[vs.boruta1$finalDecision %in% c("Confirmed","Tentative")]
s_variables2=names(vs.boruta2$finalDecision)[vs.boruta2$finalDecision %in% c("Confirmed","Tentative")]
s_variables3=names(vs.boruta3$finalDecision)[vs.boruta3$finalDecision %in% c("Confirmed","Tentative")]
s_variables4=names(vs.boruta4$finalDecision)[vs.boruta4$finalDecision %in% c("Confirmed","Tentative")]
s_variables5=names(vs.boruta5$finalDecision)[vs.boruta5$finalDecision %in% c("Confirmed","Tentative")]
s_variables6=names(vs.boruta6$finalDecision)[vs.boruta6$finalDecision %in% c("Confirmed","Tentative")]

s_variables.final=names(vs.boruta1$finalDecision)[vs.boruta1$finalDecision %in% c("Confirmed")]
s_variables2.final=names(vs.boruta2$finalDecision)[vs.boruta2$finalDecision %in% c("Confirmed")]
s_variables3.final=names(vs.boruta3$finalDecision)[vs.boruta3$finalDecision %in% c("Confirmed")]
s_variables4.final=names(vs.boruta4$finalDecision)[vs.boruta4$finalDecision %in% c("Confirmed")]
s_variables5.final=names(vs.boruta5$finalDecision)[vs.boruta5$finalDecision %in% c("Confirmed")]
s_variables6.final=names(vs.boruta6$finalDecision)[vs.boruta6$finalDecision %in% c("Confirmed")]

vars <- unique(c(s_variables.final,s_variables2.final,s_variables3.final,s_variables4.final,s_variables5.final,s_variables6.final))
vars <- vars[! vars %in% c('(Intercept)','X')]

train.fit <- dplyr::select(train.df, all_of(vars) | y.y1)
test.fit <- dplyr::select(test.df, all_of(vars) | y.y1)



svmfit <- svm(as.factor(train.fit$y.y1)~., data=train.fit, kernal = "polynomial", cost=1, scale=FALSE)

ypred <- predict(svmfit, test.fit, type = 'prob')



tune.out=tune(svmfit, train.fit$y.y1~., data=train.fit, 
              kernel="polynomial",
              ranges = list(cost=c(0.01, 0.1, 1, 10, 100))
)


summary(tune.out)

bestmod=tune.out$best.model
ypred = predict(bestmod,test.fit, type = 'prob')



#RF

library(rpart)
library(rpart.plot)


hyper_grid_rf <- expand.grid(
  mtry = seq(9, 30, by = 3),
  node_size = c(25, 50, 100, 150, 200),
  OOB_RMSE = 0
)

for(i in 1:nrow(hyper_grid_rf)) {
  # reproducibility
  set.seed(1)
  # train model
  model <- ranger(
    formula = as.factor(train.fit$y.y1) ~ .,
    data = train.fit,
    num.trees = 500,
    mtry = hyper_grid_rf$mtry[i],
    min.node.size = hyper_grid_rf$node_size[i],
    seed = 345
  )
  # add OOB error to grid
  hyper_grid_rf$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

oo_rf = hyper_grid_rf[order(hyper_grid_rf$OOB_RMSE),]

rf.fit.final <- ranger(
  formula = as.factor(train.fit$y.y1) ~ .,
  data = train.fit,
  num.trees = 500,
  mtry = oo_rf[1,]$mtry,
  min.node.size = oo_rf[1,]$node_size
)

yhat.rf = predict(rf.fit.final, data = test.fit)$predictions















#### extra code
train1.fit <- dplyr::select(train.dummies, all_of(s_variables.final) | y.y1_Laying)
test1.fit <- dplyr::select(test.dummies, all_of(s_variables.final) | y.y1_Laying)

train2.fit <- dplyr::select(train.dummies, all_of(s_variables2.final) | y.y1_Sitting)
test2.fit <- dplyr::select(test.dummies, all_of(s_variables2.final) | y.y1_Sitting)

train3.fit <- dplyr::select(train.dummies, all_of(s_variables3.final) | y.y1_Standing)
test3.fit <- dplyr::select(test.dummies, all_of(s_variables3.final) | y.y1_Standing)

train4.fit <- dplyr::select(train.dummies, all_of(s_variables4.final) | y.y1_Walking)
test4.fit <- dplyr::select(test.dummies, all_of(s_variables4.final) | y.y1_Walking)

train5.fit <- dplyr::select(train.dummies, all_of(s_variables5.final) | y.y1_WalkingDownstairs)
test5.fit <- dplyr::select(test.dummies, all_of(s_variables5.final) | y.y1_WalkingDownstairs)

train6.fit <- dplyr::select(train.dummies, all_of(s_variables6.final) | y.y1_WalkingUpstairs)
test6.fit <- dplyr::select(test.dummies, all_of(s_variables6.final) | y.y1_WalkingUpstairs)

#SVM
svmlaying = svm(train1.fit$y.y1_Laying~., data=train1.fit, kernel="linear", cost=1, scale=FALSE)
svmsitting = svm(train2.fit$y.y1_Sitting~., data=train2.fit, kernel="linear", cost=1, scale=FALSE)
svmstanding = svm(train3.fit$y.y1_Standing~., data=train3.fit, kernel="linear", cost=1, scale=FALSE)
svmwalking = svm(train4.fit$y.y1_Walking~., data=train4.fit, kernel="linear", cost=1, scale=FALSE)
svmwalkingdown= svm(train5.fit$y.y1_WalkingDownstairs~., data=train5.fit, kernel="linear", cost=1, scale=FALSE)
svmwalkingup = svm(train6.fit$y.y1_WalkingUpstairs~., data=train6.fit, kernel="linear", cost=1, scale=FALSE)









svm.con <- confusionMatrix(as.factor(svmfit), test.fit)






ypred1 = predict(svmlaying,test1.fit, type = 'prob')
ypred2 = predict(svmsitting,test2.fit, type = 'prob')
ypred3 = predict(svmstanding,test3.fit, type = 'prob')
ypred4 = predict(svmwalking,test4.fit, type = 'prob')
ypred5 = predict(svmwalkingdown,test5.fit, type = 'prob')
ypred6 = predict(svmwalkingup,test6.fit, type = 'prob')

table(predict=ypred1, truth=test1.fit$y.y1_Laying)

sqrt(mean((test1.fit$y.y1_Laying - ypred1))^2)
sqrt(mean((test2.fit$y.y1_Sitting - ypred2))^2)
sqrt(mean((test3.fit$y.y1_Standing - ypred3))^2)
sqrt(mean((test4.fit$y.y1_Walking - ypred4))^2)
sqrt(mean((test5.fit$y.y1_WalkingDownstairs - ypred5))^2)
sqrt(mean((test6.fit$y.y1_WalkingUpstairs - ypred6))^2)

getROC <- function(y, phat, title="", add=F, col=4){
  pred <- prediction(phat, y)
  # ROC curve
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(perf, add=add, col=col, main=paste("ROC", title), lwd=2)
  abline(0,1,lty=2)
  # AUC
  perf.auc <- performance(pred, measure = "auc")
  auc <- perf.auc@y.values[[1]]
  
  return(auc)
}

getROC(as.numeric(test1.fit$y.y1_Laying), ypred1, "SVM Laying", col=4)
getROC(as.numeric(test2.fit$y.y1_Sitting), ypred2, "SVM Sitting", col=4)
getROC(as.numeric(test3.fit$y.y1_Standing), ypred3, "SVM Standing", col=4)
getROC(as.numeric(test4.fit$y.y1_Walking), ypred4, "SVM Walking", col=4)
getROC(as.numeric(test5.fit$y.y1_WalkingDownstairs), ypred5, "SVM Walking Downstairs", col=4)
getROC(as.numeric(test6.fit$y.y1_WalkingUpstairs), ypred6, "SVM Walking Upstairs", col=4)











for(i in 1:nrow(hyper_grid_rf)) {
  # reproducibility
  set.seed(1)
  # train model
  model1 <- ranger(
    formula = train1.fit$y.y1_Laying ~ .,
    data = train1.fit,
    num.trees = 500,
    mtry = hyper_grid_rf$mtry[i],
    min.node.size = hyper_grid_rf$node_size[i],
    seed = 345
  )
  # add OOB error to grid
  hyper_grid_rf$OOB_RMSE[i] <- sqrt(model1$prediction.error)
}

oo_rf = hyper_grid_rf[order(hyper_grid_rf$OOB_RMSE),]

rf.fit.final1 <- ranger(
  formula = train1.fit$y.y1_Laying ~ .,
  data = train1.fit,
  num.trees = 500,
  mtry = oo_rf[1,]$mtry,
  min.node.size = oo_rf[1,]$node_size
)























for(i in 1:nrow(hyper_grid_rf)) {
  # reproducibility
  set.seed(1)
  # train model
  model2 <- ranger(
    formula = train2.fit$y.y1_Sitting ~ .,
    data = train2.fit,
    num.trees = 500,
    mtry = hyper_grid_rf$mtry[i],
    min.node.size = hyper_grid_rf$node_size[i],
    seed = 345
  )
  # add OOB error to grid
  hyper_grid_rf$OOB_RMSE[i] <- sqrt(model2$prediction.error)
}

oo_rf = hyper_grid_rf[order(hyper_grid_rf$OOB_RMSE),]

rf.fit.final2 <- ranger(
  formula = train2.fit$y.y1_Sitting ~ .,
  data = train2.fit,
  num.trees = 500,
  mtry = oo_rf[1,]$mtry,
  min.node.size = oo_rf[1,]$node_size
)





for(i in 1:nrow(hyper_grid_rf)) {
  # reproducibility
  set.seed(1)
  # train model
  model3 <- ranger(
    formula = train3.fit$y.y1_Standing ~ .,
    data = train3.fit,
    num.trees = 500,
    mtry = hyper_grid_rf$mtry[i],
    min.node.size = hyper_grid_rf$node_size[i],
    seed = 345
  )
  # add OOB error to grid
  hyper_grid_rf$OOB_RMSE[i] <- sqrt(model3$prediction.error)
}

oo_rf = hyper_grid_rf[order(hyper_grid_rf$OOB_RMSE),]

rf.fit.final3 <- ranger(
  formula = train3.fit$y.y1_Standing ~ .,
  data = train3.fit,
  num.trees = 500,
  mtry = oo_rf[1,]$mtry,
  min.node.size = oo_rf[1,]$node_size
)







for(i in 1:nrow(hyper_grid_rf)) {
  # reproducibility
  set.seed(1)
  # train model
  model4 <- ranger(
    formula = train4.fit$y.y1_Walking ~ .,
    data = train4.fit,
    num.trees = 500,
    mtry = hyper_grid_rf$mtry[i],
    min.node.size = hyper_grid_rf$node_size[i],
    seed = 345
  )
  # add OOB error to grid
  hyper_grid_rf$OOB_RMSE[i] <- sqrt(model4$prediction.error)
}

oo_rf = hyper_grid_rf[order(hyper_grid_rf$OOB_RMSE),]

rf.fit.final4 <- ranger(
  formula = train4.fit$y.y1_Walking ~ .,
  data = train4.fit,
  num.trees = 500,
  mtry = oo_rf[1,]$mtry,
  min.node.size = oo_rf[1,]$node_size
)




for(i in 1:nrow(hyper_grid_rf)) {
  # reproducibility
  set.seed(1)
  # train model
  model5 <- ranger(
    formula = train5.fit$y.y1_WalkingDownstairs ~ .,
    data = train5.fit,
    num.trees = 500,
    mtry = hyper_grid_rf$mtry[i],
    min.node.size = hyper_grid_rf$node_size[i],
    seed = 345
  )
  # add OOB error to grid
  hyper_grid_rf$OOB_RMSE[i] <- sqrt(model5$prediction.error)
}

oo_rf = hyper_grid_rf[order(hyper_grid_rf$OOB_RMSE),]

rf.fit.final5 <- ranger(
  formula = train5.fit$y.y1_WalkingDownstairs ~ .,
  data = train5.fit,
  num.trees = 500,
  mtry = oo_rf[1,]$mtry,
  min.node.size = oo_rf[1,]$node_size
)



for(i in 1:nrow(hyper_grid_rf)) {
  # reproducibility
  set.seed(1)
  # train model
  model6 <- ranger(
    formula = train6.fit$y.y1_WalkingUpstairs ~ .,
    data = train6.fit,
    num.trees = 500,
    mtry = 10,
    min.node.size = hyper_grid_rf$node_size[i],
    seed = 345
  )
  # add OOB error to grid
  hyper_grid_rf$OOB_RMSE[i] <- sqrt(model6$prediction.error)
}

oo_rf = hyper_grid_rf[order(hyper_grid_rf$OOB_RMSE),]

rf.fit.final6 <- ranger(
  formula = train6.fit$y.y1_WalkingUpstairs ~ .,
  data = train6.fit,
  num.trees = 500,
  mtry = 10,
  min.node.size = oo_rf[1,]$node_size
)


yhat1.rf = predict(rf.fit.final1, data = test1.fit)$predictions
yhat2.rf = predict(rf.fit.final2, data = test2.fit)$predictions
yhat3.rf = predict(rf.fit.final3, data = test3.fit)$predictions
yhat4.rf = predict(rf.fit.final4, data = test4.fit)$predictions
yhat5.rf = predict(rf.fit.final5, data = test5.fit)$predictions
yhat6.rf = predict(rf.fit.final6, data = test6.fit)$predictions






getROC(as.numeric(test1.fit$y.y1_Laying), as.numeric(yhat1.rf), "RF Laying", col=4)
getROC(as.numeric(test2.fit$y.y1_Sitting), as.numeric(yhat2.rf), "RF Sitting", col=4)
getROC(as.numeric(test3.fit$y.y1_Standing), as.numeric(yhat3.rf), "RF Standing", col=4)
getROC(as.numeric(test4.fit$y.y1_Walking), as.numeric(yhat4.rf), "RF Walking", col=4)
getROC(as.numeric(test5.fit$y.y1_WalkingDownstairs), as.numeric(yhat5.rf), "RF Walking Downstairs", col=4)
getROC(as.numeric(test6.fit$y.y1_WalkingUpstairs), as.numeric(yhat6.rf), "RF Walking Upstairs", col=4)





mean((validating$log_count - yhat.rf) ^ 2)



getROC(as.numeric(test1.fit$y.y1_Laying), as.numeric(yhat.rf), "RF Laying", col=4)



#tree model

library(ranger)

get.phat.rf <- function(df.train, df.test){
  
  p <- ncol(df.train)-1
  
  hyper_grid_rf <- expand.grid(
    mtry       = c(p,ceiling(sqrt(p))),
    node_size  = c(5, 10, 20))
  
  phat.rf.matrix <- matrix(0.0, nrow(df.test), nrow(hyper_grid_rf))
  
  for(j in 1:nrow(hyper_grid_rf)){
    # Train model
    rf.model <- ranger(
      formula = Retained.in.2012. ~ .,
      data = df.train,
      num.trees = 250,
      mtry = hyper_grid_rf$mtry[j],
      min.node.size = hyper_grid_rf$node_size[j],
      probability = T,
      seed = 233
    )
    phat <- predict(rf.model, data = df.test)$predictions[,2]
    phat.rf.matrix[,j] <- phat
  }
  
  y <- as.numeric(df.test$Retained.in.2012.) - 1
  mse <- rep(0, nrow(hyper_grid_rf))
  for(j in 1:nrow(hyper_grid_rf)){
    mse[j] <- sum((y-phat.rf.matrix[j])^2)
  }
  j.best <- which.min(mse)
  phat.best <- phat.rf.matrix[,j.best]
  return(phat.best)
}

phat.rf <- get.phat.rf(df.train, df.test)


#sqrt(mean((test$price - predict(best.tree, test))ˆ2))








