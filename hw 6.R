##hw 6


load('eureka.RData')
library(Hmisc)
describe(eureka.df)

eureka.df$converted_in_7days =  as.factor( ifelse(eureka.df$converted_in_7days==0, 0, 1) )
table(eureka.df$converted_in_7days)
library(dplyr)
eureka.df = eureka.df %>% select(-client_id)

fixNAs <- function(data_frame){
  # Define reactions to NAs
  integer_reac <- 0
  factor_reac <- "FIXED_NA"
  character_reac <- "FIXED_NA"
  date_reac <- as.Date("1900-01-01")
  
  # Loop through columns in the data frame 
  # and depending on which class the
  # variable is, apply the defined reaction and 
  # create a surrogate
  
  for (i in 1:ncol(data_frame)) {
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))) {
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")] <-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]), i] <- integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")] <-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
        }
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }
        }
      }
  }
  
  return(data_frame)
}

eureka.df<-fixNAs(eureka.df)

eureka.df$date = as.Date.factor(eureka.df$date) #changed date to date variable
eureka.df$country = as.factor(eureka.df$country)
eureka.df$device = as.factor(eureka.df$device)
eureka.df$region = as.factor(eureka.df$region)
eureka.df$sourceMedium = as.factor(eureka.df$sourceMedium)

#For the factors, I changed the actual values to numerical factors, i.e. 0,1,2 instead of Germany, France, Paris



id.complete = complete.cases(eureka.df)
eureka.complete = eureka.df[id.complete,]
table(eureka.complete$converted_in_7days)


library(caret)
in.train = createDataPartition(eureka.df$converted_in_7days, p=0.1, list=FALSE)
train.df = eureka.df[in.train,]
table(train.df$converted_in_7days)
test.df = eureka.df[-in.train,]
table(test.df$converted_in_7days)

set.seed(123)

table(train.df$converted_in_7days)
table(test.df$converted_in_7days)


#ran last night and spent hours compiling 
library(Boruta)
inVars=sample(nrow(train.df))
vs.boruta<-Boruta(train.df$converted_in_7days~.,data=train.df[inVars,],maxRuns=500,doTrace=0)

plot(vs.boruta,xlab="",xaxt="n")
lz<-lapply(1:ncol(vs.boruta$ImpHistory),function(i)
  vs.boruta$ImpHistory[is.finite(vs.boruta$ImpHistory[,i]),i])
names(lz)<-colnames(vs.boruta$ImpHistory)
lb<-sort(sapply(lz,median))
axis(side=1,las=2,labels=names(lb),at=1:ncol(vs.boruta$ImpHistory),cex.axis=0.5,font=4)

#In the interest of time, using the top 10 selected variables 


#s_variables=names(vs.boruta$finalDecision)[vs.boruta$finalDecision %in% c("Confirmed","Tentative")]

s_variables = c( "sessionDuration","pageviews","demo_page_top","sessions_hist",
                  "sessionDuration_hist","bounces","pageviews_hist", "sessions",
                 "visited_demo_page_hist_surrogate","visited_water_purifier_page")



train.fit <- dplyr::select(train.df, all_of(s_variables) | converted_in_7days)
test.fit <- dplyr::select(test.df, all_of(s_variables) | converted_in_7days)


table(train.fit$converted_in_7days)
table(test.fit$converted_in_7days)

#logistic regression
lgfit.all <- glm(train.fit$converted_in_7days~ ., 
                 data=train.fit, 
                 family="binomial")

phat.lgfit.selected <- predict(lgfit.all, 
                               newdata = test.fit,
                               type = "response")


#random forest 
library(ranger)
library(randomForest)

rf.fit.final <- ranger(
  formula= train.fit$converted_in_7days ~ .,
  data= train.fit,
  num.trees= 500,
  mtry= 10,
  min.node.size= 100
)

yhat.rf <- predict(rf.fit.final, data = test.fit, type = "response")$predictions

#SVM

library(e1071)

svmfit = svm(train.fit$converted_in_7days ~ ., data=train.fit, kernel="radial", cost=1, scale=FALSE)
summary(svmfit)
ypred = predict(svmfit, data = test.fit,  type = "response")





library(ROCR)

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

# ROC
getROC(test.fit$converted_in_7days, phat.lgfit.selected, "Logistic", col=4)
getROC(test.fit$converted_in_7days, as.numeric(yhat.rf), "Random Forest", col=4)
getROC(test.fit$converted_in_7days, ypred, "SVM", col=4)


#Lift 
getLift <- function(y, phat, title="", add=F, col=4){
  pred = prediction(phat, y)
  perf = performance(pred, measure = "lift", x.measure = "rpp") 
  plot(perf, add=add, col=col, main=paste("Lift Curve",title), lwd=2)
  abline(h=1, lty=2)
}


# Lift curves 
getLift(test.fit$converted_in_7days, phat.lgfit.selected, "Logistic", col=4)
getLift(test.fit$converted_in_7days, as.numeric(yhat.rf), "Random Forest", col=4)
getLift(test.fit$converted_in_7days, ypred, "SVM", col=4)


#Q4 Ideally, please try out down-sampling, up-sampling, and SMOTE (three approaches).


#Q4

#upsample

data.upsample <- upSample(
  x = train.fit[,1:10], # select all columns save for the last one,
  y = train.fit[,11],
  yname = "converted_in_7days"
)

test.upsample<- upSample(
  x = test.fit[,1:10], # select all columns save for the last one,
  y = test.fit[,11],
  yname = "converted_in_7days"
)

#logit
lgfit.all <- glm(data.upsample$converted_in_7days~ ., 
                 data=data.upsample, 
                 family="binomial")

phat.lgfit.selected <- predict(lgfit.all, 
                               newdata = test.upsample,
                               type = "response")


#rf
rf.fit.final <- ranger(
  formula= data.upsample$converted_in_7days~ .,
  data= data.upsample,
  num.trees= 500,
  mtry= 10,
  min.node.size= 100
)

yhat.rf <- predict(rf.fit.final, data = test.upsample, type = "response")$predictions


#svm
svmfit = svm(data.upsample$converted_in_7days ~ ., data=data.upsample, kernel="radial", cost=1, scale=FALSE)
summary(svmfit)

ypred = predict(svmfit, data = test.upsample,  type = "response")
summary(ypred)



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

# ROC
getROC(test.upsample$converted_in_7days, phat.lgfit.selected, "Logistic", col=4)
getROC(test.upsample$converted_in_7days, as.numeric(yhat.rf), "Random Forest", col=4)
getROC(test.upsample$converted_in_7days, ypred, "SVM", col=4)


#Lift 
getLift <- function(y, phat, title="", add=F, col=4){
  pred = prediction(phat, y)
  perf = performance(pred, measure = "lift", x.measure = "rpp") 
  plot(perf, add=add, col=col, main=paste("Lift Curve",title), lwd=2)
  abline(h=1, lty=2)
}


# Lift curves 
getLift(test.upsample$converted_in_7days, phat.lgfit.selected, "Logistic", col=4)
getLift(test.upsample$converted_in_7days, as.numeric(yhat.rf), "Random Forest", col=4)
getLift(test.upsample$converted_in_7days, ypred, "SVM", col=4)




#downsample

data.downsample <- downSample(
  x = train.fit[,1:10], # select all columns save for the last one,
  y = train.fit[,11],
  yname = "converted_in_7days"
)

test.downsample<- downSample(
  x = test.fit[,1:10], # select all columns save for the last one,
  y = test.fit[,11],
  yname = "converted_in_7days"
)


#logit
lgfit.all <- glm(data.downsample$converted_in_7days~ ., 
                 data=data.downsample, 
                 family="binomial")

phat.lgfit.selected <- predict(lgfit.all, 
                               newdata = test.downsample,
                               type = "response")


#rf
rf.fit.final <- ranger(
  formula= data.downsample$converted_in_7days~ .,
  data= data.downsample,
  num.trees= 500,
  mtry= 10,
  min.node.size= 100
)

yhat.rf <- predict(rf.fit.final, data = test.downsample, type = "response")$predictions


#svm
svmfit = svm(data.downsample$converted_in_7days ~ ., data=data.downsample, kernel="radial", cost=1, scale=FALSE)
summary(svmfit)


ypred = predict(svmfit, data = test.downsample,  type = "response")
summary(ypred)



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

# ROC
getROC(test.downsample$converted_in_7days, phat.lgfit.selected, "Logistic", col=4)
getROC(test.downsample$converted_in_7days, as.numeric(yhat.rf), "Random Forest", col=4)
getROC(test.downsample$converted_in_7days, ypred, "SVM", col=4)


#Lift 
getLift <- function(y, phat, title="", add=F, col=4){
  pred = prediction(phat, y)
  perf = performance(pred, measure = "lift", x.measure = "rpp") 
  plot(perf, add=add, col=col, main=paste("Lift Curve",title), lwd=2)
  abline(h=1, lty=2)
}


# Lift curves 
getLift(test.downsample$converted_in_7days, phat.lgfit.selected, "Logistic", col=4)
getLift(test.downsample$converted_in_7days, as.numeric(yhat.rf), "Random Forest", col=4)
getLift(test.downsample$converted_in_7days, ypred, "SVM", col=4)





