library(dplyr)
library(caret)
library(glmnet)





STCdata_A <- read.csv('travelData.csv')
STCdata_B <- read.csv('travelData_supplement.csv')
STCdata_merged = merge(STCdata_A, STCdata_B, by = 'ID')
STCdata_merged <- STCdata_merged[,-1]



str(STCdata_merged)

n_distinct(STCdata_A$From.Grade, na.rm = FALSE)

STCdata_merged <- mutate_at(STCdata_merged, vars(From.Grade), as.factor)

str(STCdata_merged$From.Grade)

(unique.per.column <- sapply(dplyr::select_if(STCdata_merged, is.numeric), n_distinct ) )
(column.names.to.factor <- names(unique.per.column)[unique.per.column < 15] )

STCdata_merged <- mutate_at(STCdata_merged, column.names.to.factor, as.factor)

str(STCdata_merged)







#date.columns = c('Departure.Date', 'Return.Date', 'Deposit.Date', 'Early.RPL', 'Latest.RPL', 
#                 'Initial.System.Date', 'FirstMeeting', 'LastMeeting', 'X...3.FPP.Date', 
#                 'X...10.FPP.Date', 'X...20.FPP.Date','X...35.FPP.Date')


#STCdata_merged <- mutate_at(STCdata_merged, date.columns, function(x) as.Date(x, format = "%m/%d/%Y"))

#str(STCdata_merged)



STCdata_merged <- mutate_if(STCdata_merged, is.character, as.factor)

str(STCdata_merged)


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










STCdata_merged<-fixNAs(STCdata_merged)

any( sapply(STCdata_merged, function(x) sum(is.na(x))) > 0)


str(STCdata_merged)



table(STCdata_merged$Group.State)

#############################################################################


combinerarecategories<-function(data_frame,mincount){
  for (i in 1:ncol(data_frame)) {
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-
      paste("Other", colnames(data_frame)[i], sep=".")
    data_frame[,i]<-a 
  }
  return(data_frame) 
}



STCdata_merged<-combinerarecategories(STCdata_merged,10)

table(STCdata_merged$Group.State)




set.seed(233) 

inTrain <- createDataPartition(
  y = STCdata_merged$Retained.in.2012.,
  p = 1888/2389, 
  list = FALSE)
df.train <- STCdata_merged[ inTrain,]
df.test <- STCdata_merged[ -inTrain, ]


print('Training set proportion:')
table(df.train$Retained.in.2012.) / nrow(df.train)
print('Test set proportion:')
table(df.test$Retained.in.2012.) / nrow(df.test)



lgfit.all <- glm(Retained.in.2012.~ ., 
                 data=df.train, 
                 family="binomial")
summary(lgfit.all)



lgfit.null <- glm(Retained.in.2012.~ 1, 
                  data=df.train, family="binomial")

lgfit.selected <- step(lgfit.null,                  # the starting model for our search
                       scope=formula(lgfit.all),    # the largest possible model that we will consider.
                       direction="forward", 
                       k=log(nrow(df.train)),       # by default step() uses AIC, but by
                       # multiplying log(n) on the penalty, we get BIC.
                       # See ?step -> Arguments -> k
                       trace=1)



summary(lgfit.selected)


phat.lgfit.selected <- predict(lgfit.selected, 
                               newdata = df.test,
                               type = "response")


X <- model.matrix(formula(lgfit.all), STCdata_merged)
#need to subtract the intercept
X <- X[,-1]

X.train = X[ inTrain, ]
X.test = X[ -inTrain, ]



cv.l1.lgfit <- cv.glmnet(
  x       = X.train, 
  y       = df.train$Retained.in.2012.,
  family  = "binomial", 
  alpha   = 1,   #alpha=0 gives ridge regression
  nfolds  = 5)




plot(cv.l1.lgfit, sign.lambda=-1)

glmnet.fit <- cv.l1.lgfit$glmnet.fit



betas <- coef(cv.l1.lgfit, s = "lambda.1se")
model.1se <- which(betas[2:length(betas)]!=0)
colnames(X[,model.1se])


phat.l1.lgfit <- predict(glmnet.fit,
                         newx = X.test,
                         s = cv.l1.lgfit$lambda.1se,
                         type = "response")


###########Confusion Matrix

library(caret)



# y should be 0/1
# phat are probabilities obtained by our algorithm 
# thr is the cut off value - everything above thr is classified as 1
getConfusionMatrix = function(y,phat,thr) {
  yhat = as.factor( ifelse(phat > thr, 1, 0) )
  confusionMatrix(yhat, y)
}

#lgfit.selected
#glmnet.fit
#cv.l1.lgfit$glmnet.fit
#phat.lgfit.selected
#phat.l1.lgfit


cfm_selected <- getConfusionMatrix(df.test$Retained.in.2012., phat.lgfit.selected, 0.6072)
cfm_lgfit <- getConfusionMatrix(df.test$Retained.in.2012., phat.l1.lgfit, 0.6072)




###############ROC Curve

library(ROCR)
library(pROC)

phat.lgfit.selected

phat.l1.lgfit



phat.l1.lgfit <- predict(glmnet.fit,
                         newx = X.test,
                         s = cv.l1.lgfit$lambda.1se,
                         type = "response")

phat.lgfit.selected <- predict(lgfit.selected, 
                               newdata = df.test,
                               type = "response")





pred = prediction(phat.lgfit.selected, df.test$Retained.in.2012.)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col = 1, lwd = 2,
     main= 'ROC curve', xlab='FPR', ylab='TPR', cex.lab=1)
perf <- performance(pred, measure = "auc")
perf@y.values




pred = prediction(phat.l1.lgfit, df.test$Retained.in.2012.)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col = 1, lwd = 2,
     main= 'ROC curve', xlab='FPR', ylab='TPR', cex.lab=1)
perf <- performance(pred, measure = "auc")
perf@y.values








cfm_fit <- getConfusionMatrix(df.test$Retained.in.2012., phat.lgfit.selected, 0.6072)

pred = prediction(phat.lgfit.selected, df.test$Retained.in.2012.)
perf <- performance(pred,"lift","rpp")
plot(perf, main="lift curve", colorize=T)

pred = prediction(phat.l1.lgfit, df.test$Retained.in.2012.)
perf <- performance(pred,"lift","rpp")
plot(perf, main="lift curve", colorize=T)

ProfitCurve <- function(benefitTP, benefitFN, benefitFP, benefitTN, y, phat){
  
  if(length(y) != length(phat)) stop("Length of y and phat not identical")
  if(length(levels(y))!=2 | levels(y)[1]!="0" | levels(y)[2]!="1")
    stop("y should be a vector of factors, only with levels 0 and 1")
  
  n <- length(y)
  df <- data.frame(y, phat)
  # Order phat so that we can pick the k highest groups for promotion
  df <- df[order(df[,2], decreasing = T),]
  TP <- 0; FP <- 0; FN <- table(y)[2]; TN <- table(y)[1]
  
  # Initializing the x and y coordinates of the plot
  ratio.vec <- seq(0,n)/n
  profit.vec <- rep(0,n+1)
  profit.vec[1] <- FN * benefitFN + TN * benefitTN
  
  for(k in 1:n){ # k is the number of groups classified as "YES"
    # In every round, we are picking one more group for promotion.
    # If this group was ratained (positive), then in this round, it is classified
    # as a "YES" instead of "NO" before. The confusion matrix is updated each round
    # with one more TP, and one less FN. It's similar when the group was not ratained.
    if(df[k,1]=="1"){TP <- TP + 1; FN <- FN - 1}
    else{FP <- FP + 1; TN <- TN - 1}
    #print(paste(TP, FP, TP-FP, benefitTP, benefitFP))
    profit.vec[k+1] <- TP*benefitTP + FP*benefitFP + FN*benefitFN + TN*benefitTN
  }
  
  plt <- plot(ratio.vec, profit.vec, type="l", lwd=2, col=4, main="Profit Curve",
              xlab="Percentage of Targetted Groups", ylab="Profit")
  abline(b=(profit.vec[n+1]-profit.vec[1]), a=profit.vec[1], lty=2) #Random guess
  return(plt)
}

profit_select = ProfitCurve(242,46,62,150,df.test$Retained.in.2012., phat.lgfit.selected)
profit_fit = ProfitCurve(241,50,63,146,df.test$Retained.in.2012., phat.l1.lgfit)






