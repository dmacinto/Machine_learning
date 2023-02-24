#Homework 5

library(modelsummary)
library(dplyr)
library(bestglm)
library(glmnet)
set.seed(123)


housing.train = read.csv('housing_train.csv')
housing.test = read.csv('housing_test.csv')



#Part 1
hist (housing.train$Sale_Price)
hist (log(housing.train$Sale_Price))
plot(housing.train$Gr_Liv_Area, housing.train$Sale_Price)
plot(housing.train$Gr_Liv_Area, log(housing.train$Sale_Price))



#Part 2
#Data prep
housing.train$Sale_Price <- log(housing.train$Sale_Price)
housing.train <- housing.train %>% 
  dplyr::select(-Sale_Price, Sale_Price)


#Forward Step BIC
fwd.step.bic <- bestglm(housing.train,
                        family = gaussian,
                        IC     = "BIC",
                        method = "forward")

dev.plot = -2 * fwd.step.bic$Subsets$logLikelihood
bic.plot = fwd.step.bic$Subsets$BIC
plot(dev.plot, lwd=2, col='black', type='p',
     xlab='Subset Size', ylab='Deviance', main='All subsets')
points(bic.plot, lwd=2, col='red')
legend("topright", legend=c("deviance", "BIC"), col=c("black", "red"), pch=1)

#Forward Step CV
fwd.step.cv = bestglm(housing.train, 
                               family = gaussian, 
                               IC     = "CV",
                               CVArgs = list(Method="HTF", K=10, REP=1),
                               method = "forward")

fwd.step.cv$Subsets
dev.plot = -2 * fwd.step.cv$Subsets$logLikelihood
cverrs = fwd.step.cv$Subsets$CV
sdCV = fwd.step.cv$Subsets$sdCV
CVLo = cverrs - sdCV
CVHi = cverrs + sdCV
k = 0:(length(cverrs)-1)
plot(k, cverrs, xlab="Subset Size", ylab="CV Error", main='All subsets',
     ylim=c(min(CVLo),max(CVHi)), type="n")
points(k, cverrs, cex=2, col="red", pch=16)
lines(k, cverrs, col="red", lwd=2)
# plot error bars
segments(k, CVLo, k, CVHi, col="blue", lwd=2)
eps = 0.15
segments(k-eps, CVLo, k+eps, CVLo, col="blue", lwd=2)
segments(k-eps, CVHi, k+eps, CVHi, col="blue", lwd=2)

indBest = oneSDRule(fwd.step.cv$Subsets[,c("CV", "sdCV")])
abline(v=indBest-1, lty=2)
indMin = which.min(cverrs)
fmin = sdCV[indMin]
cutOff = fmin + cverrs[indMin]
abline(h=cutOff, lty=2)
indMin = which.min(cverrs)
abline(v=indMin-1, lty=2)

points(dev.plot, lwd=2, col='black')
legend("topright", legend=c("deviance", "CV"), col=c("black", "red"), pch=1)



#Find min cv (cvm) and associated std dev (sm). 

#Find the lowest subset size with cv within the [cvm -sm, cvm + sm] (generally on the left of the min cv subset size.)
##### 1 sigma 






#msummary(list('BIC' = best.subset.model.bic$BestModel,
#              'CV' = best.subset.model.cv$BestModel))



######2.3 Lasso models


## Lasso fit

#Lasso regression with the `glmnet()` function 
#has as a first argument the design matrix and as a second argument the response vector.
#figure out column length with ncol()
x = as.matrix( housing.train[,1:99] )
y = housing.train[,100]
lasso.fit = glmnet(x, y)
plot( lasso.fit, xvar = "lambda" )
plot( lasso.fit, xvar = "norm" )


#Clearly the regression coefficients are smaller for the lasso regression than for least squares.

#data.frame(lasso = as.matrix(coef(lasso.fit, s = c(5, 1, 0.5))),
#          LS    = coef(lm(y ~ x)))

#We observe that the lasso

#- Can shrink coefficients exactly to zero
#- Simultaneously estimates and selects coefficients


#We need to choose lambda.

housing.lasso <- cv.glmnet(
  x=as.matrix(housing.train %>% dplyr::select(-Log_Sale_Price)),
  y=housing.train$Log_Sale_Price,
  family='gaussian',
  alpha=1
)


lasso.cv = cv.glmnet(x, y)
plot(housing.lasso, sign.lambda=-1)


glmnet.fit <- housing.lasso$glmnet.fit






plot(glmnet.fit, xvar = "lambda")
abline(v = log(lasso.cv$lambda.min), lty=2, col="red")
abline(v = log(lasso.cv$lambda.1se), lty=2, col="green")
legend("topright", legend=c("min", "1se"), lty=2, col=c("red", "green"))

#Let us investigate coefficients for these values of lambda:
#lasso.1se.coef=coef(lasso.cv, s = c(lasso.cv$lambda.min, lasso.cv$lambda.1se))
lasso.1se.coef=coef(lasso.cv$glmnet.fit, s = lasso.cv$lambda.1se)


lasso.1se.coef = coef(lasso.fit$glmnet.fit, s=lasso.fit$lambda.1se)

lasso.1se.coef[,1]=0








#Q4
housing.train$Log_Sale_Price <- log(housing.train$Sale_Price)
housing.train <- housing.train %>% dplyr::select(-Sale_Price)
housing.train <- housing.train %>% dplyr::select(-Log_Sale_Price, Log_Sale_Price)



in.train=createDataPartition(housing.train$Log_Sale_Price,p=.75,list=FALSE)
train.df=housing.train[in.train, ]
test.df = housing.train[-in.train, ]



fwd.step.bic <- bestglm(train.df,
                        family = gaussian,
                        IC     = "BIC",
                        method = "forward")

fwd.step.cv = bestglm(train.df, 
                      family = gaussian, 
                      IC     = "CV",
                      CVArgs = list(Method="HTF", K=10, REP=1),
                      method = "forward")


housing.lasso <- cv.glmnet(
  x=as.matrix(train.df %>% dplyr::select(-Log_Sale_Price)),
  y=train.df$Log_Sale_Price,
  family='gaussian',
  alpha=1
)

housing.lasso$glmnet.fit

yhat.bic = predict(fwd.step.bic$BestModel, data = train.df)
mean((train.df$Log_Sale_Price - yhat.bic) ^ 2)

yhat.cv = predict(fwd.step.cv$BestModel, data = train.df)
mean((train.df$Log_Sale_Price - yhat.cv) ^ 2)

lasso.1se.coef=coef(housing.lasso, s = housing.lasso$lambda.1se)
mean((train.df$Log_Sale_Price - housing.lasso$glmnet.fit$lambda) ^ 2)

lasso.cv.coef=coef(housing.lasso, s = housing.lasso$lambda.min)
mean((validating$log_count - yhat.rf) ^ 2)


mean((validating$log_count - yhat.rf) ^ 2)

















x.test <- as.matrix(housing.test)
yhat = predict(lasso.cv, x.test, s = lasso.cv$lambda.min)
#should give the final predictions. Here s is the lambda we want to use for the predictions (which can be lambda.1se or lambda.min). x.test can be obtained by calling x.test <- as.matrix(housing.test),

#yhat = exp(predict(fwd.step.cv$BestModel, as.data.frame(housing.test)))


yhat = exp(predict(lasso.cv$glmnet.fit, as.matrix(housing.test), s= lasso.cv$lambda.1se))
#sampleSubmission is a dataframe with columns 'Id' and 'count'
sampleSubmission = data.frame(Id=1:length(yhat), Sale_Price=yhat)
write.csv(sampleSubmission,
          file = "sampleSubmission.csv",
          row.names = FALSE,
          quote = FALSE)




x.test <- as.matrix(housing.test)
yhat = predict(lasso.cv, x.test, s = lasso.cv$lambda.min)
sampleSubmission = data.frame(Id=1:length(yhat), Sale_Price=yhat)
write.csv(sampleSubmission,
          file = "sampleSubmission.csv",
          row.names = FALSE,
          quote = FALSE)


