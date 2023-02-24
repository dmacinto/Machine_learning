library(MASS)
library(bestglm)
library(dplyr)
library(glmnet)

# setwd("~/Downloads")
housing.train <- read.csv("housing_train.csv")
housing.test <- read.csv("housing_test.csv")

# bestglm requires that "y" is the last column, so we will do just that
# I use the solution in 
# https://stackoverflow.com/questions/43897844/r-move-column-to-last-using-dplyr
housing.train$Log_Sale_Price <- log(housing.train$Sale_Price)
housing.train <- housing.train %>% dplyr::select(-Sale_Price)
housing.train <- housing.train %>% dplyr::select(-Log_Sale_Price, Log_Sale_Price)

lm.fit <- lm(Log_Sale_Price~., as.data.frame(housing.train))
yhat <- exp(predict(lm.fit, as.data.frame(housing.test)))


# part 2
fwd.step.bic <- bestglm(housing.train, 
                        family = gaussian, 
                        IC     = "BIC",
                        method = "forward")

fwd.step.cv <- bestglm(housing.train, 
                       family = gaussian, 
                       IC     = "CV",
                       method = "forward")


yhat = exp(predict(fwd.step.bic$BestModel, as.data.frame(housing.test)))
sampleSubmission = data.frame(Id=1:length(yhat), Sale_Price=yhat)
write.csv(sampleSubmission,
          file = "sampleSubmission.csv",
          row.names = FALSE,
          quote = FALSE)


# step 1 for part 3
# using glmnet to fit and tune a lasso model
housing.lasso <- cv.glmnet(
  x=as.matrix(housing.train %>% dplyr::select(-Log_Sale_Price)),
  y=housing.train$Log_Sale_Price,
  family='gaussian',
  alpha=1
)









in.train=createDataPartition(housing.train$Log_Sale_Price,p=.75,list=FALSE)
train.df=housing.train[in.train, ]
test.df = housing.train[-in.train, ]


library(Boruta)
inVars=sample(nrow(train.df))
vs.boruta<-Boruta(train.df$Log_Sale_Price~.,data=train.df[inVars,],maxRuns=500,doTrace=0)

plot(vs.boruta,xlab="",xaxt="n")
lz<-lapply(1:ncol(vs.boruta$ImpHistory),function(i)
  vs.boruta$ImpHistory[is.finite(vs.boruta$ImpHistory[,i]),i])
names(lz)<-colnames(vs.boruta$ImpHistory)
lb<-sort(sapply(lz,median))
axis(side=1,las=2,labels=names(lb),at=1:ncol(vs.boruta$ImpHistory),cex.axis=0.5,font=4)


s_variables=names(vs.boruta$finalDecision)[vs.boruta$finalDecision %in% c("Confirmed","Tentative")]
s_variables

housing.train.fit <- dplyr::select(housing.train, all_of(s_variables) | Log_Sale_Price)
housing.train.fit.model <- dplyr::select(train.df, all_of(s_variables) | Log_Sale_Price)

library(ranger)
library(gbm)


getNonRejectedFormula(vs.boruta)
getConfirmedFormula(vs.boruta)


#fit_gbm = gbm(housing.train$Log_Sale_Price ~ Lot_Frontage + Lot_Area + Year_Built + 
#                Year_Remod_Add + Bsmt_Unf_SF + First_Flr_SF + Full_Bath + 
#                TotRms_AbvGrd + Garage_Area + Wood_Deck_SF + Open_Porch_SF + 
#                Enclosed_Porch + Longitude + Latitude + MS_SubClass_other + 
#                Lot_Shape_other + Overall_Cond_Above_Average + Exterior_2nd_VinylSd + 
#                Mas_Vnr_Type_other + Exter_Qual_Typical + Bsmt_Qual_Typical + 
#                Kitchen_Qual_Typical + Garage_Type_Detchd + Garage_Finish_Unf + 
#                Gr_Liv_Area, data = housing.train, distribution = "gaussian",
#              interaction.depth = 1, n.trees = 5000, shrinkage = 0.01,
#              cv.folds = 5, n.cores = 1, verbose = FALSE)
#yhat.rf = exp(predict(fit_gbm, housing.test))

######creating the boosted model


hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3), ## controls the learning rate
  interaction.depth = c(3, 5, 10), ## tree depth
  n.minobsinnode = c(5, 10, 30), ## minimum number of observations required in each terminal node
  bag.fraction = seq(.7, .8, .01), ## percent of training data to sample for each tree
  optimal_trees = 0, # a place to dump results
  min_RMSE = 0 # a place to dump results
)



X.train = as.matrix(housing.train.fit.model[,1:29])
Y.train = train.df$Log_Sale_Price

for(i in 1:nrow(hyper_grid)) {
  # create parameter list
  params <- list(
    eta = hyper_grid$shrinkage[i],
    max_depth = hyper_grid$interaction.depth[i],
    min_child_weight = hyper_grid$n.minobsinnode[i],
    subsample = hyper_grid$bag.fraction[i]
  )
  # reproducibility
  set.seed(1)
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = X.train,
    label = Y.train,
    nrounds = 3000,
    nfold = 5,
    objective = "reg:squarederror", # for regression models
    verbose = 0, # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

oo = hyper_grid[order(hyper_grid$min_RMSE),]


# parameter list
params <- list(
  eta = oo[1,]$shrinkage,
  max_depth = oo[1,]$interaction.depth,
  min_child_weight = oo[1,]$n.minobsinnode,
  subsample = oo[1,]$bag.fraction
)



X.train = as.matrix(housing.train.fit.model[,1:29])
Y.train = train.df$Log_Sale_Price
# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = as.matrix(housing.train[,1:99]),
  label = housing.train$Log_Sale_Price,
  nrounds = oo[1,]$optimal_trees,
  objective = "reg:squarederror",
  verbose = 0
)





yhat = exp(predict(xgb.fit.final, as.matrix(housing.test)))

sampleSubmission = data.frame(Id=1:length(yhat), Sale_Price=yhat)
write.csv(sampleSubmission,
          file = "sampleSubmission.csv",
          row.names = FALSE,
          quote = FALSE)

