
##########################################################
##########  Machine Learning  ############################
##########################################################

rm(list=ls())

# libraries 
library("tidyverse")  
library("DescTools")  # for winsorizing 
library("readxl")     # self explanatory
library("moments")    # self explanatory
library("envalysis")  # publishable graphs 
library("glmnet")     # lasso 
library("pls")        # pca regression and plots
library("gridExtra")  # for multiplots
library("stargazer")  # nice regression tables
library("caret")      # machine learning algos


as.data.frame(load("data.Rda"))




crypto <- 
  crypto %>% 
  mutate(y1 = as.factor(ifelse(ret>0,1,0)),
         y2 = as.factor(ifelse(ret_sqr >0.01,1,0)))

# check for na in data
anyNA(crypto)

# classification variable y: 
# Splitting data: 
training <- crypto[crypto$date<=as.Date("2021-12-31"),] # 66 %

# train df with simple ret 
train1 <- training[,c(17:26,40,43,45,47,49,51,53:54)]
# train df with ret^2
train2 <- training[,c(17:26,40,43,45,47,49,51,53,55)]

testing <-  crypto[crypto$date>as.Date("2021-12-31"),]  # 34 %
testing <- testing[,c(17:26,40,43,45,47,49,51,54,55)]


# Checking distribution in original data and partitioned data
prop.table(table(training$y1)) * 100
prop.table(table(testing$y1)) * 100
prop.table(table(crypto$y1)) * 100

prop.table(table(training$y2)) * 100
prop.table(table(testing$y2)) * 100
prop.table(table(crypto$y2)) * 100

########### LASSO ########################

Y <- train1$ret_ex

X = as.matrix(train1[,-c(17,18)])


lasso1 <- glmnet(x = X , y=Y , alpha = 1)

print(lasso1)

lasso_cv <- cv.glmnet(x = X, y = Y,
                      ## type.measure: loss to use for cross-validation.
                      type.measure = "mse",
                      ## K = 10 is the default.
                      nfold = 10,
                      ## 'alpha = 1' is the lasso penalty, and 'alpha = 0' the ridge penalty.
                      alpha = 1)
lasso_cv$lambda.min



coef(lasso_cv, s = lasso_cv$lambda.min)

# adaptive lasso 
best_ridge_coef <- as.numeric(coef(lasso_cv, s = lasso_cv$lambda.min))[-1]
lasso2 <-  glmnet(x = X, y = Y,
                  ## type.measure: loss to use for cross-validation.
                  ## 'alpha = 1' is the lasso penalty, and 'alpha = 0' the ridge penalty.
                  alpha = 1,
                  ##
                  ## penalty.factor: Separate penalty factors can be applied to each
                  ##           coefficient. This is a number that multiplies 'lambda' to
                  ##           allow differential shrinkage. Can be 0 for some variables,
                  ##           which implies no shrinkage, and that variable is always
                  ##           included in the model. Default is 1 for all variables (and
                  ##           implicitly infinity for variables listed in 'exclude'). Note:
                  ##           the penalty factors are internally rescaled to sum to nvars,
                  ##           and the lambda sequence will reflect this change.
                  penalty.factor = 1 / abs(best_ridge_coef))



lasso2_cv <- cv.glmnet(x = X, y = Y,
                                           ## type.measure: loss to use for cross-validation.
                                           type.measure = "mse",
                                           ## K = 10 is the default.
                                           nfold = 10,
                                           ## 'alpha = 1' is the lasso penalty, and 'alpha = 0' the ridge penalty.
                                           alpha = 1,
                                           ##
                                           ## penalty.factor: Separate penalty factors can be applied to each
                                           ##           coefficient. This is a number that multiplies 'lambda' to
                                           ##           allow differential shrinkage. Can be 0 for some variables,
                                           ##           which implies no shrinkage, and that variable is always
                                           ##           included in the model. Default is 1 for all variables (and
                                           ##           implicitly infinity for variables listed in 'exclude'). Note:
                                           ##           the penalty factors are internally rescaled to sum to nvars,
                                           ##           and the lambda sequence will reflect this change.
                                           penalty.factor = 1 / abs(best_ridge_coef),
                                           ## prevalidated array is returned
                                           keep = TRUE)


coef(lasso2_cv, s = lasso2_cv$lambda.min)


# check for na in data
anyNA(training)




############## Classification #################################################
train1 <- train1[,-c(17)]
train1$y1 <- as.factor(train1$y1)



# pcacomp for number of pcs considered, "cv" for cross validtion 
trctrl = trainControl(method = "cv",
                      number=5,
                      #preProcOptions = list(pcaComp=6),
                      verboseIter = TRUE) 





nnet_grid <- expand.grid(decay = c(0.5, 0.1, 1e-2, 1e-3), 
                         size = c(1,5, 10, 20),
                         bag = TRUE)

# neural network for returns 
set.seed(42)
nnfit1 <- train(y1 ~ ., 
                data = train1, 
                method = "avNNet", 
                preProcess=c("center","scale"),
                trControl = trctrl,
                na.action = na.omit,
                tuneGrid = nnet_grid,
                #tuneLength = 5,
                trace = FALSE)



nnpredict1 <- predict(nnfit1,newdata=testing)
confusionMatrix(nnpredict1, testing$y1)



# Tested the above setting in local machine
tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)
xgb_fit <- train(y1 ~., data = train1, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

xgbpredict1 <- predict(rf_fit,newdata=testing)
confusionMatrix(rfpredict1, testing$y1)














