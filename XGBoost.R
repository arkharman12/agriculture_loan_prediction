######### GBM XGBoost #############

library(tidyverse)
library(ggplot2)
library(rsample)

setwd("/Users/singhh/Downloads/CSCI48900/agriculture-loan-prediction")

set.seed(42)
X <- as_tibble(read_csv('train_comp1_2020.csv'))
Xt <- as_tibble(read_csv('test_comp1_2020.csv'))

colnames(Xt)

cc <- c('CreditScore','TYCornUnits','TYBeanUnits','TYWheatUnits',
        'LYCornUnits','LYBeanUnits','LYWheatUnits', 'AT36', 'G093')

all <- rbind(X[cc],Xt[cc])

all$TYCornUnits[is.null(all$TYCornUnits)] <- 1
all$TYBeanUnits[is.null(all$TYBeanUnits)] <- 1
all$TYWheatUnits[is.null(all$TYWheatUnits)] <- 1
all$LYCornUnits[is.null(all$LYCornUnits)] <- 1
all$LYBeanUnits[is.null(all$LYBeanUnits)] <- 1
all$LYWheatUnits[is.null(all$LYWheatUnits)] <- 1
all$AT36[is.null(all$AT36)] <- 1
all$G093[is.null(all$G093)] <- 1


all$TYCornUnits <- as.numeric(as.factor(all$TYCornUnits))
all$TYBeanUnits <- as.numeric(as.factor(all$TYBeanUnits))
all$TYWheatUnits <- as.numeric(as.factor(all$TYWheatUnits))
all$LYCornUnits <- as.numeric(as.factor(all$LYCornUnits))
all$LYBeanUnits <- as.numeric(as.factor(all$LYBeanUnits))
all$LYWheatUnits <- as.numeric(as.factor(all$LYWheatUnits))
all$AT36 <- as.numeric(as.factor(all$AT36))
all$G093 <- as.numeric(as.factor(all$G093))

label <- X$PastDue

new_all <- model.matrix(~.+0, data=all)
new_x <- new_all[1:nrow(X),]
new_xt <- new_all[(nrow(X)+1):nrow(new_all),]

ind_tr <- sample(c(1:nrow(new_x)), round(nrow(new_x)*0.7))

new_tr <- new_x[ind_tr,]
new_ts <- new_x[-ind_tr,]

label_tr <- label[ind_tr]
label_ts <- label[-ind_tr]

dtrain <- xgb.DMatrix(data = new_tr,label = label_tr)
dtest <- xgb.DMatrix(data = new_ts,label= label_ts)


params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, 
               gamma=0, 
               max_depth=6, 
               min_child_weight=1, 
               subsample=1, 
               colsample_bytree=1)


xgb1 <- xgb.train(params = params, 
                  data = dtrain, 
                  nrounds = 1000, 
                  watchlist = list(train=dtrain,val=dtest), 
                  print_every_n = 10, 
                  early_stop_round = 10, 
                  maximize = F , 
                  eval_metric = "error")

xgbpred <- predict(xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

acc <- mean(xgbpred == label_ts)
print(acc)


mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat) 
