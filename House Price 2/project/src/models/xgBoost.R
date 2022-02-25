#- load in libraries
rm(list = ls())

library(data.table)
library(caret)
library(Metrics)
library(xgboost)

#advanced methods of hyperparameter tuning discussed here:
#https://rpubs.com/jeandsantos88/search_methods_for_hyperparameter_tuning_in_r


test <- fread("./project/volume/data/raw/Stat_380_test.csv")
train <- fread("./project/volume/data/raw/Stat_380_train.csv")
samp_sub <- fread("./project/volume/data/raw/Stat_380_sample_submission.csv")
#train <- train[1:50000,]

test$SalePrice <- 0

#----------------------------------#
#      Prep Data for Modeling      #
#----------------------------------#
y.train <- train$SalePrice
y.test <- test$SalePrice

dummies <- dummyVars(SalePrice~ ., data = train)
x.train <- predict(dummies, newdata = train)
x.test <- predict(dummies, newdata = test)



# notice that I've removed label=departure delay in the dtest line
dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)


# Initialize my table
hyper_perm_tune <- NULL
#----------------------------------#
#     Use cross validation         #
#----------------------------------#


# define parameters
param <- list(  objective           = "reg:squarederror",
                gamma               = 0.02,           #minimum loss reduction required
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.25,         #learning rate
                max_depth           = 3,      #max number of nodes
                subsample           = 1.0,      
                colsample_bytree    = 1.0,    
                tree_method = 'hist'  
)


XGBfit <- xgb.cv(params = param,
                 nfold = 5,
                 nrounds = 10000,   #the max number of iterations
                 missing = NA,      #By default is set to NA, which means that NA values should be considered as 'missing' by the algorithm. 
                 data = dtrain,
                 print_every_n = 1,
                 early_stopping_rounds = 15)   



best_tree_n <- unclass(XGBfit)$best_iteration
new_row <- data.table(t(param))
new_row$best_tree_n <- best_tree_n

test_error <- unclass(XGBfit)$evaluation_log[best_tree_n,]$test_rmse_mean
new_row$test_error <- test_error
hyper_perm_tune <- rbind(new_row, hyper_perm_tune)

#----------------------------------#
# fit the model to all of the data #
#----------------------------------#

watchlist <- list( train = dtrain)



XGBfit <- xgb.train( params = param,
                     nrounds = best_tree_n,
                     missing = NA,
                     data = dtrain,
                     watchlist = watchlist,
                     print_every_n = 1)



pred <- predict(XGBfit, newdata = dtest)



rmse(y.test,pred)

samp_sub$SalePrice<-pred
fwrite(samp_sub,"./project/volume/data/processed/submit_xg.csv")

