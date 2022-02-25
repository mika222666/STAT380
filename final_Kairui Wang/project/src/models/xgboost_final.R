#read library
library(keras)
library(tidyverse)
library(xgboost)
library(readr)
library(caret)
library(Rtsne)
library(Metrics)
library(data.table)
#advanced methods of hyperparameter tuning discussed here:
#https://rpubs.com/jeandsantos88/search_methods_for_hyperparameter_tuning_in_r


example_sub<- fread("./project/volume/data/raw/example_sub.csv")
test<-fread("./project/volume/data/interim/test_data_clean.csv")
tsne_dt_train<-fread("./project/volume/data/interim/tsne_dt_train.csv")
tsne_dt_test<-fread("./project/volume/data/interim/tsne_dt_test.csv")

#----------------------------------#
#      Prep Data for Modeling      #
#----------------------------------#

y.train <- tsne_dt_train$reddit
y.test <- tsne_dt_test$reddit


dummies <- dummyVars(reddit~ ., data = tsne_dt_train)
x.train <- predict(dummies, newdata = tsne_dt_train)
x.test <- predict(dummies, newdata = tsne_dt_test)



# make model matrix
dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)


# Initialize my table
hyper_perm_tune <- NULL
#----------------------------------#
#     Use cross validation         #
#----------------------------------#
num_class = length(levels(reddit))

# Set parameters(default)
params <- list(objective = "multi:softprob", 
               gamma = 0.005, 
               booster = "gbtree", 
               eval_metric = "mlogloss",
               eta= 0.02, 
               max_depth = 5, 
               min_child_weight = 3, 
               subsample=1, 
               colsample_bytree = 1, 
               num_class = 10, 
               tree_method = 'hist')

# Calculate # of folds for cross-validation
xgbcv <- xgb.cv(params = params, 
                nfold = 5, 
                data = dtrain, 
                nrounds = 10000, 
                missing = NA, 
                print_every_n = 1, 
                early_stopping_rounds = 25)



best_ntrees<-unclass(xgbcv)$best_iteration
new_row<-unlist(t(params))
new_row$best_ntrees<-best_ntrees

test_error<-unclass(xgbcv)$evaluation_log[best_ntrees,]$test_rmse_mean
new_row$test_error<-test_error
hyper_perm_tune<-rbind(new_row,hyper_perm_tune)

str(hyper_perm_tune)
hyper_perm_tune


#----------------------------------#
# fit the model to all of the data #
#----------------------------------#

watchlist <- list( train = dtrain)


XGBfit <- xgb.train( params = params,
                     nrounds = best_ntrees,
                     missing = NA,
                     data = dtrain,
                     watchlist = watchlist,
                     print_every_n = 1)



pred <- predict(XGBfit, newdata = dtest)



rmse(y.test,pred)





#===========================================


#transpose
trans <- as.data.frame(split(pred, 1:10))

#change name
colnames(trans) <- c("subredditcars", "subredditCooking", "subredditMachineLearning", "subredditMagicTCG", "subredditpolitics",
                     "subredditReal_Estate", "subredditscience", "subredditStockMarket", "subreddittravel", "subredditvideogames")

trans$id <- test$id

write.csv(trans, "./project/volume/data/processed/submit_file3.csv", row.names = FALSE)




#make a loop for find the best eta and tree_depth

#for (eta in seq(0,1, by = 0.05)) {
#for (max_depth in 0:15){
# Set parameters(default)
#params <- list(objective = "multi:softprob", 
#gamma = 0.005, 
#booster = "gbtree", 
#eval_metric = "mlogloss",
#eta= eta, 
#max_depth = max_depth, 
#min_child_weight = 3, 
#subsample=1, 
#colsample_bytree = 1, 
#num_class = 10, 
#tree_method = 'hist')

# Calculate # of folds for cross-validation
#xgbcv <- xgb.cv(params = params, 
#nfold = 5, 
#data = dtrain, 
#nrounds = 10000, 
#missing = NA, 
#print_every_n = 1, 
#early_stopping_rounds = 25)

#best_ntrees<-unclass(xgbcv)$best_iteration
#new_row<-unlist(t(params))
#new_row$best_ntrees<-best_ntrees

#test_error<-unclass(xgbcv)$evaluation_log[best_ntrees,]$test_rmse_mean
#print("eta = ", eta)
#print("max_depth = ", max_depth)
#print("test error = ", test_error)

#}
#}

