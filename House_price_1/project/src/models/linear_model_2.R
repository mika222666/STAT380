rm(list=ls())

library(caret)
library(data.table)
library(Metrics)
library(dplyr)


set.seed(9001)

train <- fread('./project/volume/data/interim/train.csv')
test <- fread('./project/volume/data/interim/test.csv')

test$SalePrice <- 0
train_y <- train$SalePrice
test_y <- test$SalePrice

train <- select (train,-c(SalePrice))

# y= b0+b1x+.... where y is DepDelay and x are other variables
dummies <- dummyVars( ~ ., data = train)
dummies

# data frame to data table
train <- predict(dummies, newdata = train)
train <- data.table(train)

test <- predict(dummies, newdata = test)
test <- data.table(test)

#appending the original data back
train$SalePrice <- train_y

#finally create the linear model
# "SalePrice~" read as SalePrice as predicted by"
fit <- lm(SalePrice~., data = train)

#model assessment
summary(fit)

#save my linear model
saveRDS(dummies,"./project/volume/models/SalePrice_linear_model.dummies")
saveRDS(fit,"./project/volume/models/fit_lm.model")

#predict saleprice for test data 
test$SalePrice <- predict(fit,newdata = test)

# create a null model for comparison
avg_price <- mean(train_y)

#fill the NA in SalePrice of test data with the average value from train data
test[is.na(test$SalePrice)]$SalePrice <- avg_price

#create a colimn in my test set that contains the average as a predict
test$Null_model <- avg_price

#Compare lm with NULL model
rmse(test_y,test$Null_model)
rmse(test_y,test$SalePrice)




submit <- test[,.(Id,SalePrice)]

fwrite(submit,'./project/volume/data/processed/submit_lm.csv')