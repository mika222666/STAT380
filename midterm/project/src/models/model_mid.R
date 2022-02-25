#load in libraries
library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)

#read in data
train <- fread("./project/volume/data/interim/train_bs.csv")
test <- fread("./project/volume/data/interim/test_bs.csv")
samp_sub <- fread("./project/volume/data/raw/samp_sub.csv")

# prep data for modeling #
train$release_date<-as_date(train$release_date)
train<-train[order(-release_date)]

# subset out only the columns to model
drops<- c('id', 'future_date', 'release_date')
train<-train[, !drops, with=FALSE]
test<-test[, !drops, with = FALSE]

test$future_price<-0

keep<-c("rarity","cmc","future_price","Land","Artifact","BS_330","BS_331","BS_332","BS_333","BS_273","BS_274","BS_275")
train<-train[, keep, with=FALSE]
test<-test[, keep, with = FALSE]

#save the response var because dummyVars will remove
train_y<-train$future_price

test$future_price<-0

#work with dummies
dummies <- dummyVars(future_price ~., data = train)
train <- predict(dummies, newdata = train)
test<-predict(dummies, newdata = test)

train<-data.table(train)
test<-data.table(test)


# use cross validation #

train<-as.matrix(train)

test<-as.matrix(test)

gl_model<-cv.glmnet(train, train_y, alpha = 1, family="gaussian")

bestlam<-gl_model$lambda.min


# fit the model to all of the data #

gl_model<-glmnet(train, train_y, alpha = 1, family = "gaussian")

plot_glmnet(gl_model)

#save model
saveRDS(gl_model,"./project/volume/models/gl_model.model")

test<-as.matrix(test)

#use the full model
pred<-predict(gl_model,s=bestlam, newx = test)

bestlam
predict(gl_model,s=bestlam, newx = test, type = "coefficients")
gl_model


# make a submission file #

samp_sub$future_price<-pred

fwrite(samp_sub,"./project/volume/data/processed/submit_mid.csv")

