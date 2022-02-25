rm(list=ls())

library(caret) 
library(data.table)
library(Metrics)

set.seed(9002)

#Load files here
train <- fread('./project/volume/data/raw/train_file.csv')
test <- fread('./project/volume/data/raw/test_file.csv')


# save cause predict will drop this column
train_y <- train$result

test$result <- mean(train_y)
test_y <- test$result

dummies <- dummyVars(result ~ ., data = train)
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)

# predicts return a data frame, turn it back to data table
train <- data.table(train)
train$result <- train_y
test <- data.table(test)

#fit the logistic model
glm_fit <- glm(result ~ . , family = binomial, data = train)

summary(glm_fit)
coef(glm_fit)

#save the models
saveRDS(dummies,"./project/volume/models/Direction_lm.dummies")
saveRDS(glm_fit,"./project/volume/models/Direction_glm.model")

#need the type = response to get probabilities rarher than
#log likelihoods
test$result <- predict(glm_fit, newdata = test, type = "response")

#compare the logistic model with a null model
# ll stand for log loss
mean(ll(test_y,test$result))
# null model
mean(ll(test_y,0.5))


submission <- test[,.(id,result)]
#submission$result=round(submission$result,digit = 4)

fwrite(submission, "./project/volume/data/processed/submission.csv")
