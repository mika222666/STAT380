rm(list = ls())
library(httr)
library(data.table)

test <- fread('./project/volume/data/interim/test_data_clean.csv')
train <- fread('./project/volume/data/interim/train_data_clean.csv')

emb_dt <- NULL

getEmbeddings <- function(text){
  input <- list(
    instances = list(text)
  )
  res <- POST("https://dsalpha.vmhost.psu.edu/api/use/v1/models/use:predict", body = input,encode = "json", verbose())
  emb <- unlist(content(res)$predictions)
  emb
}


#loop goes here
for (i in 1:nrow(train)) {
  emb_train <- rbind(emb_dt, getEmbeddings(train$text[i]))
}

emb_train <- data.table(emb_train)

fwrite(emb_train,"./project/volume/data/interim/train_emb.csv")

for (i in 1:nrow(test)) {
  emb_test <- rbind(emb_dt, getEmbeddings(test$text[i]))
}

emb_test <- data.table(emb_test)

fwrite(emb_test,"./project/volume/data/interim/test_emb.csv")


