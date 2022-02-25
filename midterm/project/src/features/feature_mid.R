#load in libraries
library(data.table)
library(caret)
library(dplyr)
set.seed(21)

#read in data
card_tab <- fread("./project/volume/data/raw/card_tab.csv")
train <- fread("./project/volume/data/raw/start_train.csv")
test <- fread("./project/volume/data/raw/start_test.csv")
set_tab <- fread("./project/volume/data/raw/set_tab.csv")

#merge train data with card_tab and set_tab
train_1 <- merge(train, card_tab, by="id")
train_2 <- merge(train_1, set_tab, by="set")

#select features from train data
final_train <- train_2[, c("id", "release_date", "future_date", "future_price")]
final_train <- distinct(final_train)

#merge test data with card_tab and set tab
test$future_price<-0
test_1 <- merge(test, card_tab, by="id")
test_2 <- merge(test_1, set_tab, by="set")

#select features from test data
final_test <- test_2[, c("id", "release_date", "future_date", "future_price")]

fwrite(final_train,'./project/volume/data/interim/train.csv')
fwrite(final_test,'./project/volume/data/interim/test.csv')


train <- fread("./project/volume/data/interim/train.csv")
test <- fread("./project/volume/data/interim/test.csv")

#add a column that easily differentiate between train and test row once they are together
test$train<-0
train$train<-1

#blind them together
master<-rbind(train,test)

#add in features
setkey(master,id)
setkey(card_tab,id)

card_tab$Legendary <-0
card_tab$Legendary[grep("Legendary",card_tab$supertypes)]<-1

types_tab<-as.data.table(tstrsplit(card_tab$types," "))
types_tab$id<-card_tab$id
m_types_tab<-melt(types_tab,id.vars = "id")
m_types_tab<-m_types_tab[!is.na(m_types_tab$value)]
m_types_tab$True<-1
types_tab<-dcast(m_types_tab,id ~ value,length,value.var="True")

master<-merge(master,card_tab[,.(id,rarity,cmc,Legendary)],all.x = T)
master<-merge(master,types_tab,all.x = T)


master[is.na(master)]<-0


# add in BS columns 
BS_data<-replicate(500,sample(c(1,0),nrow(master),replace = T))
BS_data<-data.table(BS_data)

setnames(BS_data,paste0("V",1:500),paste0("BS_",1:500))
master<-cbind(master,BS_data)

# split back to train/test #

train<-master[train==1]
test<-master[train==0]

#clean up columns
train$train<-NULL
test$train<-NULL
test$future_price<-NULL


# write out to interim #

fwrite(train,"./project/volume/data/interim/train_bs.csv")
fwrite(test,"./project/volume/data/interim/test_bs.csv")

