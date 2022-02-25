library(httr)
library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(ClusterR)

set.seed(3)

train<-fread("./project/volume/data/interim/train_data_clean.csv")
test<-fread("./project/volume/data/interim/test_data_clean.csv")

train_emb<-fread("./project/volume/data/interim/train_emb.csv")
test_emb<-fread("./project/volume/data/interim/test_emb.csv")
#pca<-fread("./project/volume/data/interim/pca.csv")


#make a master table
train$train<-1
test$train<-0

train$order<-as.integer(1:200)
train2<-melt(train, id = c("id","text","order","train"), variable.name = "reddit")
train2<-train2[value==1][order(order)][,.(id,reddit,text,train)]

test$reddit<-NA
test2<-subset(test,select=c(id,reddit,text,train))

test_train<-rbind(train2,test)
test_train_emb<-rbind(train_emb,test_emb)

master<-cbind(test_train,test_train_emb)

fwrite(master,"./project/volume/data/interim/master.csv")

#- store and remove the id
id <- master$id
master$id <- NULL
reddit <- master$reddit
master$reddit <- NULL
text <- master$text
master$text <- NULL
Mtrain <- master$train
master$train <- NULL


#label processing
labels <- train$id
labels <- as.data.frame(labels)
labels$V1 <- train[, 3] * 1
labels$V2 <- train[, 4] * 2
labels$V3 <- train[, 5] * 3
labels$V4 <- train[, 6] * 4
labels$V5 <- train[, 7] * 5
labels$V6 <- train[, 8] * 6
labels$V7 <- train[, 9] * 7
labels$V8 <- train[, 10] * 8
labels$V9 <- train[, 11] * 9
labels$V10 <- train[, 12] * 10
labels$SUM <- labels$V1 + labels$V2 + labels$V3 + labels$V4 + labels$V5 + labels$V6 + labels$V7 + labels$V8 + labels$V9 + labels$V10
labels$SUM <- labels$SUM - 1
labels <- labels[, c(1,12)]



#jitter data
# master <- data.frame(lapply(master, jitter,factor=0.0001))

#dummies <- dummyVars(~., data=master)
#final<-predict(dummies, newdata=master)
pca <- prcomp(master)

# look at the percent variance explained by each pca
#screeplot(pca)

# look at the rotation of the variables on the PCs
#pca

# see the values of the scree plot in a table 
#summary(pca)

# see a biplot of the first 2 PCs
#biplot(pca)

# use the unclass() function to get the data in PCA space
pca_dt<-data.table(unclass(pca)$x)


# tsne
tsne1 <- Rtsne(pca_dt,pca = F, perplexity=15,check_duplicates = FALSE)
tsne2 <- Rtsne(pca_dt,pca = F, perplexity=20,check_duplicates = FALSE)
tsne3 <- Rtsne(pca_dt,pca = F, perplexity=30,check_duplicates = FALSE)
tsne4 <- Rtsne(pca_dt,pca = F, perplexity=40,check_duplicates = FALSE)
tsne5 <- Rtsne(pca_dt,pca = F, perplexity=10,check_duplicates = FALSE)
tsne6 <- Rtsne(pca_dt,pca = F, perplexity=50,check_duplicates = FALSE)
tsne7 <- Rtsne(pca_dt,pca = F, perplexity=65,check_duplicates = FALSE)
tsne8 <- Rtsne(pca_dt,pca = F, perplexity=80,check_duplicates = FALSE)

tsne1_tb <- data.table(tsne1$Y)
tsne2_tb <- data.table(tsne2$Y)
tsne3_tb <- data.table(tsne3$Y)
tsne4_tb <- data.table(tsne4$Y)
tsne5_tb <- data.table(tsne5$Y)
tsne6_tb <- data.table(tsne6$Y)
tsne7_tb <- data.table(tsne7$Y)
tsne8_tb <- data.table(tsne8$Y)

tsne2_tb$V3<-tsne2_tb$V1
tsne2_tb$V4<-tsne2_tb$V2
tsne3_tb$V5<-tsne3_tb$V1
tsne3_tb$V6<-tsne3_tb$V2
tsne4_tb$V7<-tsne4_tb$V1
tsne4_tb$V8<-tsne4_tb$V2
tsne5_tb$V9<-tsne5_tb$V1
tsne5_tb$V10<-tsne5_tb$V2
tsne6_tb$V11<-tsne6_tb$V1
tsne6_tb$V12<-tsne6_tb$V2
tsne7_tb$V13<-tsne7_tb$V1
tsne7_tb$V14<-tsne7_tb$V2
tsne8_tb$V15<-tsne8_tb$V1
tsne8_tb$V16<-tsne8_tb$V2

tsne2_tb<-subset(tsne2_tb,select=c(V3,V4))
tsne3_tb<-subset(tsne3_tb,select=c(V5,V6))
tsne4_tb<-subset(tsne4_tb,select=c(V7,V8))
tsne5_tb<-subset(tsne5_tb,select=c(V9,V10))
tsne6_tb<-subset(tsne6_tb,select=c(V11,V12))
tsne7_tb<-subset(tsne7_tb,select=c(V13,V14))
tsne8_tb<-subset(tsne8_tb,select=c(V15,V16))

tsne_dt <- cbind(tsne1_tb, tsne2_tb, tsne3_tb, tsne4_tb, tsne5_tb, tsne6_tb, tsne7_tb, tsne8_tb)
tsne_dt<-subset(tsne_dt,select=c(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16))

fwrite(tsne_dt,"./project/volume/data/interim/tsne_dt.csv")

#tsne_dt<-fread("./project/volume/data/interim/tsne_dt.csv")

# add back in reddit and train
tsne_dt$reddit<-reddit
tsne_dt$train<-Mtrain

tsne_dt_train<-subset(tsne_dt,train==1)
tsne_dt_test<-subset(tsne_dt,train==0)

tsne_dt_train<-subset(tsne_dt_train,select=-c(train))
tsne_dt_test<-subset(tsne_dt_test,select=-c(train))

tsne_dt_train$reddit <- labels$SUM


fwrite(tsne_dt_train,"./project/volume/data/interim/tsne_dt_train.csv")
fwrite(tsne_dt_test,"./project/volume/data/interim/tsne_dt_test.csv")

