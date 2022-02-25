rm(list=ls())

library(data.table)
set.seed(9001)


# download.file(url="https://s3.amazonaws.com/stat.184.data/Flights/2008.csv", destfile='./project/volume/data/raw/2008.csv', method='curl')
# download.file(url="https://s3.amazonaws.com/stat.184.data/Flights/airports.csv",destfile='./project/volume/data/raw/airports.csv', method='curl')


dat <- fread('./project/volume/data/raw/Stat_380_train.csv')

pairs(SalePrice ~ OverallQual + BedroomAbvGr + GrLivArea, data = dat[1:100,])



dat[is.na(dat$SalePrice)]$SalePrice <- 0
dat <- dat[!is.na(dat$SalePrice)]


sub_dat <- dat[,.(Id,OverallQual,BedroomAbvGr,GrLivArea,SalePrice)]


rand_idx <- sample(1:nrow(dat),100)
test <- sub_dat[rand_idx,]
train <- sub_dat[!rand_idx,]
train <- train[1:1000]




fwrite(train,'./project/volume/data/interim/train.csv')
fwrite(test,'./project/volume/data/interim/test.csv')
