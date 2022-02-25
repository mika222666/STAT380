rm(list=ls())

#set my randomization seed
set.seed(9002)

library(ISLR)
library(data.table)

#the data I am working with shows market prices over 5 or 6 years
market_dat <- data.table(Smarket)
str(market_dat)

# market_dat$Today <- NULL

# logical operator checking if direction is up or not up
market_dat$Direction == "Up"
# multiply by 1 to change T F to 1 0
1*(market_dat$Direction == "Up")

# store the 1's and 0's into direction
market_dat$Direction <- 1*(market_dat$Direction == "Up")



train <- market_dat[!Year == 2005,]
test <- market_dat[Year == 2005,]

test$Year <- NULL
train$Year <- NULL

fwrite(train,"./project/volume/data/interim/train.csv")
fwrite(test,"./project/volume/data/interim/test.csv")