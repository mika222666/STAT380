rm(list = ls())
library(data.table)


#- Read in data
data <- fread("./project/volume/data/raw/test_data.csv")

#- Remove all the empty strings
data <- data[!data[,text == ""]]

#- Save the cleaned data
fwrite(data,"./project/volume/data/interim/test_data_clean.csv")

#- Read in data
data <- fread("./project/volume/data/raw/train_data.csv")

#- Remove all the empty strings
data <- data[!data[,text == ""]]

#- Save the cleaned data
fwrite(data,"./project/volume/data/interim/train_data_clean.csv")
