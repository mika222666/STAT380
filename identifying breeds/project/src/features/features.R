rm(list =ls())
library(data.table)
library(caret)

data <- fread("./project/volume/data/raw/data.csv")

#id was kinda like an ID, so delete it
data$id <- NULL

#solutionfile <- data[,.(type)]
solutionfile$id <- 1:nrow(solutionfile)
solutionID <- solutionfile$id
dumVars <- dummyVars(id~., data = solutionfile)
solutionfile <- predict(dumVars,newdata = solutionfile)
solutionfile <- data.table(solutionfile)
solutionfile$id <- solutionID
setnames(solutionfile,c("typered","typewhite"),c("Red","White"))
fwrite(solutionfile,"./project/volume/data/processed/solution.csv")