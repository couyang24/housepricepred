library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(Amelia)
library(mice)
library(lattice)
library(rpart)
library(xgboost)
library(e1071)

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
price = train[,c("SalePrice")]

simpledf <- rbind(train[,-81], test)

deal_missing <- function(simpledf){
  for(i in 1:ncol(simpledf))
  {
    u <- simpledf[,i]
    if (is.numeric(u))
    {
      simpledf[is.na(u),i] <- median(simpledf[!is.na(u),i])
    } else
    {
      u[is.na(u)] <- "Not Available"
      simpledf[,i] <- as.factor(u)
    }
  }
  return(simpledf)
}

simpletrain <- deal_missing(simpledf)
sdf = simpletrain

sdftrain = sdf[1:1460,]
sdftrain = cbind(sdftrain,price)
sdftest = sdf[1461:2919,]
sdftrain = sdftrain[,-1]
id = sdftest[,1]
sdftest = sdftest[,-1]

straincpy = sdftrain
straincpy[sapply(straincpy, is.factor)] <- lapply(straincpy[sapply(straincpy, is.factor)], as.numeric)
stestcpy = sdftest
stestcpy[sapply(stestcpy, is.factor)] <- lapply(stestcpy[sapply(stestcpy, is.factor)], as.numeric)

sdftrain %>% str()

model.svm <- svm(price ~ ., data = sdftrain, cost = 1)
price.svm = predict(model.svm, sdftest)
submission = cbind(Id = id, SalePrice = price.svm)
colnames(submission) = c("Id","SalePrice")

write.csv(submission,file="svm11_submission.csv",row.names=FALSE)