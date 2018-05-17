# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, mice, e1071, Metrics, skimr)

# Load datasets

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
full <- bind_rows(train,test)

full %>% skim()
full %>% summary()
sapply(full,function(x) sum(is.na(x)))

SalePrice <- train$SalePrice
Id <- test$Id

full[,c('Id','SalePrice')] <- NULL
rm(train,test)

chr <- full[,sapply(full,is.character)]
int <- full[,sapply(full,is.integer)]

fill_chr <- function(df){
  for(i in 1:ncol(df)){
    for(j in 1:nrow(df)){
      if(is.na(df[j,i])){
        df[j,i] = "Not Avaiable"
      }
    } 
  } 
  return(df)
}

chr <- fill_chr(chr)
fac <- chr %>% lapply(as.factor) %>% as.data.frame()

full <- bind_cols(fac,int)

micemod <- full %>% mice(method='rf')
full <- complete(micemod)

rm(chr,fac,int,fill_chr,micemod)

train <- full[1:length(SalePrice),]
test<-full[(length(SalePrice)+1):nrow(full),]

svm_model<-svm(SalePrice~.,data=train,cost = 3)
svm_pred <- predict(svm_model,newdata = test)
solution <- data.frame(Id=Id,SalePrice=svm_pred)
write.csv(solution,"svm_solution.csv",row.names = F)
