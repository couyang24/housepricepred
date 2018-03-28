library(tidyverse)
library(mice)

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
full <- bind_rows(train,test)

full %>% str()
full %>% summary()
sapply(full,function(x) sum(is.na(x)))

SalePrice <- train$SalePrice
Id <- test$Id

full[,c('Id','SalePrice')] <- NULL

rm(train,test)

(chr <- full[,sapply(full,is.character)])
(int <- full[,sapply(full,is.integer)])

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
