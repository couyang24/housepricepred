# Load packages
library(tidyverse)
library(randomForest)
library(ggthemes)
library(corrplot)
library(caret)
library(gridExtra)
library(mice)
library(rebus)

# Load datasets

train<-read_csv("train.csv")
test<-read_csv("test.csv")
full <- bind_rows(train,test)

str(full)

reshaped <- full %>% gather(var,value, 2:80)

reshaped$value[is.na(reshaped$value)]<-0

res_full <- reshaped %>% spread(var,value)

res_full2 <- lapply(res_full,as.factor) %>% as_data_frame()

num <- res_full2[,sapply(res_full2,function(x) length(unique(x)))>15] %>% lapply(as.numeric) %>% as_data_frame()
fac <- res_full2[,sapply(res_full2,function(x) length(unique(x)))<=15]

full <- bind_cols(num,fac)

rm(res_full,res_full2,num,fac,test,train,reshaped)

train <- full %>% filter(!is.na(SalePrice))
test  <- full %>% filter(is.na(SalePrice))

train %>% str()


rf_model <- train %>% randomForest(SalePrice~.)
