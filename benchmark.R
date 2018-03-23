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


# fill NA
temp <- sapply(full, function(x) sum(is.na(x))) %>% as.data.frame() %>% rownames_to_column()

colnames(temp) <- c('var','num_miss')

rm_var <- temp %>% filter(num_miss>500) %>% select(var) %>% as.list()

full <- full[, !colnames(full) %in% rm_var[[1]]]

full <- lapply(full,function(x) as.numeric(as.factor(x))) %>% as.data.frame()
# 
# full <- lapply(full,function(x) as.factor(x)) %>% as.data.frame()


mice_mod<-mice(full,method='rf')
completed_full <- complete(mice_mod)

summary(completed_full)

full <- bind_rows(train,test)
completed_full$SalePrice <- full$SalePrice
rm(rm_var,temp,train,test,mice_mod,full)

sapply(completed_full, function(x) sum(is.na(x)))

train <- completed_full %>% filter(!is.na(SalePrice))
test <- completed_full %>% filter(is.na(SalePrice))
rf_model <- randomForest(SalePrice~.,train)
rf_pred <- predict(rf_model,test)

solution <- data.frame(Id=test$Id,SalePrice=rf_pred)

solution %>% View()

write.csv(solution,"rf_benchmark_solution.csv",row.names = F)
