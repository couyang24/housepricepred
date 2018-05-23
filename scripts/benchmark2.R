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
summary(full)

# rm var with too many NA (Add-in) 
temp <- sapply(full, function(x) sum(is.na(x))) %>% as.data.frame() %>% rownames_to_column()

colnames(temp) <- c('var','num_miss')

rm_var <- temp %>% filter(num_miss>500) %>% select(var) %>% as.list()

rm_full <- full[, !colnames(full) %in% rm_var[[1]]]

# # fill NA
# reshaped <- full %>% gather(var,value, 2:80)
# 
# reshaped$value[is.na(reshaped$value)]<-0
# 
# full <- reshaped %>% spread(var,value)

# Factorize
num<-rm_full[,colnames(rm_full) %>% 
            str_detect("Id|Area|Price|SF|Porch|Yr|tage|Year|Val")] %>% lapply(as.numeric) %>% as_data_frame()
fac<-rm_full[,!colnames(rm_full) %>% 
            str_detect("Id|Area|Price|SF|Porch|Yr|tage|Year|Val")] %>% lapply(as.factor) %>% as_data_frame()

res_full <- bind_cols(num,fac)

colnames(res_full) <- colnames(res_full) %>% str_replace(START %R% DGT,"X")

rm(num,rm_full,fac,test,train,temp,rm_var)

# Fill NA
mice_mod<-mice(res_full,method='rf')
comp_full <- complete(mice_mod)

comp_full$SalePrice <- full$SalePrice

# Split
comp_train <- comp_full %>% filter(!is.na(SalePrice))
comp_test  <- comp_full %>% filter(is.na(SalePrice))


# Model
rf_model <- randomForest(SalePrice~.,data=comp_train)

rf_pred <- predict(rf_model,newdata=comp_test)

dim(comp_test)
length(rf_pred)

(solution <- data.frame(Id=comp_test$Id,SalePrice=rf_pred))

write.csv(solution,"rf_benchmark_solution2.csv",row.names = F)
