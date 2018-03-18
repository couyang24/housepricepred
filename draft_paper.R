str(train)
str(test)

rm_var <- temp %>% filter(num_miss>500) %>% select(var) %>% as.list()

full <- full[, !colnames(full) %in% rm_var[[1]]]

library(mice)
library(randomForest)

full <- lapply(full,function(x) as.numeric(as.factor(x))) %>% as.data.frame()

mice_mod<-mice(full[,!colnames(full) %in% c("SalePrice")],method='rf')
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

write_csv(solution,"rf_benchmark_solution.csv")
