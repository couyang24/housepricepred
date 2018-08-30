library(tidyverse)
library(skimr)
library(mice)
library(e1071)

train <- read.csv("input/train.csv", stringsAsFactors = F)
test <- read.csv("input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)


setdiff(names(train), names(test))
full %>% skim() %>% kable()
price <- train$SalePrice
id <- test$Id

full[,c("SalePrice", "Id")] <- NULL
rm(train, test)

chr <- full[, sapply(full, is.character)]
int <- full[, sapply(full, is.integer)]
rm(full)

chr[is.na(chr)] <- "Not Applicable"
fac <- chr %>% lapply(as.factor) %>% data.frame()

full <- bind_cols(int, fac)
micemod <- mice(full, method = "rf")
full <- complete(micemod)

rm(chr,fac,int,micemod)

train <- full[1:length(price),]
test <- full[length(price)+1:nrow(full),]

svm_model <- svm(price~., train, cost = 3)
svm_pred <- predict(svm_model, test)
solution <- data.frame(Id=id, SalePrice=svm_pred)
write.csv(solution,"svm_solution.csv",row.names = F)
