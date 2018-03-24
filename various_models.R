library(e1071)
library(Metrics)

train <- sdftrain
test<-sdftest
train <- train %>% mutate(SalePrice=price)

rm(completed_full,full,res_full,solution,rf_model,rf_pred,sdftest,sdftrain,stestcpy,straincpy,
   simpletrain,simpledf,sdf,deal_missing,price)

log_rmse <- function(x,y){
  rmse(log(x),log(y))
}

train$price <- NULL




for (i in 1:100){

InTrain <- createDataPartition(train$SalePrice,p=.7,list=F)
in_train <- train[InTrain,]
in_val <- train[-InTrain,]
rm(InTrain)

# svm
svm_model<-svm(SalePrice~.,data=in_train,cost = 1)
svm_pred <- predict(svm_model,newdata = in_val)

(cost1[i] <- log_rmse(in_val$SalePrice,svm_pred))


}

cost <- bind_cols(id=1:100,cost3=cost3,cost1=cost1)
cost %>% ggplot(aes(cost3)) + geom_density()+geom_vline(aes(xintercept = mean(cost3)))

mean(cost3)



# svm
svm_model<-svm(SalePrice~.,data=train)
svm_pred <- predict(svm_model,newdata = test)

solution <- data.frame(Id=id,SalePrice=svm_pred)

write.csv(solution,"svm_solution.csv",row.names = F)



# randomforest
rf_model<-randomForest(SalePrice~.,data=in_train)
rf_pred <- predict(rf_model,newdata = in_val)

log_rmse(in_val$SalePrice,rf_pred)

# caret rf
rf_caret_model <- train(SalePrice~.,data=in_train,method='rf')
rf_caret_pred<- predict(rf_caret_model,newdata = in_val)

log_rmse(in_val$SalePrice,rf_caret_pred)

# caret ranger
ranger_caret_model <- train(SalePrice~.,data=in_train,method='ranger')
ranger_caret_pred<- predict(ranger_caret_model,newdata = in_val)

log_rmse(in_val$SalePrice,ranger_caret_pred)

# caret xgboost
trControl <- trainControl(method="repeatedcv", number=5, repeats=5);
xgbGrid <- expand.grid(nrounds=c(600),
                       max_depth=8,
                       eta=0.123,
                       colsample_bytree=c(0.512),
                       subsample=.734,
                       gamma=0.0385,
                       min_child_weight=c(3))

(model.xgb <- train(SalePrice~.,data=in_train,trControl=trControl,method='xgbTree',
                    tuneGrid = xgbGrid))

xgb_caret_pred <- predict(model.xgb,newdata = in_val)

log_rmse(in_val$SalePrice,xgb_caret_pred)

ensemble <- .7*svm_pred+.3*xgb_caret_pred

log_rmse(in_val$SalePrice,ensemble)

solution <- data.frame(Id=id,SalePrice=ensemble)

write.csv(solution,"ensemble_solution.csv",row.names = F)

# caret xgboost
trControl <- trainControl(method="repeatedcv", number=5, repeats=5);
xgbGrid <- expand.grid(nrounds=c(600),
                       max_depth=8,
                       eta=0.123,
                       colsample_bytree=c(0.512),
                       subsample=.734,
                       gamma=0.0385,
                       min_child_weight=c(3))

(model.xgb <- train(SalePrice~.,data=train,trControl=trControl,method='xgbTree',
                    tuneGrid = xgbGrid))

xgb_caret_pred <- predict(model.xgb,newdata = test)

solution <- data.frame(Id=id,SalePrice=xgb_caret_pred)

write.csv(solution,"xgb_solution.csv",row.names = F)





# svm
(svm_caret<-train(SalePrice~.,data=in_train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='svmLinear3'))

svm_pred <- predict(svm_caret,in_val)

log_rmse(in_val$SalePrice,svm_pred)
rm(svm)


