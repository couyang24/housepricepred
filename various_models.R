library(e1071)
library(Metrics)

rm(completed_full,full,res_full,solution,rf_model,rf_pred)

InTrain <- createDataPartition(train$SalePrice,p=.7,list=F)
in_train <- train[InTrain,]
in_val <- train[-InTrain,]
rm(InTrain)

# svm
svm_model<-svm(SalePrice~.,data=in_train)
svm_pred <- predict(svm_model,newdata = in_val)

rmse(in_val$SalePrice,svm_pred)

# randomforest
rf_model<-randomForest(SalePrice~.,data=in_train)
rf_pred <- predict(rf_model,newdata = in_val)

rmse(in_val$SalePrice,rf_pred)

# caret rf
rf_caret_model <- train(SalePrice~.,data=in_train,method='rf')
rf_caret_pred<- predict(rf_caret_model,newdata = in_val)

rmse(in_val$SalePrice,rf_caret_pred)

# caret ranger
ranger_caret_model <- train(SalePrice~.,data=in_train,method='ranger')
ranger_caret_pred<- predict(ranger_caret_model,newdata = in_val)

rmse(in_val$SalePrice,ranger_caret_pred)

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

xgb_caret_pred <- predict(model.xgb,test)

#rmse(in_val$SalePrice,xgb_caret_pred)

solution <- data.frame(Id=test$Id,SalePrice=xgb_caret_pred)

write.csv(solution,"xgb_benchmark_solution.csv",row.names = F)





# svm
(svm_caret<-train(SalePrice~.,data=in_train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='svmLinearWeights'))

svm_test_pred <- predict(svm_caret,in_val)
rm(svm)


