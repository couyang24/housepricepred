library(e1071)
library(Metrics)
library(glmnet)

train <- full[1:length(SalePrice),]
test<-full[(length(SalePrice)+1):nrow(full),]

# train <- train %>% mutate(SalePrice=price)

# rm(completed_full,full,res_full,solution,rf_model,rf_pred,sdftest,sdftrain,stestcpy,straincpy,
#    simpletrain,simpledf,sdf,deal_missing,price)

log_rmse <- function(x,y){
  rmse(log(x),log(y))
}

train$SalePrice <- SalePrice

InTrain <- createDataPartition(train$SalePrice,p=.7,list=F)
in_train <- train[InTrain,]
in_val <- train[-InTrain,]
rm(InTrain)

# svm
svm_model<-svm(SalePrice~.,data=in_train,cost = 3)
svm_pred <- predict(svm_model,newdata = in_val)

log_rmse(in_val$SalePrice,svm_pred)



# svm
svm_model<-svm(SalePrice~.,data=train,cost = 3)
svm_pred <- predict(svm_model,newdata = test)

solution <- data.frame(Id=Id,SalePrice=svm_pred)

write.csv(solution,"svm_solution.csv",row.names = F)



# randomforest
rf_model<-randomForest(SalePrice~.,data=in_train)
rf_pred <- predict(rf_model,newdata = in_val)

log_rmse(in_val$SalePrice,rf_pred)

# # caret rf
# rf_caret_model <- train(SalePrice~.,data=in_train,method='rf')
# rf_caret_pred<- predict(rf_caret_model,newdata = in_val)
# 
# log_rmse(in_val$SalePrice,rf_caret_pred)
# 
# # caret ranger
# ranger_caret_model <- train(SalePrice~.,data=in_train,method='ranger')
# ranger_caret_pred<- predict(ranger_caret_model,newdata = in_val)
# 
# log_rmse(in_val$SalePrice,ranger_caret_pred)

# caret xgboost
trControl <- trainControl(method="repeatedcv", number=10, repeats=10);
xgbGrid <- expand.grid(nrounds=c(1000),
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





# svm caret
(svm_caret<-train(SalePrice~.,data=in_train,
            trControl=trainControl(method="repeatedcv", number=5, repeats=5),
            method='svmLinear3'))

svm_pred <- predict(svm_caret,in_val)

log_rmse(in_val$SalePrice,svm_pred)
rm(svm)

# glmnet caret
(glmnet_caret<-train(SalePrice~.,data=in_train,
                  trControl=trainControl(method="repeatedcv", number=5, repeats=5),
                  method='glmnet'))

glmnet_pred <- predict(glmnet_caret,in_val)

log_rmse(in_val$SalePrice,glmnet_pred)
rm(glmnet)

# glmnet_h2o caret
(glmnet_h2o_caret<-train(SalePrice~.,data=in_train,
                     trControl=trainControl(method="repeatedcv", number=5, repeats=5),
                     method='glmnet_h2o'))

glmnet_h2o_pred <- predict(glmnet_h2o_caret,in_val)

log_rmse(in_val$SalePrice,glmnet_h2o_pred)
rm(glmnet_h2o)
