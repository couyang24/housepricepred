cost1=c()
cost2=c()
cost3=c()
cost4=c()
cost5=c()

for (i in 1:1000){
  
  InTrain <- createDataPartition(train$SalePrice,p=.7,list=F)
  in_train <- train[InTrain,]
  in_val <- train[-InTrain,]
  rm(InTrain)
  
  # svm
  svm_model<-svm(SalePrice~.,data=in_train,cost = 1)
  svm_pred <- predict(svm_model,newdata = in_val)
  
  cost1[i] <- log_rmse(in_val$SalePrice,svm_pred)
  
  # svm
  svm_model<-svm(SalePrice~.,data=in_train,cost = 2)
  svm_pred <- predict(svm_model,newdata = in_val)
  
  cost2[i] <- log_rmse(in_val$SalePrice,svm_pred)
  
  # svm
  svm_model<-svm(SalePrice~.,data=in_train,cost = 3)
  svm_pred <- predict(svm_model,newdata = in_val)
  
  cost3[i] <- log_rmse(in_val$SalePrice,svm_pred)
  
  # svm
  svm_model<-svm(SalePrice~.,data=in_train,cost = 4)
  svm_pred <- predict(svm_model,newdata = in_val)
  
  cost4[i] <- log_rmse(in_val$SalePrice,svm_pred)
  
  # svm
  svm_model<-svm(SalePrice~.,data=in_train,cost = 5)
  svm_pred <- predict(svm_model,newdata = in_val)
  
  cost5[i] <- log_rmse(in_val$SalePrice,svm_pred)
}

cost <- bind_cols(id=1:1000,cost1=cost1,cost2=cost2,cost3=cost3,cost4=cost4,cost5=cost5) %>% 
  gather(cost,value,2:6)

cost %>% group_by(cost) %>% summarise(averagescore=median(value))



cost %>% ggplot(aes(value,col=cost)) + geom_density()+
  geom_vline(aes(xintercept = mean(value)))

write.csv(cost,"cost.csv",row.names = F)
