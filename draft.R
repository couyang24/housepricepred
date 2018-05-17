library(DescTools)
library(knitr)
library(rminer)
library(caret)

PlotFdist(SalePrice)
SalePrice %>% log() %>% PlotFdist()

SalePrice %>% log() %>% plot(train$GrLivArea)
  


methods(varImp)

M <- fit(SalePrice~., data=train, model="svm", kpar=list(sigma=0.10), C=2)
svm.imp <- Importance(M, data=train)
svm.imp$sresponses[[4]]
