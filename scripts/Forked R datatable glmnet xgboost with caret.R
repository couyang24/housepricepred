require(knitr)              ## dynamic report generation in R
require(DT)                 ## display data in html tables
require(gridExtra)          ## arrange visualizations using grid 
require(dplyr)              ## easy data wrangling on small data frames
require(data.table)         ## fast data wrangling and analysis
require(funModeling)        ## table with counts on missing values. Do not load before psych or caret. It will mask some stuff.
require(psych)              ## descriptive statistics, skewness and kurtosis
require(caret)              ## (near) zero variance, train and predict
require(caretEnsemble)      ## ensemble modelling
require(xgboost)
require(glmnet)
require(LiblineaR)          ## svm
require(vtreat)             ## one hot encode and ignore factor levels with low frequency
require(rebus)
library(tidyverse)
require(skimr)

train.dt <- fread(input = "input/train.csv", 
                  sep = ",", 
                  nrows = -1,
                  header = T,
                  na.strings=c("NA","N/A","null"),
                  stringsAsFactors = F,
                  check.names = T,
                  strip.white = T,
                  blank.lines.skip = T,
                  data.table = T
) 
test.dt <- fread(input = "input/test.csv", 
                 sep = ",", 
                 nrows = -1,
                 header = T,
                 na.strings=c("NA","N/A","null"),
                 stringsAsFactors = F,
                 check.names = T,
                 strip.white = T,
                 blank.lines.skip = T,
                 data.table = T
) 

## Create one data set for feature engineering. 
train.dt[, dataPartition:="train"]
test.dt[, SalePrice:=as.integer(NA)] 
test.dt[, dataPartition:="test"]
full.dt <- rbindlist(list(train.dt, test.dt), use.names = F, fill = F)

full.dt %>% select(Exterior2nd) %>% table()


full.dt %>% skim() %>% 
  datatable(filter = 'top', options = list(
    pageLength = 15, autoWidth = F
  ))

varSF <- colnames(train.dt)[str_detect(colnames(train.dt), 'Area|tage|SF|Porch')]

variablesSquareFootage <- c(
  "LotFrontage", 		## Linear feet of street connected to property 
  "LotArea",    		## Lot size in square feet
  "MasVnrArea",  		## Masonry veneer area in square feet
  "BsmtFinSF1",		  ## Type 1 finished square feet	
  "BsmtFinSF2",		  ## Type 2 finished square feet
  "BsmtUnfSF",		  ## Unfinished square feet of basement area
  "TotalBsmtSF", 		## Total square feet of basement area
  "FirstFlrSF",		  ## First Floor square feet
  "SecondFlrSF",	  ## Second floor square feet
  "LowQualFinSF", 	## Low quality finished square feet (all floors)
  "GrLivArea", 		  ## Above grade (ground) living area square feet
  "GarageArea",     ## Size of garage in square feet
  "WoodDeckSF",     ## Wood deck area in square feet
  "OpenPorchSF",    ## Open porch area in square feet  
  "EnclosedPorch",  ## Enclosed porch area in square feet 
  "ThreeSsnPorch",  ## Three season porch area in square feet 
  "ScreenPorch",    ## Screen porch area in square feet
  "PoolArea" 		    ## Pool area in square feet
)

varCount <- colnames(train.dt)[str_detect(colnames(train.dt),"Bath|AbvGr|places|Cars")]

variablesCounts <- c(
  "BsmtFullBath",       ## Basement full bathrooms
  "BsmtHalfBath",       ## Basement half bathrooms
  "FullBath",             ## Full bathrooms above grade
  "HalfBath",             ## Half baths above grade
  "BedroomAbvGr",       ## Bedrooms above grade (does NOT include basement bedrooms)
  "KitchenAbvGr",       ## Kitchens above grade
  "TotRmsAbvGrd",       ## Total rooms above grade (does not include bathrooms)
  "Fireplaces",       ## Number of fireplaces
  "GarageCars"      ## Size of garage in car capacity
)

variablesValues <- c(
  "MiscVal",        ## $ Value of miscellaneous feature
  "SalePrice"       ## $ Price paid
)

variablesFactor <- colnames(full.dt)[sapply(full.dt, class) == "character"]
variablesFactor <- setdiff(variablesFactor, "dataPartition") 
variablesFactor <- c(variablesFactor,
                     ## variables with data type integer which are factors
                     "MSSubClass",     ## Identifies the type of dwelling involved in the sale
                     "OverallQual",    ## Rates the overall material and finish of the house
                     "OverallCond"     ## Rates the overall condition of the house
)
setnames(full.dt, c("X1stFlrSF","X2ndFlrSF","X3SsnPorch"), c("FirstFlrSF","SecondFlrSF","ThreeSsnPorch"))






full.dt[YearRemodAdd > YrSold, YearRemodAdd:= YrSold] ## Fix typo
full.dt[GarageYrBlt == 2207, GarageYrBlt:= 2007] ## Fix typo
full.dt[MSSubClass  == 150, MSSubClass:= 160] ## 150 not in training set
full.dt[Exterior1st  == "Wd Sdng", Exterior1st:= "WdSdng"] ## Fix spaces
full.dt[Exterior2nd  == "Wd Sdng", Exterior2nd:= "WdSdng"] ## Fix spaces
full.dt[Exterior2nd  == "Brk Cmn", Exterior2nd:= "BrkComm"] ## Fix typo
full.dt[Exterior2nd  == "Wd Shng", Exterior2nd:= "WdShing"] ## Fix typo
full.dt[RoofMatl  == "Tar&Grv", RoofMatl:= "TarGrv"] ## Fix '&'
full.dt[RoofMatl  == "WdShngl", RoofMatl:= "WdShing"] ## See exterior





changeColType <- variablesFactor
full.dt[,(changeColType):= lapply(.SD, as.factor), .SDcols = changeColType]
## Set columns to numeric
changeColType <- c(variablesSquareFootage, variablesCounts, variablesValues)
full.dt[,(changeColType):= lapply(.SD, as.numeric), .SDcols = changeColType]



## OverallQual, rates the overall material and finish of the house
full.dt[,OverallQual:=ordered(OverallQual, levels = c(1:10))]
## OverallCond, rates the overall condition of the house
full.dt[,OverallCond:=ordered(OverallCond, levels = c(1:10))]
## KitchenQual, kitchen quality
full.dt[,KitchenQual:=ordered(KitchenQual, levels = c("Po","Fa","TA","Gd","Ex"))]
## GarageFinish (contains NA's)
full.dt[,GarageFinish:=ordered(GarageFinish, levels = c("None","Unf","RFn","Fin"))]
## GarageQual
full.dt[,GarageQual:=ordered(GarageQual, levels = c("None","Po","Fa","TA","Gd","Ex"))]
## GarageCond
full.dt[,GarageCond:=ordered(GarageCond, levels = c("None","Po","Fa","TA","Gd","Ex"))]
## ExterQual, evaluates the quality of the material on the exterior  
full.dt[,ExterQual:=ordered(ExterQual, levels = c("Po","Fa","TA","Gd","Ex"))]
## ExterCond, evaluates the present condition of the material on the exterior
full.dt[,ExterCond:=ordered(ExterCond, levels = c("Po","Fa","TA","Gd","Ex"))]
## BsmtQual (contains NA's), evaluates the height of the basement
full.dt[,BsmtQual:=ordered(BsmtQual, levels = c("None","Po","Fa","TA","Gd","Ex"))]
## BsmtCond (contains NA's), evaluates the general condition of the basement
full.dt[,BsmtCond:=ordered(BsmtCond, levels = c("None","Po","Fa","TA","Gd","Ex"))]
## BsmtExposure (contains NA's), refers to walkout or garden level walls
full.dt[,BsmtExposure:=ordered(BsmtExposure, levels = c("None","No","Mn","Av","Gd"))]
## BsmtFinType1 (contains NA's), rating of basement finished area
full.dt[,BsmtFinType1:=ordered(BsmtFinType1, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))]
## FireplaceQu (contains NA's), fireplace quality
full.dt[,FireplaceQu:=ordered(FireplaceQu, levels = c("None","Po","Fa","TA","Gd","Ex"))]
## Electrical
full.dt[,Electrical:=ordered(Electrical, levels = c("FuseP","Mix","FuseF","FuseA","SBrkr"))]
## Fence
full.dt[,Fence:=ordered(Fence, levels = c("None","MnWw","MnPrv","GdWo","GdPrv"))]
## PoolQC
full.dt[,PoolQC:=ordered(PoolQC, levels = c("None","Fa","Gd","Ex"))]


descStats <- describe(full.dt[, c(variablesSquareFootage,variablesValues), with = FALSE]) 
datatable(descStats, rownames = T,
          caption = "Descriptive statistics", 
          options = list(pageLength = 8)) ## Interactive HTML table

zeroVarianceVariables.df <- nearZeroVar(train.dt, names = T, saveMetrics = T,
                                        foreach = T, allowParallel = T)

datatable(round(subset(zeroVarianceVariables.df, nzv == TRUE, 
                       select =     c("freqRatio","percentUnique")),2), 
          rownames = T,
          caption = "Variables with (near) zero variance", 
          options = list(pageLength = 8))


descMissing <- df_status(full.dt, print_results = FALSE) 
pZeros <- ggplot(filter(descMissing,q_zeros > 0), aes(reorder(variable, p_zeros),p_zeros)) +
  geom_bar(stat = "identity") +
  ggtitle("Numerical variables with zero's") + 
  xlab("") + 
  ylab("") +
  theme(text = element_text(size=9)) +   
  coord_flip()
pNa <-    ggplot(filter(descMissing,q_na > 0), aes(reorder(variable, p_na),p_na)) +
  geom_bar(stat = "identity") +
  ggtitle("Categorical variables empty") + 
  xlab("") + 
  ylab("") +
  theme(text = element_text(size=9)) +   
  coord_flip()
#datatable(descMissing[(descMissing$q_zeros > 0 & descMissing$type == "numeric") |
#                        (descMissing$q_na > 0 & descMissing$type != "numeric"),], rownames = T,
#          caption = "Missing values or zero's", 
#          options = list(pageLength = 8)) ## Interactive HTML table
pZeros
pNa
