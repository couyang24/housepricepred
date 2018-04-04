require(knitr)              ## dynamic report generation in R
require(DT)                 ## display data in html tables
library(tidyverse)            ## plotting 
require(gridExtra)          ## arrange visualizations using grid 
require(data.table)         ## fast data wrangling and analysis
require(funModeling)        ## table with counts on missing values. Do not load before psych or caret. It will mask some stuff.
require(psych)              ## descriptive statistics, skewness and kurtosis
require(caret)              ## (near) zero variance, train and predict
require(caretEnsemble)      ## ensemble modelling
require(xgboost)
require(glmnet)
require(LiblineaR)          ## svm
require(vtreat)             ## one hot encode and ignore factor levels with low frequency
knitr::opts_chunk$set(error=FALSE, message=FALSE, warning=FALSE) ## some defaults for this report

train.dt <- fread(input = "train.csv", 
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
test.dt <- fread(input = "test.csv", 
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

variablesCounts <- c(
  "BsmtFullBath",		## Basement full bathrooms
  "BsmtHalfBath",		## Basement half bathrooms
  "FullBath",			  ## Full bathrooms above grade
  "HalfBath",			  ## Half baths above grade
  "BedroomAbvGr",		## Bedrooms above grade (does NOT include basement bedrooms)
  "KitchenAbvGr",		## Kitchens above grade
  "TotRmsAbvGrd",		## Total rooms above grade (does not include bathrooms)
  "Fireplaces",		  ## Number of fireplaces
  "GarageCars"     	## Size of garage in car capacity
)

variablesValues <- c(
  "MiscVal",        ## $ Value of miscellaneous feature
  "SalePrice"       ## $ Price paid
)




variablesFactor <- colnames(full.dt)[which(as.vector(full.dt[,sapply(full.dt, class)]) == "character")]
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

full.dt[Exterior2nd  == "Brk Cmn"]

summary(full.dt)

col <- colnames(full.dt)[which(colnames(full.dt) %>% str_detect("Yr|Year"))]




full.dt %>% select(col) %>% lapply(summary)

full.dt %>% group_by(MSSubClass) %>% summarise(count=n)

full.dt[MSSubClass  == 150]

