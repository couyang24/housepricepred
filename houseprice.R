# Load packages
library(tidyverse)
library(randomForest)
library(ggthemes)
library(corrplot)
library(caret)
library(gridExtra)

# Load datasets
train<-read_csv("data/train.csv")
test<-read_csv("data/test.csv")
