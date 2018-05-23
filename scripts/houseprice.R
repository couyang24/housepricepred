# Load packages
library(tidyverse)
library(randomForest)
library(ggthemes)
library(corrplot)
library(caret)
library(gridExtra)

# Load datasets
train<-read_csv("train.csv")
test<-read_csv("test.csv")
full <- bind_rows(train,test)

# Check data
str(full)
summary(full)
sapply(full, function(x) sum(is.na(x)))
head(full)
dim(full)

non_char <- full[which(sapply(full,class)!="character")]
char <- full[which(sapply(full,class)=="character")]


temp <- sapply(full, function(x) sum(is.na(x))) %>% as.data.frame() %>% rownames_to_column()

colnames(temp) <- c('var','num_miss')

temp %>% arrange(desc(num_miss))

temp %>% filter(num_miss!=0 & var!='SalePrice')  %>% ggplot(aes(var,num_miss)) + geom_col() + 
  coord_flip() + 
  labs(x='variables', y='number of missing')








# Toolbox:

# Visualize Discrete Single Variables
vis_bar<-function(dataset,variable){
  
  dataset$variable<-dataset[[variable]]
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_bar(position = "stack")+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_bar(position = "fill")+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_bar(position = "dodge")+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable," vs Survived \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

# Visualize Continuous Single Variables
vis_hist<-function(dataset,variable,binwidths=5){
  
  dataset$variable<-dataset[[variable]]
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_histogram(position = "stack",binwidth = binwidths)+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_histogram(position = "fill",binwidth = binwidths)+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable,fill=as.factor(Survived)))+geom_histogram(position = "dodge",binwidth = binwidths)+
      theme_economist()+labs(fill="Survived",x=variable))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable," vs Survived \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

# Visualize Discrete Multiple Variables
vis_bar_multi<-function(dataset,variable1,variable2){
  
  dataset$variable1<-dataset[[variable1]]
  dataset$variable2<-dataset[[variable2]]
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_bar(position = "stack")+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_bar(position = "fill")+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_bar(position = "dodge")+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable1," vs ",variable2," vs Survived \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}

# Visualize Continuous Single Variables
vis_hist_multi<-function(dataset,variable1,variable2,binwidths=5){
  
  dataset$variable1<-dataset[[variable1]]
  dataset$variable2<-dataset[[variable2]]
  
  (g1<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_histogram(position = "stack",binwidth = binwidths)+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (g2<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_histogram(position = "fill",binwidth = binwidths)+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (g3<-dataset %>% filter(!is.na(Survived)) %>% 
      ggplot(aes(variable1,fill=as.factor(Survived)))+geom_histogram(position = "dodge",binwidth = binwidths)+
      theme_economist()+facet_grid(.~variable2)+labs(fill="Survived",x=variable1))
  
  (Title<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
      labs(x="",y="")+
      annotate('text', x = 0, y = 0, label = paste0(variable1," vs ",variable2," vs Survived \n multi_views"),size=5)+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),plot.margin = unit(c(0,0,-1,-1),"cm")))
  
  grid.arrange(Title,g1,g2,g3,layout_matrix=t(matrix(c(1,2,
                                                       3,4),nrow=2)))
}



