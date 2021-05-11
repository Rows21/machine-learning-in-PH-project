setwd("C:/Users/jiachunguang/Desktop/400")
data=read.csv("data(3).csv")
head(data)
table(data$stroke)
library(ROSE)
data_balanced_under <- ovun.sample(stroke ~ ., 
                                   data = data[,-1], 
                                   method = "under", 
                                   N = 498, seed = 1)$data
table(data_balanced_under$stroke)
data_balanced_under$stroke=as.factor(data_balanced_under$stroke)
set.seed(0)
ind=sample(2,nrow(data_balanced_under),replace = TRUE,prob=c(0.8,0.2))
train=data_balanced_under[ind==1,]
test=data_balanced_under[ind==2,]
library(randomForest)
model=randomForest(stroke ~ ., data = train)
pre=predict(model,newdata=test,type="class")
library(caret)
confusionMatrix(pre,test$stroke)


kresult=kmeans(data_balanced_under[,-10],2)
kresult$cluster

library(ggplot2)
data_balanced_under=cbind(data_balanced_under,kresult$cluster)
data_balanced_under[,11]=as.factor(data_balanced_under[,11])
ggplot(data_balanced_under,aes(x = age,y = avg_glucose_level,color = data_balanced_under[,11]))+ geom_point()



