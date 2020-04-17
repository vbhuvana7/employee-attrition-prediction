#  Course          : CS 513 Knowledge Discovery and Data Mining
#  Purpose         : Group Project

rm(list=ls())

#Loading attrition_data.csv
attrition_data = read.csv(file.choose())
#View(attrition_data)

#Loading Recursive Partitioning functions
library(rpart)
library(rpart.plot)

set.seed(111)
#Excluding few columns
attrition_data <- attrition_data[-c(1,2,3,14)]
summary(attrition_data)

#Converting all columns to factors
for(z in 1:length(attrition_data)){
  attrition_data[,z] <- as.factor(attrition_data[,z])
}

#Checking if all columns are converted to factors
str(attrition_data)

#Dividing data into 50%-50% for training-test
idx<-sample(nrow(attrition_data), as.integer(.50*nrow(attrition_data)))
training<-attrition_data[idx,]
test<-attrition_data[-idx,]

#Recursive partitioning for STATUS against all other fields in training data
CART_class<-rpart(STATUS~.,data=training)
CART_class

#Plotting the created recursive partitioning
rpart.plot(CART_class)

#Predicting test data by classifying as A or T
CART_predict_custom<-predict(CART_class,test, type="class")
#CART_predict_custom<-ifelse(CART_predict[,2]>=.05,4,2)
#Plotting Actual target vs CART(Predicted)
table(Actual=test$STATUS,CART=CART_predict_custom)

#Calculating error rate
CART_wrong<-sum(test$STATUS!=CART_predict_custom)
CART_error_rate<-CART_wrong/length(test$STATUS)
#Printing accuracy in %
(1 - CART_error_rate)*100