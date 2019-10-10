library(rpart)
library(rpart.plot)
library(RWeka)
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)
# The formula used for regression tree is SDR 
sdr1=sd(tee)-(length(at1)/length(tee)*sd(at1)+length(at2)/length(tee)*sd(at2))
sdr1
sdr2=sd(tee)-(length(bt1)/length(tee)*sd(bt1)+length(bt2)/length(tee)*sd(bt2))
sdr2
# sd has more sdr value so regression tree will use sdr2 first
data=read.csv('white_wines.txt')
str(data)
summary(data)
hist(data$quality,col='sky blue')

w_train=data[1:3750,]
w_test=data[3750:length(data$quality),]

model=rpart(quality~.,w_train)
model

# The nodes having * can be used for predictions
summary(model)

# Visualizing Tree
rpart.plot(model,digits=4,type=3,fallen.leaves=TRUE,extra = 101)

pred=predict(model,w_test)
summary(pred)
summary(w_test$quality)
# From the comparison we can see the original data quality has a wide range while our pred has quite lesser

cor(pred,w_test$quality)
# the correlation only measures how strongly the predictions are related to the true value.
# it is not a measure of how far off the predictions were from the true values.

# So we need to check mean absolute error
MAE=function(act,pred){
   mean(abs(act-pred))
}
MAE(w_test$quality,pred)

# Using Regression models at regression trees
model=M5P(quality~.,data=w_train)
summary(model)
model
# There are different weights and intercepts for features at the different nodes

pred=predict(model,w_test)
summary(pred)
