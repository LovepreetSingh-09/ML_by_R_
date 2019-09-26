library(caTools)
library(ggplot2)
library(gmodels)
getwd()
dataset=read.csv('Salary_Data.csv')
str(dataset)

split=sample.split(dataset$Salary,SplitRatio=0.7)
# returns the split in True/False values
split

train_dataset=subset(dataset,split==TRUE)
train_dataset
test_data=subset(dataset,split==FALSE)

# Build model
model=lm(formula=Salary~YearsExperience,data=train_dataset)
model # shows intercept and weight value
summary(model)
coef(model)

ypred=predict(model,newdata=test_data)

ggplot()+geom_point(aes(x=train_dataset$YearsExperience,y=train_dataset$Salary),color='red')+
    geom_line(aes(x=train_dataset$YearsExperience),y=predict(model,newdata=train_dataset),color='blue')+
    ggtitle('Salary vs Experience')+
    xlab('Years of Experience')+
    ylab('Salary')
ggplot()+geom_point(aes(x=test_data$YearsExperience,y=test_data$Salary),color='red')+
  geom_line(aes(x=train_dataset$YearsExperience,y=predict(model,newdata=train_dataset)),color='blue')+
  ggtitle('Salary as Experience')+
  xlab('Years')+
  ylab('Salary')


