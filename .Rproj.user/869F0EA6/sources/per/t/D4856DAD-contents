library(C50)
library(gmodels)
credit=read.csv('credit.txt')
str(credit)

# Entropy curve
# Default values of from and to are also 0 and 1
curve(-x * log2(x) - (1 - x) * log2(1 - x),from=0,to=1,col = "red", xlab = "x", ylab = "Entropy",lwd=5)

# Generating Random Numbers
set.seed(1)
sd=sample(1000,900)

crop=function(x){x=ifelse(x>1,'Yes','No')}

# spliting data for training and testing
credit_train=credit[sd,-17]
credit_train_labels=factor(sapply((credit$default[sd]),crop))
str(credit_train)
str(credit_train_labels)
table(credit_train_labels)
credit_test=credit[-sd,-17]
str(credit_test)
credit_test_labels=factor(sapply(credit[-sd,17],crop))
table(credit_test_labels)


# trails are the number of trees to be made for boosting. 1 by default.
# Costs is a matrix associated with the different types of errors. NULL by default.
model=C5.0(credit_train,credit_train_labels,trials=1,costs=NULL)
# model gives the depth or size, samples and features used
# Summary shows the rules by which tree is made of and the error rate.
model
summary(model)

# Evaluating the model
pred=predict(model,credit_test)
CrossTable(credit_test_labels,pred,prop.chissq=F,prop.t=FALSE,dnn=c('Actual','Prediction'))
# We got 33 misclassified out of 100
# The one thing to notice here is that we have more False 
# This might be the case of overfitting 

# Improving Model Performance :-
# Firstly increase the no. of trials
model=C5.0(credit_train,credit_train_labels,trials=4)
model
summary(model)
pred=predict(model,credit_test)
CrossTable(credit_test_labels,pred,prop.chissq=F,prop.t=FALSE,dnn=c('Actual','Prediction'))
# At trials=4 give us the maximum accuracy with 75%.
# But still there are many False Negative and False Positive

# Using Cost Function
# Costs is used to reduce the no. of False Negative or Positive at the expense of the other
matrix=list(c('No','Yes'),c('No','Yes'))
names(matrix)=c('actual','predicted')
matrix
costf=matrix(c(0,4,1,0),nrow=2,dimnames=matrix)
costf
model=C5.0(credit_train,credit_train_labels,trials=12,costs=costf)
model
summary(model)
pred=predict(model,credit_test)
# Accuracy has been got better slightly but the False negative and False Positive have been changed.
CrossTable(credit_test_labels,pred,prop.chissq=F,prop.t=FALSE,dnn=c('Actual','Prediction'))

