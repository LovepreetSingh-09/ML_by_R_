library(class)
library(gmodels)
wdbc=read.csv('Cancer.csv',sep=',',stringsAsFactors=FALSE,header = TRUE)
str(wdbc)

# Remove the id col
wdbc=wdbc[-1]
summary(wdbc)
table(wdbc$diagnosis)

# Change the class into Factor Vector
wdbc$diagnosis<-factor(wdbc$diagnosis,levels=c('B','M'),labels=c('Bengin','Malignant'))
str(wdbc)
table(wdbc[1])
prop.table(table(wdbc[1]))*100

# Use the normalization by creating a function
norm=function(x){ ((x-min(x))/(max(x)-min(x)))}
wdbc_norm=as.data.frame(lapply(wdbc[2:31], norm))
summary(wdbc_norm)
str(wdbc_norm)

# Create Training data
wdbc_train=wdbc_norm[1:469,]
wdbc_train_label=wdbc[1:469,1]
str(wdbc_train)

# Create Testing data
wdbc_test=wdbc_norm[470:569,]
wdbc_test_label=wdbc[470:569,1]
str(wdbc_test_label)
str(wdbc_test)

# Training model :-
# Here, k value is the sq. root of the total no. of training samples
wdbc_test_pred=knn(wdbc_train,wdbc_test,cl=wdbc_train_label,k=21)

# Evaluating model performances :-
CrossTable(x=wdbc_test_label,y=wdbc_test_pred,prop.chisq=FALSE)
# This model gives us the 98% accuracy with only 2 False Negative and 0 false Positive

# Improving Model Performance :-
# Now use the standardization where mean is 0
wdbc_z=as.data.frame(scale(wdbc[2:31]))
summary(wdbc_z)

# Now again create Traing and Testing data 
wdbc_train=wdbc_z[1:469,]
wdbc_test=wdbc_z[470:569,]
str(wdbc_test)
 
# Build model :-
wdbc_test_pred=knn(wdbc_train,wdbc_test,cl=wdbc_train_label,k=21)
CrossTable(x=wdbc_test_label,y=wdbc_test_pred,prop.chisq=FALSE)
# The performance didn't chnage but by changng k the the accuracy is different
wdbc_test_pred=knn(wdbc_train,wdbc_test,cl=wdbc_train_label,k=10)
CrossTable(x=wdbc_test_label,y=wdbc_test_pred,prop.chisq=FALSE)
# Here with k=10 we get 1 False Negative and 1 False Positive which is a very good trade-off


