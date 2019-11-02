library(neuralnet)
data=read.csv('concrete.txt')
str(data)
# Replacing NA values by 0
data[is.na(data)]=0
summary(data)

# Exclude missing values
data=na.omit(data)
str(data)
summary(data)

# Normalize the data
norm=function(x){
  (x-min(x))/(max(x)-min(x))
}

concrete=as.data.frame(lapply(data,norm))
summary(concrete)

set.seed(12345)
v=as.integer(length(concrete$cement)*0.75)
sp=sample(length(concrete$cement),v)
length(sp)

train=concrete[sp,]
test=concrete[-sp,]
str(test)

# by default, hidden =1 indicates 1 hidden node
model=neuralnet(strength~.,train,hidden=c(2))
model # Shows each weight and intercept/bias
# from plotting we see that SSE is 4.011 while steps(iterations) are 851
plot(model)

# Compute returns a list with two components: $neurons, which stores the neurons for each layer in the network
# and $net.result, which stores the predicted values.
pred=compute(model,test)
pred$neurons
pred$net.result
cor(pred$net.result,test$strength) # 0.821
# We have got a pretty good corelation

# Make neural network with 4 hidden nodes
model=neuralnet(strength~.,train,hidden=4)
model
plot(model) # Now our SSE error is reduced to approx. 1 and steps has been increased

pred=compute(model,test)
cor(pred$net.result,test$strength)

