library(neuralnet)
library(MASS)

# Dataset in MASS function
str(Boston)

data=Boston
hist(data$medv,col='sky blue')
head(data)

maxvalue=apply(data,2,max)
maxvalue
minvalue=apply(data,2,min)
minvalue
dataf=as.data.frame(scale(data,center=minvalue,scale=maxvalue-minvalue))
summary(dataf)
# It is sameas normalization
norm=function(x){ ((x-min(x))/(max(x)-min(x)))}
dt=as.data.frame(lapply(data,norm))
summary(dt)

ind=sample(1:nrow(dataf),400)
str(ind)

traindf=dataf[ind,]
str(traindf)
testdf=dataf[-ind,]
str(testdf)

allvars=colnames(data)
allvars
predictors=allvars[!allvars %in% 'medv']
predictors

# joining variable with + sign
predictors=paste(predictors,collapse = '+')
predictors

form=as.formula(paste('medv~',predictors,collapse='+'))
form

neuralmodel=neuralnet(formula=form,hidden=c(4,2),linear.output=T,data=traindf)
plot(neuralmodel)
