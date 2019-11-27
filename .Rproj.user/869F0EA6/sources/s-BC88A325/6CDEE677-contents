library(gmodels)
library(caret)
library(vcd)
library(irr)
library(ROCR)
# stringsAsFactors=FALSE let the strings data columns as character type
# While stringsAsFactors=TRUE makes the strings data columns as Factor type
# Default value of stringsSFactors=TRUE
usedcars<-read.csv('Usedcars.txt') 
str(usedcars)
usedcars$price[1:9]
usedcars$color
summary(usedcars)
summary(usedcars[c('price','color')])
usedcars[1:2][4,]
usedcars[1,][,c(3,5)]

x=function(x){5*x}
# lapply() applies only a function to some given data and returns a list
b=lapply(usedcars[1,],x)
# To convert the list into data frame use as.data.frame
as.data.frame(b)

mean(c(15000,20000,27000))
range(usedcars$price)
diff(range(usedcars$price))
IQR(usedcars$price) # Interquartile Range = diff b/w Q1 and Q1
quantile((usedcars$price))
quantile(usedcars$price,probs=c(0.01,0.99)) # 1st percentile and 99th percentile
quantile(usedcars$price,seq(from=0,to=1,by=0.20))
# Maximum or Minimun value denoted by horizontal line in the plot is 1.5 times the IQR
# Any value over or below that will be displayed as dots or circles and reffered as outliers.
boxplot(usedcars$price,main='Box-Plot of Price',ylab='Price ($)')
boxplot(usedcars$mileage,main='Box-Plot of Mileage',ylab='Mileage (Km)')

hist(usedcars$price,main='Histogram of Prices',ylab='Prices ($)',breaks=15,col='green')
hist(usedcars$mileage,main='Histogram of Mileage',ylab='Mileage (Km)',breaks=30,col='sky blue')

var(usedcars$price)
var(usedcars$mileage)
sd(usedcars$price)
sd(usedcars$mileage)
# In normal distribution 68.2% (34.1% on both sides) of values lies within the range of mean-+(1*sd)
# Next 27.2% (13.6% on both sides) after the previous range mean-+(from 1sd to 2sd)
# Next are 4,2% (2% on each side) and 0.2%(0.1% each side) for 2sd to 3sd and 3sd to 4sd
mean(usedcars$price)
# Normal Distribution says that the 68% of cars prices are lying b/w $9,840 to $16,804

# Categorical variable like Year, model etc are examined using table rather than summary
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)
# since table give us the count of each categorical variable
# We can also find proportion or percentage of each variable in the data by using prop.table
model_table=table(usedcars$model)
prop.table(model_table)
# Always use prop.table() on the table of categorical variable otherwise proportion of every single data point will be calculated
color_table<-table(usedcars$color)
color_table<-prop.table(color_table)*100
round(color_table,digits=2)

# mode() in R doesn't give mode but type of data i.e. 'Numeric','list',etc.
mode(color_table) 

# Scatter-Plot :
plot(x=usedcars$mileage,y=usedcars$price,xlab='Mileage (Km)',ylab='Price ($)',col=c('sky blue'))

conservative<-usedcars$color %in% c('Black','Silver','White','Gray')
conservative
table(conservative) # Gives values of True and False and their count

# checking the relation between the model and the color
CrossTable(x=usedcars$model,y=conservative)

# kappa statistic :-
# It adjusts accuracy by accounting for the possibility of a correct prediction by chance alone.
# It is (pr(a)-pr(e))/1-pr(e)
pr_a=0.865+0.109 # True Positive and True Negative proportions
pr_e=0.869*0.888 + 0.132*0.112 # (TP+FP)*(TP+FN) + (TN+FN)*(TN*FP)
kap=(pr_a-pr_e)/(1-pr_e)
kap # >0.8 means very good agreement

# Here we need to pass the actual and predicted vectors
# Kappa funvtion in vcd package
help(Kappa)

# Kappa2 in irr package
help(kappa2)

# The bottom functions are in caret package
# Sensitivity is TPR or Recall
help("sensitivity")
# Specificity is TNR
help("specificity")
# Precision is Positive Predicted value (PosPredValue)
help("posPredValue")

# The following functions are used to create ROC curve
help("prediction")
help('performance')
# For marking a line on the already plotted graph
abline(a=0,b=0.11,lwd=2)
