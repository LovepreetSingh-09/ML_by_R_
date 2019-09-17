# Vectors :-
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)
temperature[1:2]
temperature[-2] # deletes the 2nd argument
temperature[-1] # deletes the 1st argument
temperature[-(length(temperature)-1)]
temperature[c(flu_status)]

# Factors :-  Used for Categorical Variables
gender=factor(c('Male','Female','Male'))
gender
blood <- factor(c("O", "AB", "A"),levels = c("A", "B", "AB", "O"),ordered = TRUE)
blood<'AB'

# Lists :-
b=list(full_name=subject_name[1],temperature=temperature[1],flu=flu_status[1],gender=gender[1],blood=blood[1])
b
b[2]
b$temperature
b[[2]]
b[c('temperature','flu')]

# DataFrames:
data=data.frame(subject_name,temperature,flu_status,gender,blood)
data
data$temperature
data[1,3]
data[c(1,3),c(2,4)] # 1st and 3rd row of 2nd and 4th col
data[c('temperature','flu_status')]
data[1,]
data[c(1,3),c('temperature','blood')]
data[-2,c(-1,-3,-5)] # Erase 2nd row of all col and erase 1,3,5 col

# Matrix and Arrays :-
# R uses Column Major-Order
m=matrix(c(1,2,4,6,7,9),nrow=2)
m
n=matrix(c(2:4,6,7,5,1,5),ncol=2)
n

# Manupulating Data :-
save(m,n,data,file='mydata.RData')
h=load('mydata.Rdata')
h
ls()[1:5] # List of all data structures
rm('h','m') # Removes h and m 
ls()
rm(list=ls()) # Removes all data
ls()
usedcars<-read.csv('Usedcars.txt')
str(usedcars)
usedcars$price[1:9]
usedcars$color
summary(usedcars)
summary(usedcars[c('price','color')])

mean(c(15000,20000,27000))
range(usedcars$price)
diff(range(usedcars$price))
IQR(usedcars$price) # Interquartile Range = diff b/w Q3 and Q1
quantile((usedcars$price))
quantile(usedcars$price,probs=c(0.01,0.99)) # 1st percentile and 99th percentile
quantile(usedcars$price,seq(from=0,to=1,by=0.20))
# Maximum or Minimun value denoted by horizontal line in the plot is 1.5 times the IQR
# Any value over or below that will be displayed as dots or circles and reffered as outliers.
boxplot(usedcars$price,main='Box-Plot of Price',ylab='Price ($)')
boxplot(usedcars$mileage,main='Box-Plot of Mileage',ylab='Mileage (Km)')
