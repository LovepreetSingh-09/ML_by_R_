
library(stats)
teens=read.csv('sns.txt')
str(teens)
# There are NA values in gender and age which means the missing values
summary(teens)

# There are almost 2800 NA values
table(teens$gender,useNA='ifany')

# almost 5000 NA in age
summary(teens$age)
# Teens age can not be too small or too large so, limit it to 13-20 years
teens$age=ifelse(teens$age>13 & teens$age<20,teens$age,NA)
summary(teens$age)

# We can use dummy coding on gender
# We won't create variable for M becoz 0 to Female and no_gender means M
# We need to apply !(is.na(teens$gender)) becoz otherwise NA values will appear in female variables also 
teens$female=ifelse(teens$gender=='F' & (!is.na(teens$gender)),1,0)
teens$no_gender=ifelse(is.na(teens$gender),1,0)

table(teens$gender,useNA = 'ifany')
table(teens$female,useNA='ifany')
table(teens$no_gender,useNA='ifany')

# only after extracting NA we can find the mean of a variable
mean(teens$age)
mean(teens$age,na.rm=TRUE)
# Mean age with respect to grad yeR
# aggregate gives the output in a data frame so it is difficult to merge it into our data
aggregate(age~gradyear,teens,mean,na.rm=TRUE)

# We can use ave() to store the information into our dataset
# It gives the vector of the output 
# It has 30000 values which are mean of their respective gradyear
ave_age=ave(teens$age,teens$gradyear,FUN=function(x) mean(x,na.rm=TRUE))
? ave()
ave_age[29995:30000]
summary(ave_age)
teens$age=ifelse(is.na(teens$age),ave_age,teens$age)
summary(teens$age)

# While we have to find the interest so, exclude the person info 1 to 4 columns
interest=teens[5:40]

# Standardize the data before training:
interest_=as.data.frame(lapply(interest,scale))

# Train the model
set.seed(12345)
model=kmeans(interest_,5)
# cluster centers of each variable
model$centers
# Size or no. of elements in the clusters
model$size
# Cluster label of each data point of the training dataset
model$cluster

# Add cluster label column in the original data set
teens$cluster=model$cluster
# Info of a person with its assigned cluster label
(teens[1:10,c('gradyear','age','gender','friends','cluster')])
teens$cluster=factor(teens$cluster)

# Average age based on each cluster
aggregate(age~cluster,teens,mean)

# Female proportion in each cluster
aggregate(female~cluster,teens,mean)

# Average no. of friends based on each cluster
aggregate(friends~cluster,teens,mean)
levels(teens$cluster) <- c('one',"two", "three",'four','five')
levels(teens$cluster)
teens$cluster
