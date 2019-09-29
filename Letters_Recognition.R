library(kernlab)
letters <- read.csv("letters_data.txt")
str(letters)

train=letters[1:16000,]
test=letters[16001:20000,]

model=ksvm(letter~.,train,kernel='vanilladot',C=1)
model
# error is just 0.13

pred=predict(model,test)
head(test$letter)

# For checking confusion matrix
v=pred==test$letter
table=table(v)
table
prop.table(table)
# Here we can see that we have 83.9 % accuracy

# improving model by rbf kernel
model=ksvm(letter~.,train,kernel='rbfdot',C=1)
pred=predict(model,test)
arg=pred==test$letter
prop.table(table(arg))
# Now we have eaily got 93 % accuracy
