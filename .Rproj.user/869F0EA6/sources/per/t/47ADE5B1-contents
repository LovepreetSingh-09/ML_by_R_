# Rule Learner Algorithms
library(RWeka)
mushroom=read.csv('mushrooms.csv')
str(mushroom)

# As veil.type has only one value 
mushroom$veil.type<-NULL
table(mushroom$class)
# Here we are gonna use all the samples for training

# Training a 1R model from RWeka
# Here class represent the target variable while tilde refers to the features to use.
# Here . after ~ means all the features ut if we want to specify some certtain features, we need to put + b/w those
# like... OneR(class~odor+bruises,mushroom)
# Mushroom is a dataframe
model=OneR(class~.,mushroom)

# It tells us the rule and feature it used
model

# gives the confusion matrix and full details methamatically
summary(model)

# Here our accuracy is almost 99% with no False Positive(poisonous) means no edible has classified into poisonous
# but 120 samples are classifed as False Negative(edible) 
# So we have classified 120 poisonous samples into edible category.
# This is a huge mistake and can be very dangerous in real world

# Improving Performance :-
# Use JRip() another rule algorithm from RWeka
model=JRip(class~.,mushroom)
model

# It has more rules than the previous one
summary(model)
# Now we got 100% efficiency on training data with 0 FN and 0 FP
# This is because this model is more complex and used more rules
# Perhaps this accuracy is on the training data only. We don't know how will it perform on the unseen data
