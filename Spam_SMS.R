library(tm)
library(SnowballC)
library(wordcloud)
library(e1071)
library(gmodels)

raw=read.csv('spam.csv',sep=',')
fix(raw)
length(raw$X.1)
str(raw)
prop.table(table(raw$v1))

# VCorpus refers to a volatile corpus—volatile as it is stored in memory as opposed to being stored on disk
# we use the VectorSource() reader function to create a source object from the existing raw$v2 vector.
corpus=VCorpus(VectorSource(raw$v2))
print(corpus[1:2])

# for displaying text use as.character
as.character(corpus[[1]])
# For applying it to all use lapply
lapply(corpus[1:5],as.character)

# we use tm wrapper function content_transformer() to treat tolower() as a transformation function that can be used to access the corpus.
clean_corpus=tm_map(corpus,content_transformer(tolower))
as.character(clean_corpus[[1]])
clean_corpus=tm_map(clean_corpus,removeNumbers)

# remove words is a function of tm while stopwords is the collection of most commonly used words
clean_corpus=tm_map(clean_corpus,removeWords,stopwords())
as.character(clean_corpus[[1]])
clean_corpus=tm_map(clean_corpus,removePunctuation)
wordStem(c("learn", "learned", "learning", "learns"))
clean_corpus=tm_map(clean_corpus,stemDocument)
as.character(clean_corpus[[1]])
clean_corpus=tm_map(clean_corpus,stripWhitespace)
as.character(clean_corpus[[1]])

# Tokenization :
# Create DocumentTermMatrix where everyrow represent a document and column represent a word.
# There is also TDM which is opposite to it.
sms_dtm=DocumentTermMatrix(clean_corpus)
str(sms_dtm)

# If we didn't have done the preprocessing then we should prepare data by the following method
sms_dtm2 <- DocumentTermMatrix(corpus, control = list(tolower = TRUE,removeNumbers = TRUE,stopwords = TRUE,removePunctuation = TRUE,stemming = TRUE))
str(sms_dtm2)
sms_dtm[1,]
sms_dtm2[1,]

# Creating Training and Testing Data
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_train_labels <- raw[1:4169, ]$v1
sms_test_labels <- raw[4170:5559, ]$v1
prop.table(table(sms_test_labels)) 

# Creating WordCloud
wordcloud(clean_corpus,min.freq=50,random.order=FALSE)
spam=subset(raw,v1=='spam')
spam

# Wordcloud of sam and ham 
wordcloud(spam$v2,max.words=40,scale=c(3,0.5))
ham=subset(raw,v1=='ham')
wordcloud(ham$v2,max.words=40,scale=c(3,0.5))

# Create Dataset of frequent terms
# findFreqTerms takes a dtm  and return a vector of all specified frequent terms
sd=findFreqTerms(sms_dtm,5)
sd[1:10]
length(sd) # 1410
sms_train=sms_dtm_train[,sd]
sms_test=sms_dtm_test[,sd]
sms_train[1,]

# Naive Bayes typically trains on data with categorical values
corp=function(x){ x=ifelse(x>0,'yes','no')}

# apply() can be used on either rows or columns 
# MARGIN=1 means row-wise and 2 means column-wise
sms_train=apply(sms_train,MARGIN=2,corp)
sms_test=apply(sms_test,MARGIN = 2,corp)

# Building Model :-
# Default Laplace is also 0
nb=naiveBayes(sms_train,sms_train_labels,laplace=0)

# Evaluating Model:-
pred=predict(nb,sms_test)
pred
length(pred)
length(sms_test_labels)
typeof(pred)
typeof(sms_test_labels)
# Here we have only 31 out of 1390 misclassified so we have the accuracy of about 97.7%
CrossTable(pred,sms_test_labels,prop.chisq=FALSE,dnn=c('predicted','actual'),prop.t = FALSE)

# Using Laplace :
nb1=naiveBayes(sms_train,sms_train_labels,laplace=1)
pred1=predict(nb1,sms_test)
CrossTable(pred1,sms_test_labels,prop.chisq=FALSE,dnn=c('predicted','actual'),prop.t = FALSE)
