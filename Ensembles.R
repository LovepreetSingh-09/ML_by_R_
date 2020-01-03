
library(C50)
library(caret)
library(gmodels)
library(irr)
library(ipred)
library(adabag)

credit=read.csv('credit.txt')
str(credit)

crop=function(x){x=ifelse(x>1,'Yes','No')}
credit$default=factor(sapply((credit$default),crop))
credit=na.omit(credit)

# bagging from ipred
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)


set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
# train of caret do CV of 10 folds by default
train(default ~ ., data = credit, method = "treebag", trControl = ctrl)

# svmBag of caret supplies 3 functions
str(svmBag)
svmBag$fit

# nBag=naive bayes, ctreeBag=decisiontree, nnetBag=nn
bagctrl <- bagControl(fit = svmBag$fit,
                      predict = svmBag$pred,
                      aggregate = svmBag$aggregate)

svmbag <- train(default ~ ., data = credit, "bag",
                trControl = ctrl, bagControl = bagctrl)
svmbag


# AdaBoosting
# from adabag 
m_adaboost=boosting(default~.,credit)
p_adaboost <- predict(m_adaboost, credit)
p_adaboost$confusion

# Boosting using Cross Validation with 10 folds by default
m_adaboost=boosting.cv(default~.,credit)
m_adaboost$confusion

library(vcd)
Kappa(m_adaboost$confusion)


# Random Forests
# from randomForest package
library(randomForest)
rf=randomForest(default~.,credit,ntree=500)
rf

# for validation from caret with 10 folds repeat for 10 times
ctrl=trainControl(method='repeatedcv',number=10,repeats=10)
modelLookup(rf)
# mtry is for no. of random selction of features which is sqrt(16)=4 by default 
grid=expand.grid(.mtry=c(2,4,8,16))

# using train of caret
m_rf=train(default~.,credit,method='rf',tuneGrig=grid,trcontrol=ctrl,metric='Kappa')

# comparing with simple decision tree f C5.0 with 10 folds for 10 repetitions
grid_c50 <- expand.grid(.model = "tree",.trials = c(10, 20, 30, 40),.winnow = "FALSE")
m_c50 <- train(default ~ ., data = credit, method = "C5.0", metric = "Kappa", trControl = ctrl, tuneGrid = grid_c50)

m_rf
m_c50

