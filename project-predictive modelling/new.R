library(car)
library(caret)
library(class)
library(devtools)
library(e1071)
install.packages("ggord")
library(ggord)
library(ggplot2)
library(Hmisc)
library(klaR)
library(klaR)
library(MASS)
library(nnet)
library(plyr)
library(pROC)
library(psych)
library(scatterplot3d)
library(SDMTools)
install.packages("dplyr")
library(dplyr)
library(ElemStatLearn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(data.table)
library(ROCR)
library(corrplot)
library(tidyverse)
library(VIF)
library(car)
library(caret)
install.packages(c("SDMTools","pROC", "Hmisc"))
library(SDMTools)
library(pROC)
library(Hmisc)
library(devtools)
install.packages("ggplot2")
library(ggplot2)
install.packages("caret")
library(caret)

setwd("C:/Users/spandey/Desktop")
getwd()
phone = read.csv("Cellphone.csv", header = TRUE)
attach(phone)
head(phone)
str(phone)



# univariate analysis
dim(phone)
sd(phone)
var(phone)
str(phone)
summary(phone)
attach(phone)



#histogram for numerical variables
qplot(DataUsage, data= phone)
qplot(AccountWeeks, data= phone)
qplot(CustServCalls, data= phone)
qplot(DayMins, data= phone)
qplot(DayCalls, data= phone)
qplot(MonthlyCharge, data= phone)
qplot(OverageFee, data= phone)
qplot(RoamMins, data= phone)

#bar plot for categorical variables
qplot(ContractRenewal, data= phone, geom = "bar")
qplot(Churn, data= phone, geom = "bar")
qplot(DataPlan, data= phone, geom = "bar")





## bivariate analysis (of only numerical variables)

# qplot()
library(corrplot)
qplot(DataUsage,  DayMins, colour = Churn, data=phone)
qplot(DataUsage,  CustServCalls, colour = Churn, data=phone)
qplot(DataUsage,  MonthlyCharge, colour = Churn, data=phone)
qplot(DataUsage,  AccountWeeks, colour = Churn, data=phone)
qplot(DataUsage,  RoamMins, colour = Churn, data=phone)
qplot(CustServCalls,  RoamMins, colour = Churn, data=phone)
qplot(DayMins,  CustServCalls, colour = Churn, data=phone)
qplot(DayMins,  MonthlyCharge, colour = Churn, data=phone)
qplot(DayMins,  AccountWeeks, colour = Churn, data=phone)
qplot(DayMins,  RoamMins, colour = Churn, data=phone)

#boxplot
boxplot(phone)


#correlation plot
Scatter<- phone[,-c(1,3,4)]

cor(Scatter)

correlations<- cor(Scatter)

corrplot(correlations, method="circle")



#treat missing values and negative values in data set

colSums(is.na(phone))
outlier = (phone)$out
outlier

boxplot(phone)

neg = phone<0
sum(neg)




#create dummy variables
phone$Contractcancel<-ifelse(phone$Churn=="cancelContract",0,1)
head(phone)
#str(phone)

# Split dataset into train and test


set.seed(1)
rows <- sample(2, size = nrow(phone),replace=TRUE, prob=c(0.7,0.3))
train <- phone[rows==1,]
test <- phone[rows==2,]
dim(phone)
dim(train)
dim(test)

#Data Frame for Linear Regression
head(phone)
train.reg<-train
head(train)
test.reg<-test
head(test)

#check multicollinearity using vif factor (take full datset)
library(car)
str(phone)
head(phone)
linear1= Churn ~ AccountWeeks+ContractRenewal +DataPlan+ContractRenewal+CustServCalls+DayCalls +DataUsage+DayMins+MonthlyCharge+OverageFee+RoamMins
full = lm(linear1, data = train.reg)
summary(full)
vif(full)

# keeping only significant variables ( from 1 variable to adding significant variable): here monthly charge and overage fee seems to be collinear hence dropping monthly charge


linear= Churn ~ ContractRenewal+CustServCalls+DataPlan+DayMins+OverageFee+RoamMins
fullnew = lm(linear, data = test.reg)
summary(fullnew)
vif(full)


mse1 <- mean((test.reg$Churn- pred.reg)^2)
print(mse1)

#Now some Predictions
pred.reg<-predict(full,newdata=test.reg)
pred.reg

#Confusionmatrix
tab.LPM<-table(test.reg$Churn, pred.reg > 0.5)
tab.LPM


accuracy.LPM<-sum(diag(tab.LPM))/sum(tab.LPM)
accuracy.LPM
loss.LPM<-tab.LPM[2,1]/(tab.LPM[2,1]+tab.LPM[1,1])
loss.LPM
opp.loss.LPM<-tab.LPM[1,2]/(tab.LPM[1,2]+tab.LPM[2,2])
opp.loss.LPM
tot.loss.LPM<-0.95*loss.LPM+0.05*opp.loss.LPM
tot.loss.LPM



#10 Fold Validation with Linear regression

library(caret)
set.seed(123)
folds<-createFolds(train$Churn,k=10)
str(folds)

installed.packages("plyr")
library(plyr)
linear= Churn ~ ContractRenewal+CustServCalls+DataPlan+DayMins+OverageFee++RoamMins
cv_reg<-lapply(folds,function(x){
  train.reg.kval<-train.reg[x,]
  test.reg.kval<-test.reg[-x,]
  reg.kval<-lm(linear, train.reg.kval)

 reg.kval.pred<-predict(reg.kval, test.reg.kval)
  tab.reg.kval<-table(test.reg.kval[,1], reg.kval.pred>0.5)
  
  sum(diag(tab.reg.kval))/sum(tab.reg.kval)
})

str(cv_reg)
fit.OLS<-mean(unlist(cv_reg))
fit.OLS

##################logistic Model###########################


#data frame
train.logit<-train
test.logit<-test
head(train.logit)
head(test.logit)


# Fit the Sigmoid function
head(phone)

#using one variable
Logit1 <-Churn ~CustServCalls
Logit <- glm(Logit1  , train.logit, family = binomial)
summary(Logit)
vif(Logit)




#####Retain only significant ones  after result from logit model
Logitfull <-Contractcancel ~DataPlan+RoamMins+DayMins+OverageFee+CustServCalls+Churn
Logit.final <- glm(Logitfull  , train.logit, family = binomial)
summary(Logit.final)
vif(Logit.final)

pred.logit.final <- predict.glm(Logit.final, newdata=test.logit, type="response")
pred.logit.final



#Classification
head(test)
dim(test)
library(caret)
tab.logit= confusion.matrix(test.logit$Churn, pred.logit.final,threshold = 0.5)
tab.logit

accuracy.logit<-sum(diag(tab.logit))/sum(tab.logit)
accuracy.logit
loss.logit<-tab.logit[1,2]/(tab.logit[1,2]+tab.logit[1,1])
loss.logit
opp.loss.logit<-tab.logit[2,1]/(tab.logit[2,1]+tab.logit[2,2])
opp.loss.logit
tot.loss.logit<-0.95*loss.logit+0.05*opp.loss.logit
tot.loss.logit


#############gini, ks, auc###########


library(ROCR)
library(ineq)
pred <- prediction(CD.sample$predict.score[,2], CD.sample$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(CD.sample$predict.score[,2], type="Gini")

with(CD.sample, table(Target, predict.class))
auc
KS
gini


################  KNN Model   ##########################

setwd("C:/Users/spandey/Desktop")
getwd()
phone = read.csv("Cellphone.csv", header = TRUE)
attach(phone)
# But before that, we will normalize

normalize<-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
head(phone)
names(phone)

#create dummy variables
#phone$Contractcancel<-ifelse(phone$Churn=="cancelContract",1,0)
head(phone)
#str(phone)
phone$norm.AccountWeeks<-normalize(AccountWeeks)
phone$norm.ContractRenewal<-normalize(ContractRenewal)
phone$norm.DataPlan<-normalize(DataPlan)
phone$norm.DataUsage<-normalize(DataUsage)
phone$norm.CustServCalls<-normalize(CustServCalls)
phone$norm.DayMins<-normalize(DayMins)
phone$norm.DayCalls<-normalize(DayCalls)
phone$norm.MonthlyCharge<-normalize(MonthlyCharge)
phone$norm.RoamMins<-normalize(RoamMins)
phone$norm.OverageFee<-normalize(OverageFee)

head(phone)

#divide data into training and val



set.seed(1)
rows <- sample(2, size = nrow(phone),replace=TRUE, prob=c(0.7,0.3))
train <- phone[rows==1,]
test <- phone[rows==2,]
dim(phone)
dim(train)
dim(test)


#Data Frame for KNN
#Normalization is must

train.NB<-train[,c(1,12:20)]
test.NB<-test[,c(1,12:20)]
head(train.NB)
head(test.NB)


####KNN
#knn3
y_pred.3<-knn(train=train.NB[,-1],test=test.NB[-1], cl=train.NB[,1],k=3)
tab.knn.3<-table(test.NB[,1],y_pred.3)
tab.knn.3

accuracy.NB<-sum(diag(tab.knn.3))/sum(tab.knn.3)
accuracy.NB
loss.NB<-tab.knn.3[2,1]/(tab.knn.3[2,1]+tab.knn.3[1,1])
loss.NB
opp.loss.NB<-tab.knn.3[1,2]/(tab.knn.3[1,2]+tab.knn.3[2,2])
opp.loss.NB
tot.loss.NB<-0.95*loss.NB+0.05*opp.loss.NB
tot.loss.NB


#knn5
y_pred.5<-knn(train=train.NB[,-1],test=test.NB[-1], cl=train.NB[,1],k=5)
tab.knn.5<-table(test.NB[,1],y_pred.5)
tab.knn.5

accuracy.knn.5<-sum(diag(tab.knn.5))/sum(tab.knn.5)
accuracy.knn.5
loss.knn.5<-tab.knn.5[2,1]/(tab.knn.5[2,1]+tab.knn.5[1,1])
loss.knn.5

#knn7
y_pred.7<-knn(train=train.NB[,-1],test=test.NB[-1], cl=train.NB[,1],k=7)
tab.knn.7<-table(test.NB[,1],y_pred.7)
tab.knn.7

accuracy.knn.7<-sum(diag(tab.knn.7))/sum(tab.knn.7)
accuracy.knn.7
loss.knn.7<-tab.knn.7[2,1]/(tab.knn.7[2,1]+tab.knn.7[1,1])
loss.knn.7

#knn9
y_pred.9<-knn(train=train.NB[,-1],test=test.NB[-1], cl=train.NB[,1],k=9)
tab.knn.9<-table(test.NB[,1],y_pred.9)
tab.knn.9

accuracy.knn.9<-sum(diag(tab.knn.9))/sum(tab.knn.9)
accuracy.knn.9
loss.knn.9<-tab.knn.9[2,1]/(tab.knn.9[2,1]+tab.knn.9[1,1])
loss.knn.9
#####kfold  validation #####################,]
library(caret)
set.seed(123)
folds<-createFolds(train$Churn,k=10)
str(folds)
cv_KNN.3<-lapply(folds,function(x){
  train.NB.kval<-train.NB[x,]
  test.NB.kval<-test.NB[-x,]
  train.knn.kval<-as.data.frame(lapply(train.NB.kval[,c(2:10)],normalize))
  test.knn.kval<-as.data.frame(lapply(test.NB.kval[,c(2:10)],normalize))
  train_target.kval<-train.NB.kval[,1]
  test_target.kval<-test.NB.kval[,1]
  knn3.kval<-knn(train=train.knn.kval,test=test.knn.kval,cl=train_target.kval,k=3)
  tab.knn.3.kval<-table(test_target.kval,knn3.kval)
  sum(diag(tab.knn.3.kval))/sum(tab.knn.3.kval)
})
str(cv_KNN.3)
fit.KNN<-mean(unlist(cv_KNN.3))
fit.KNN


##################### Naive Bayes####################

train.NB$Churn<-as.factor(train.NB$Churn)
test.NB$Churn<-as.factor(test.NB$Churn)
library(naivebayes)

NB<-naive_bayes(x=train.NB[-1], y=train.NB$Churn)
#pedict
y_pred.NB<-predict(NB,newdata=test.NB[-1])
y_pred.NB


#Confusion matrix

tab.NB=table(test.NB[,1],y_pred.NB)
tab.NB

accuracy.NB<-sum(diag(tab.NB))/sum(tab.NB)
accuracy.NB
loss.NB<-tab.NB[2,1]/(tab.NB[2,1]+tab.NB[1,1])
loss.NB
opp.loss.NB<-tab.NB[1,2]/(tab.NB[1,2]+tab.NB[2,2])
opp.loss.NB
tot.loss.NB<-0.95*loss.NB+0.05*opp.loss.NB
tot.loss.NB


library(caret)
set.seed(123)
folds<-createFolds(train$Churn,k=10)
str(folds)


cv_NB<-lapply(folds,function(x){
  train.NB.kval<-train.NB[x,]
  test.NB.kval<-test.NB[-x,]
  
  NB.kval<-naive_bayes(x=train.NB.kval[-1], y=train.NB.kval[,1])
  y_pred.NB.kval<-predict(NB,newdata=test.NB.kval[-1])
  cm.NB.kval=table(test.NB.kval[,1],y_pred.NB.kval)
  sum(diag(cm.NB.kval))/sum(cm.NB.kval)
})

str(cv_NB)
fit.NB<-mean(unlist(cv_NB))
fit.NB

#Decision Tree and RFM
#Data frame
head(phone)
train.NB<-train[,c(1,12:20)]
test.NB<-test[,c(1,12:20)]
head(train.NB)
head(test.NB)



###Now Decision Trees
library(rpart)
library(rpart.plot)
library(randomForest)

DT<-rpart(Churn~., method="class",train.NB)

rpart.plot(DT, type=3,extra=101,fallen.leaves = T)

pred.DT= predict(DT, type = "class",test.NB)

############### ks, gini, auc###################
pred.1 = predict(DT, test.NB)
library(ROCR)
pred <- prediction(pred.1[,2], train.NB$Churn)
perf <- performance(pred, "tpr", "fpr")
#install.packages("ineq")
library(ineq)
gini = ineq(pred.1, type="Gini")
gini
auc <- performance(pred,"auc");
auc <- as.numeric(auc@y.values)
auc
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

###############confusion matrix###########

tab.DT<-table( test.NB$Churn,pred.DT)
tab.DT
accuracy.DT<-sum(diag(tab.DT))/sum(tab.DT)
accuracy.DT
loss.DT<-tab.DT[2,1]/(tab.DT[2,1]+tab.DT[1,1])
loss.DT
opp.loss.DT<-tab.DT[1,2]/(tab.DT[1,2]+tab.DT[2,2])
opp.loss.DT
tot.loss.DT<-0.95*loss.DT+0.05*opp.loss.DT
tot.loss.DT

###############kfold###################
#########
#10 Fold on Decision Trees 
DT<-Churn ~ norm.AccountWeeks+norm.ContractRenewal+norm.DataPlan+norm.DataUsage+norm.CustServCalls+norm.DayMins+norm.DayCalls+norm.MonthlyCharge+norm.RoamMins

set.seed(123)
folds<-createFolds(train$Churn,k=10)
str(folds)
cv_DT<-lapply(folds,function(x){
  train.DT.kval<-train.NB[x,]
  test.DT.kval<-test.NB[-x,]
  DT.kval<-rpart(Churn~., method="class",train.DT.kval)
  pred.DT.kval = predict(DT.kval, type="class",newdata=test.DT.kval)
  tab.DT.kval<-table( pred.DT.kval,test.DT.kval[,1])
  sum(diag(tab.DT.kval))/sum(tab.DT.kval)
})

str(cv_DT)
fit.DT<-mean(unlist(cv_DT))
fit.DT


############NOW RANDOM FOREST


#Run RFM
rfm<-randomForest(Churn~.,train.NB)

predictrfm<-predict(rfm,test.NB)
tab.RFM<-table(test.NB$Churn,predictrfm>0.5)
tab.RFM

accuracy.RFM<-sum(diag(tab.RFM))/sum(tab.RFM)
accuracy.RFM
loss.RFM<-tab.RFM[2,1]/(tab.RFM[2,1]+tab.RFM[1,1])
loss.RFM
opp.loss.RFM<-tab.RFM[1,2]/(tab.RFM[1,2]+tab.RFM[2,2])
opp.loss.RFM
tot.loss.RFM<-0.95*loss.RFM+0.05*opp.loss.RFM
tot.loss.RFM

#######
######################K fold######################




