setwd("~/R_Programming_file") #working directory 설정
wine<-read.csv("wine_binary.csv", stringsAsFactors = TRUE)
wine[,'quality'] <-as.factor(wine[,'quality'])
attach(wine)
table(quality)
#1. 데이터에 대한 간단한 탐색분석
summary(wine) #기술통계치 확인

hist(fixed.acidity,main="Histogram of acidity")
library(ggplot2)
ggplot(wine, aes(x=fixed.acidity, y=pH, color=quality))+geom_point(size = 3)

#2. 주어진 데이터를 7:3의 비율로 Train/Test set
set.seed(1000)
n<-nrow(wine) #데이터 수 체크
tr.idx<-sample.int(n, size = round(0.7*n))
wine.train <- wine[tr.idx,-12] #타겟 변수를 제외한 train set
wine.test <-wine[-tr.idx,-12] #타겟 변수를 제외한 test set
rftrain <-wine[tr.idx,]#타겟 변수를 포함한 전제 train set
rftest <-wine[-tr.idx,]#타겟 변수를 포함한 전체 test set
trainLabels<-wine[tr.idx,12] #타겟 변수 train set
testLabels<-wine[-tr.idx,12] #타겟 변수 test set
#3.K-NN, Decision Tree, Random Forest, Support vector machine,
# Logistic Regression 중 3개이상의 기법을 적용하여 비교 분석

#3-1. K-NN 기법
library(class)
library(caret)
library(scales)
knn_result <-knn(train=wine.train, test=wine.test, cl=trainLabels, k=5)
knn_result
confusionMatrix(knn_result, testLabels)
accuracy_k <- NULL
# try k=1 to nrow(train)/2, may use nrow(train)/3(or 4,5) depending the size of n in train data
nnum<-nrow(wine.train)/2
for(kk in c(1:nnum))
{
  set.seed(1234)
  knn_k<-knn(train=wine.train,test=wine.test,cl=trainLabels,k=kk)
  accuracy_k<-c(accuracy_k,sum(knn_k==testLabels)/length(testLabels))
}
# plot for k=(1 to n/2) and accuracy
test_k<-data.frame(k=c(1:nnum), accuracy=accuracy_k[c(1:nnum)])
# minimum k for the highest accuracy
min(test_k[test_k$accuracy %in% max(accuracy_k),"k"]) #result -> K = 1
knn_result2 <-knn(train=wine.train, test=wine.test, cl=trainLabels, k=1)
knn_result2
confusionMatrix(knn_result2, testLabels)

#3-2. Random Forest 기법

library(randomForest)
rf_result <- randomForest(quality~.,data=rftrain, importance=T)
rf_result
randomForest::importance(rf_result)
varImpPlot(rf_result)
rfpred<-predict(rf_result, rftest)
confusionMatrix(rfpred, rftest$quality)

#3-3. Logistic regression 기법
logit_result1<-glm(quality~., data=rftrain,family=binomial(logit))
summary(logit_result1) #AIC = 333.44 , 여러 케이스 중 AIC 가 가장 낮으므로 채택. 
logit_result2<-glm(quality~fixed.acidity+residual.sugar+chlorides+density+alcohol, data=rftrain,family=binomial(logit))
summary(logit_result2) #AIC = 397.45
logit_result3<-glm(quality~fixed.acidity+chlorides+density+alcohol, data=rftrain,family=binomial(logit))
summary(logit_result3) #AIC = 396.35
logit_result4<-glm(quality~fixed.acidity+free.sulfur.dioxide+sulphates, data=rftrain,family=binomial(logit))
summary(logit_result4) #AIC = 618.62

probability = predict(logit_result1, newdata = rftest, type = 'response')
logit_pred = ifelse(probability>0.5, 1, 0)
logit_pred = as.factor(logit_pred)
confusionMatrix(logit_pred, rftest$quality)
