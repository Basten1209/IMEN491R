setwd("~/R_Programming_file") #working directory 설정
wine<-read.csv("wine_binary.csv", stringsAsFactors = TRUE)
wine[,'quality'] <-as.factor(wine[,'quality'])

attach(wine)
sleep<-read.csv("sleep.csv")
attach(sleep)
require(dplyr)
sleep<- sleep %>% mutate(SleepTrouble = ifelse(SleepTrouble == "No",0,1))
sleep[,'SleepTrouble'] <-as.factor(sleep[,'SleepTrouble'])
summary(sleep)
library(class)
library(caret)
library(scales)
set.seed(1000)
n<-nrow(sleep) #데이터 수 체크
tr.idx<-sample.int(n, size = round(0.7*n))
sleeptrain <-sleep[tr.idx,]#타겟 변수를 포함한 전제 train set
sleeptest <-sleep[-tr.idx,]#타겟 변수를 포함한 전체 test set

logit_result1<-glm(SleepTrouble~., data=sleeptrain,family=binomial(logit))
summary(logit_result1)
probability = predict(logit_result1, newdata = sleeptest, type = 'response')
logit_pred = ifelse(probability>0.5, 1, 0)
logit_pred = as.factor(logit_pred)
confusionMatrix(logit_pred, sleeptest$SleepTrouble)

ggplot(sleep, aes(x=SleepTrouble))+geom_bar()

m1<- svm(SleepTrouble~., data = sleep)
summary(m1)
x<-sleep[, -10]
pred <- predict(m1, x)
y<-sleep[,10]
confusionMatrix(pred, y)
library(randomForest)
rf_out2<-randomForest(SleepTrouble~.,data=sleeptrain,importance=T,ntree=20, mrty=3)
rf_out2
round(importance(rf_out2), 2)
rfpred<-predict(rf_out2,sleeptest)
confusionMatrix(rfpred,sleeptest$SleepTrouble)

dat1<-wine[ , -12]
dat1<-na.omit(dat1)
install.packages("factoextra")
library(factoextra)
library(ggplot2)

fviz_nbclust(dat1, kmeans, method = "wss")
fviz_nbclust(dat1, kmeans, method = "gap_stat")

set.seed(1000)
km <- kmeans(dat1, 3, nstart = 25)
km

km <- kmeans(dat1, 3, nstart=10)
km

km <- kmeans(dat1, 3)
km
