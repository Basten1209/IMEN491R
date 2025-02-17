library(ggplot2)
diamond<-read.csv(file = "diamonds.csv")
attach(diamond)
dim(diamond)
mean(diamond$price)
par(mfrow=c(1,2))
boxplot(carat, boxwex = 0.5, col = "red", main="carat")
boxplot(price, boxwex = 0.5, col = "yellow", main="price")
help(boxplot)
hist(carat, col = "red", main="carat")
hist(price, col = "yellow", main = "price")
ggplot(diamond, aes(x=cut, y=price))+geom_boxplot()
subclarity <- subset(diamond, clarity=="I1" | clarity=="SI1" | clarity=="VS1")
par(mfrow=c(1,1))

ggplot(subclarity, aes(x=carat, y=price), color=clarity)+geom_point()+geom_smooth(method = 'lm')
m1<-lm(subclarity$price~subclarity$carat)
summary(m1)
subclarity2 <-subset(diamond, clarity=="SI1" )
ggplot(subclarity2, aes(x=carat, y=price), color=clarity)+geom_point()
m2<-lm(subclarity2$price~subclarity2$carat)
summary(m2)
ggplot(subclarity2, aes(x=carat, y=price,color = clarity), color=clarity)+geom_point()
cor.test(subclarity2$carat, subclarity2$price)
