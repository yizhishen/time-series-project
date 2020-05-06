library(zoo)
library(ggplot2)
library(tseries)

library(forecast)
setwd("C:/Users/sky/downloads/") #设置工作区间，这个改成你自己的文件路径，注意是“/”
avocado <- read.csv("avocado.csv")
avocado$Date <- as.Date(avocado$Date)
is.na(avocado)  # 判断是否存在缺失
n <- sum(is.na(avocado))  # 输出缺失值个
Chicago <-avocado[avocado$region=="Chicago"&avocado$type=="organic" ,]
Chicago <- Chicago[order(Chicago$Date),]
Price <- Chicago$AveragePrice
boxplot(Price)
plot.ts(train_prices)
price_ts <-ts(as.vector(Price),start=c(2015,1,5), frequency=52)
train_prices <-ts(as.vector(price_ts[1:158]),start=c(2015,1,5), frequency=52)
tsdisplay(train_prices)
acf(train_prices)
pacf(train_prices)
adf.test(train_prices)


d1_train_prices <- diff(train_prices,1)
tsdisplay(d1_train_prices)
adf.test(d1_train_prices)

model1 <- auto.arima(train_prices, ic=c( "aic"), trace = T)

model1
summary(model1)
#model1 <- arima(train_prices, order=c(0,1,0))
model1

tsdiag(model1)


f.p1<-forecast(model1,h=11,level=c(99.5))

#
plot(f.p1)
lines(f.p1$fitted,col="green")
lines(price_ts,col="red")



