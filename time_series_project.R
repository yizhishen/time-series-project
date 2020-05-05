library("readxl")
library(zoo)
library(ggplot2)
library(tseries)
setwd("C:/Users/sky/downloads/") #设置工作区间，这个改成你自己的文件路径，注意是“/”
avocado <- read.csv("avocado.csv")
avocado$Date <- as.numeric(avocado$Date)
is.na(avocado)  # 判断是否存在缺失
n <- sum(is.na(avocado))  # 输出缺失值个
boxplot(avocado$AveragePrice)
plot.ts(avocado$AveragePrice)
Price <- ts(avocado$AveragePrice)
ggplot(data = avocado, aes(x = avocado$Date, y = avocado$AveragePrice))+geom_line(color = "#00AFBB", size = 0.3) + labs(x="Date",y="Prices($)",title = "WTI Crude Oil Prices - Weekly Chart")        
zz <-avocado[avocado$region=="Atlanta",]     
plot(avocado$AveragePrice,type="l")
acf(Price)
pacf(Price)
adf.test(Price)
dd <- diff(log(Price))
plot.ts(dd)
acf(dd)
adf.test(dd)

pacf(dd)
resm <- ar(dd, method="mle"); resm
plot(as.numeric(names(resm$aic)), resm$aic, type="h",
     xlab="k", ylab="AIC")
resm2 <- arima(dd, order=c(12,0,0))
Box.test(resm2$residuals, lag=12, type="Ljung", fitdf=3)

resm4 <- arima(dd[1:18000], order=c(12,0,0))
resm4
pred4 <- predict(resm4, n.ahead=12, se.fit=TRUE)
cbind(Observed=round(c(dd[18001:18012]), 4), 
      Predict=round(c(pred4$pred), 4), 
      SE=round(c(pred4$se), 3))


