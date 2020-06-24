library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(lubridate)
data_exchng<-read.csv("C:/Alok/OneDrive/Rutgers_MITA/Semester1/BusinessForecasting_Turkoz/BF_Project/BF_Project_Irt_USDINRExchange/USD_INR_2_Finaldataset.csv")
data_exchng
attach(data_exchng)
df1<-data_exchng[c(1:3652),]
df2<-data_exchng[c(3653:3957),]
INRUSDTrain <- ts(df1$Price,     # random data
                  start = c(2009, as.numeric(format(inds[1], "%j"))),
                  end = c(2019, as.numeric(format(inds[1], "%j"))),
                  frequency = 365)
plot(INRUSDTrain)
INRUSDTest <- ts(df2$Price,     # random data
                 start = c(2019, as.numeric(format(inds[1], "%j"))),
                 #                  end = c(2019, as.numeric(format(inds[1], "%j"))),
                 frequency = 365)
plot(INRUSDTest)
INRUSD_Price2 = ts(data_exchng$Price, start=c(2009, 1), freq=365.25)
plot(INRUSD_Price2,col='blue') 
sum(is.na(data_exchng[,2]))
summary(Price)
str(Price)
dim(data_exchng)
fitstl <- stl(INRUSDTrain, s.window=5)
plot(fitstl)
seasadj(fitstl)
plot(seasadj(fitstl))
plot(fitstl$time.series[,1], col="gray", main="Seasonality component from STL decomposition", ylab="seasonality", xlab="")
plot(fitstl$time.series[,2], col="gray", main="Trend component from STL decomposition", ylab="trend", xlab="")
plot(INRUSDTrain,col="black")
lines(seasadj(fitstl),col="blue")
fcholt <- holt(INRUSDTrain, h=305)                 
round(accuracy(f=fcholt,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)
autoplot(INRUSDTrain) +
  autolayer(fcholt, series="Holt's", PI=FALSE) +
  autolayer(fitted(fcholt),series = "Holt",PI=FALSE)+
  autolayer(INRUSDTest,series = "TD",PI=FALSE)+
  ggtitle("Forecasts") + xlab("Year") +
  ylab("INR USD Exchange rate in RS") +
  guides(colour=guide_legend(title="Forecast"))
snfit = snaive(INRUSDTrain,h=305)
snfc = forecast(snfit,h=305)
round(accuracy(f=snfc,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)
autoplot(INRUSDTrain) +
  autolayer(snfc, series="SN", PI=FALSE) +
  autolayer(fitted(snfit),series = "SN",PI=FALSE)+
  autolayer(INRUSDTest,series = "TD",PI=FALSE)+
  ggtitle("Forecasts") + xlab("Year") +
  ylab("INR USD Exchange rate in RS") +
  guides(colour=guide_legend(title="Forecast"))
nfit = naive(INRUSDTrain,h=305)
nfc = forecast(nfit,h=305)
round(accuracy(f=nfc,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)
autoplot(INRUSDTrain) +
  autolayer(nfc, series="N", PI=FALSE) +
  autolayer(fitted(nfit),series = "N",PI=FALSE)+
  autolayer(INRUSDTest,series = "Test Data",PI=FALSE)+
  ggtitle("Forecasts") + xlab("Year") +
  ylab("INR USD Exchange rate in RS") +
  guides(colour=guide_legend(title="Forecast"))
mnfit = meanf(INRUSDTrain,h=305)
mnfc = forecast(mnfit,h=305)
round(accuracy(f=mnfc,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)
autoplot(INRUSDTrain) +
  autolayer(mnfc, series="M", PI=FALSE) +
  autolayer(fitted(mnfit),series = "M",PI=FALSE)+
  autolayer(INRUSDTest,series = "TD",PI=FALSE)+
  ggtitle("Forecasts") + xlab("Year") +
  ylab("INR USD Exchange rate in RS") +
  guides(colour=guide_legend(title="Forecast"))
Test=ur.kpss(INRUSDTrain) 
summary(Test)
test2=adf.test(INRUSDTrain)
tsDiff <- diff(INRUSDTrain)
plot(tsDiff)
test2r=adf.test(tsDiff)
ndiffs(INRUSD_Price2)
plot(Acf(INRUSDTrain,lag.max=100))
plot(Pacf(INRUSDTrain,lag.max = 100))
plot(Acf(tsDiff,lag.max = 100))
plot(Pacf(tsDiff,lag.max = 100))
arimafit<-auto.arima(INRUSDTrain,seasonal=FALSE) #(3,1,4)
fitted(arimafit)
arimafc = forecast(arimafit,h=305)
round(accuracy(f=arimafc,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)
autoplot(INRUSDTrain) +
  autolayer(arimafc, series="AR", PI=FALSE) +
  autolayer(fitted(arimafit),series = "AR",PI=FALSE)+
  autolayer(INRUSDTest,series = "TD",PI=FALSE)+
  ggtitle("Forecasts") + xlab("Year") +
  ylab("INR USD Exchange rate in RS") +
  guides(colour=guide_legend(title="Forecast"))
autoplot(INRUSDTrain) +
  autolayer(meanf(INRUSDTrain, h=305),
            series="M", PI=FALSE) +
  autolayer(naive(INRUSDTrain, h=305),
            series="N", PI=FALSE) +
  autolayer(snaive(INRUSDTrain, h=305),
            series="S", PI=FALSE) +
  autolayer(holt(INRUSDTrain, h=305),
            series="HW", PI=FALSE) +
  autolayer(arimafc,
            series="AR", PI=FALSE) +
  ggtitle("INR - USD -Currency Exchange Rate") +
  xlab("Year") + ylab("Price (INR)") +
  guides(colour=guide_legend(title="Forecast"))
library(ggplot2)
library(ggfortify) 
library(urca)
ggtsdiag(arimafit) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))
plot(INRUSDTrain)
residFit <- ggplot(data=arimafit, aes(residuals(arimafit))) +
  geom_histogram(aes(y =..density..),  
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of INRUSD Price ARIMA Model Residuals")
plot(residFit)

