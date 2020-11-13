#Importing data

library(readxl)
data <- read_excel("BTC-Nifty.xlsx")
View(data)
head(data)

stk_data<-ts(data[,4])
tail(stk_data)

stk_data<-na.omit(stk_data)
stk_data<-ts(stk_data, start = 2015, frequency = 52)

head(stk_data)
dim(stk_data)

plot(stk_data, col = 2,lwd = 2, xlab = "weeks", ylab = "nifty",main='Nifty 50 (2015-2019)')
abline(reg = lm(stk_data~time(stk_data)), col = 4, lwd = 2)
mean(stk_data)
var(stk_data)
sd(stk_data)

cts<-decompose(stk_data)
plot(cts,main='Decomposition of Nifty')

#deciding order on original data
# ACF and PACF of stock market 
acf(stk_data, col = 3, lwd = 2, main = "ACF of nifty")
pacf(stk_data, col = 3, lwd = 2, main = "PACF of nifty")

#acf is significant falling .pacf has 1 significant lag
library(tseries)

#adf test on nifty
tseries::adf.test(stk_data)
#ADF on df using default number of lags( amount of time dependency, for a time series of 100 this is 4)
tseries::adf.test(stk_data, k=0)

pp.test(stk_data)
kpss.test(stk_data, null="Trend")

##stk is not stationary at levels
stk_diff<-diff(stk_data)
#testing for stationarity

plot(stk_diff, col = 2,lwd = 2, xlab = "weeks", ylab = "nifty", main='Nifty after first Difference')
abline(reg = lm(stk_diff~time(stk_diff)), col = 4, lwd = 2)

#adf test on nifty
tseries::adf.test(stk_diff)
#ADF on df using default number of lags( amount of time dependency, for a time series of 100 this is 4)
tseries::adf.test(stk_diff, k=0)

pp.test(stk_diff)
kpss.test(stk_diff, null="Trend")
#stk_diff is stationary-i.e nifty is stationary at first differnece
#determining the order
pacf(stk_diff, col = 3, lwd = 2, main = "PACF of first difference of nifty")
acf(stk_diff, col = 3, lwd = 2, main = "ACF of first difference nifty")

#p=0,8,d=1,q=0,8

stk1<-ts(diffinv(stk_diff))
stk<-ts(stk1[1:251], start = 2015, frequency = 52)

#acf is significant at 0,8 lag. and pacf is significant at 0,8 lag

#modelling is done on 260 obs, out of sample modell

imodel_1 <- arima(stk,order=c(0,1,0))
imodel_2 <- arima(stk,order=c(0,1,8))
imodel_3 <- arima(stk,order=c(8,1,0))
imodel_4 <- arima(stk,order=c(8,1,8))


plot(stk, col = 2,lwd = 2, xlab = "weeks", ylab = "nifty")
abline(reg = lm(stk~time(stk)), col = 4, lwd = 2)

scores_AIC <- AIC(imodel_1,imodel_2,imodel_3,imodel_4)
scores_BIC <- BIC(imodel_1,imodel_2,imodel_3,imodel_4)

aic <- scores_AIC$AIC
bic <- scores_BIC$BIC

#stk_data[251:260]
#Comparing the AIC & BIC scores to select the best model
scores <- data.frame(aic, bic)
scores
#model 3,2 has least AIC and BIC. model 3 performs better than 2, if we ignore model1.
#although model 1 has less AIC ad BIc , it is primarily because of least no of parametrs
#Residual plots
pacf(imodel_1$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-1 PACF")
acf(imodel_1$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-1 ACF")
hist(imodel_1$residuals)

pacf(imodel_2$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-2 PACF")
acf(imodel_2$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-2 ACF")
hist(imodel_2$residuals)

pacf(imodel_3$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-3 PACF")
acf(imodel_3$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-3 ACF")
hist(imodel_3$residuals)

pacf(imodel_4$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-4 PACF")
acf(imodel_4$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-4 ACF")
head(imodel_4$residuals)


#Forecasting nstkt 10 year values for 'Trade Openness in India'
#install.packages("forecast")
library(forecast)

imodel_1_pred <- forecast(imodel_1, h = 10, level = c(90, 95))
head(imodel_1_pred)

imodel_2_pred <- forecast(imodel_2, h = 10, level = c(90, 95))
head(imodel_2_pred)

imodel_3_pred <- forecast(imodel_3, h = 10, level = c(90, 95))
head(imodel_3_pred)


imodel_4_pred <- forecast(imodel_4, h = 10, level = c(90, 95))

plot(imodel_1_pred, xlab='Time', ylab='stkchnage Rate Forecast')
plot(imodel_2_pred, xlab='Time', ylab='stkchnage Rate Forecast')
plot(imodel_3_pred, xlab='Time', ylab='stkchnage Rate Forecast')
plot(imodel_4_pred, xlab='Time', ylab='stkchnage Rate Forecast')

#Calculating the Error in Forecasting
iactuals <- stk_data[252:261]
head(iactuals)

#errors
imodel_1_error <- ((iactuals - imodel_1_pred$mean)/iactuals)
head(imodel_1_error)

imodel_2_error <- ((iactuals - imodel_2_pred$mean)/iactuals)
head(imodel_2_error)
imodel_3_error <- ((iactuals - imodel_3_pred$mean)/iactuals)

imodel_4_error <- ((iactuals - imodel_4_pred$mean)/iactuals)

plot(cbind(imodel_1_error,imodel_2_error,imodel_3_error,imodel_4_error),type='b')

mean_error <- c(mean(imodel_1_error),mean(imodel_2_error),mean(imodel_3_error),mean(imodel_4_error))
mean_error


#Calculating the Root Mean Squared Error for the forecasted values
#install.packages("Metrics")
library(Metrics)

#Model_1
#Model_1
rmse(iactuals, imodel_1_pred$mean)

rmse(iactuals, imodel_2_pred$mean)

rmse(iactuals, imodel_3_pred$mean)

rmse(iactuals, imodel_4_pred$mean)


#Comparing the plots of the Actuals v/s the Forecasts
#install.packages("ggplot2")
library(ggplot2)

week<- c(252:261)
week

ggplot() + geom_line(aes(x= week, y = iactuals,color='red' ))+geom_line(aes(x= week, y = imodel_1_pred$mean,color='blue' )) + geom_line(aes(x= week, y = imodel_2_pred$mean, color='green'))+geom_line(aes(x= week, y = imodel_3_pred$mean, color='yellow'))+geom_line(aes(x= week, y = imodel_4_pred$mean, color='orange' )) + ylab(" nifty") + xlab("week")


ggplot() + geom_line(aes(x= week, y = iactuals,color='orange' )) + ylab(" nifty") + xlab("week")

ggplot() + geom_line(aes(x= week, y = imodel_1_pred$mean,color='red' )) + geom_line(aes(x= week, y = imodel_2_pred$mean, color='green'))+geom_line(aes(x= week, y = imodel_3_pred$mean, color='yellow'))+geom_line(aes(x= week, y = imodel_4_pred$mean, color='orange' )) + ylab(" nifty") + xlab("week")

#model 3 performs better relative to other models. 
#although the forecasts graphs show a linear forecasts, a seperate plot of modleforecasts show that model 2 and 3 follow the trend of nifty.and model 3 performs better than model 2.
# hence we can conclude that nifty follows an ARIMA(8,1,0) model.

##normal forecast
ifor_1 <- arima(stk_data,order=c(8,1,0))
ifor_2 <- arima(stk_data,order=c(0,1,8))


#forcasts
ifor_1_pred <- forecast(ifor_1, h = 10, level = c(90, 95))
ifor_2_pred <- forecast(ifor_2, h = 10, level = c(90, 95))

head(ifor_1_pred)
head(ifor_2_pred)
#fit_basic1<- auto.arima(stk1)
#forecast_1<-forecast(fit_basic1)
#forecast_1

plot(ifor_1_pred)
plot(ifor_2_pred)

fut<-c(262:271)
ggplot() + geom_line(aes(x= fut, y = ifor_1_pred$mean,color='blue' )) + geom_line(aes(x= fut, y = ifor_2_pred$mean, color='green')) + ylab(" nifty") + xlab("forecasts")

imodel_2
