#Importing data
data <- read_excel("BTC-Nifty.xlsx")

head(data)

btc_data<-ts(data[,3])
head(btc_data)
btc_data<-na.omit(btc_data)
btc_data<-ts(btc_data, start = 2015, frequency = 52)

dim(btc_data)

plot(btc_data, col = 2,lwd = 2, xlab = "weeks", ylab = " bitcoins BTC/INR ",main='Bitcoins(2015-2019)')
abline(reg = lm(btc_data~time(btc_data)), col = 4, lwd = 2)
mean(btc_data)
var(btc_data)
sd(btc_data)
max(btc_data)
#btc <- ts(btc_data[1:305], start = 2014, frequency = 52)

#decinf=ding order on original data
# ACF and PACF of Exchnage Rate
pacf(btc_data, col = 3, lwd = 2, main = "PACF of log reurns on bitcoins")
acf(btc_data, col = 3, lwd = 2, main = "ACF of log returns of bitcoins")

#acf is significant at 0 lags.pacf has 3,4 significant lag
library(tseries)
#adf test on exchange rate
tseries::adf.test(btc_data)
#ADF on df using default number of lags( amount of time dependency, for a time series of 100 this is 4)
tseries::adf.test(btc_data, k=0)
#

##lnbtc is non-stationary at levels
#taking first difference
btc_diff<-diff(btc_data)

plot(btc_diff, col = 2,lwd = 2, xlab = "weeks", ylab = "bitcoins prices", main='Bitcoins after first Difference')
abline(reg = lm(btc_diff~time(btc_diff)), col = 4, lwd = 2)

#adf test on exchange rate
tseries::adf.test(btc_diff)
#ADF on df using default number of lags( amount of time dependency, for a time series of 100 this is 4)
tseries::adf.test(btc_diff, k=0)



#modelling is done on 300 obs, out of sample model
# ACF and PACF of Exchnage Rate
pacf(btc_diff, col = 3, lwd = 2, main = "PACF of First Difference for Bitcoins")
acf(btc_diff, col = 3, lwd = 2, main = "ACF of First Difference for Bitcoins")


#p=0,3,4,11,16 ,d=1,q=0,3,4,6,11,16

btc1<-ts(diffinv(btc_diff))
btc <-ts(btc1[1:251],start=2014,frequency=52)


model_1 <- arima(btc,order=c(0,1,0))
model_2 <- arima(btc,order=c(0,1,3))
model_3 <- arima(btc,order=c(0,1,4))
model_4 <- arima(btc,order=c(0,1,6))
model_5 <- arima(btc,order=c(0,1,11))
model_6 <- arima(btc,order=c(3,1,0))
model_7 <- arima(btc,order=c(3,1,3))
model_8 <- arima(btc,order=c(3,1,4))
model_9 <- arima(btc,order=c(3,1,6))
model_10 <- arima(btc,order=c(3,1,11))
model_11 <- arima(btc,order=c(4,1,0))
model_12 <- arima(btc,order=c(4,1,3))
model_13 <- arima(btc,order=c(4,1,4))
model_14 <- arima(btc,order=c(4,1,6))
model_15 <- arima(btc,order=c(4,1,11))
model_16 <- arima(btc,order=c(11,1,0))
model_17 <- arima(btc,order=c(11,1,3))
model_18 <- arima(btc,order=c(11,1,4))
model_19 <- arima(btc,order=c(11,1,6))
model_20 <- arima(btc,order=c(11,1,11))
model_21 <- arima(btc,order=c(16,1,0))
model_22 <- arima(btc,order=c(16,1,3))
model_23 <- arima(btc,order=c(16,1,4))
model_24 <- arima(btc,order=c(16,1,6))
model_25 <- arima(btc,order=c(16,1,11))
model_26 <- arima(btc,order=c(16,1,16))
model_27 <- arima(btc,order=c(0,1,16))
model_28<- arima(btc,order=c(3,1,16))
model_29 <- arima(btc,order=c(4,1,16))
model_30<- arima(btc,order=c(6,1,16))
model_31<- arima(btc,order=c(11,1,16))

scores_AIC <- AIC(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10,model_11,model_12,model_13,model_14,model_15,model_16,model_17,model_18,model_20,model_21,model_22,model_23,model_24,model_25,model_26,model_27,model_28,model_29,model_30,model_31)
scores_BIC <- BIC(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10,model_11,model_12,model_13,model_14,model_15,model_16,model_17,model_18,model_20,model_21,model_22,model_23,model_24,model_25,model_26,model_27,model_28,model_29,model_30,model_31)

aic <- scores_AIC$AIC
bic <- scores_BIC$BIC

#Comparing the AIC & BIC scores to select the best model
scores <- data.frame(aic, bic)
scores
#model_11 and model_4 have lowest BIC
#Residual plots

pacf(model_1$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-1 PACF")
acf(model_1$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-1 ACF")
hist(model_1$residuals)

pacf(model_11$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-11 PACF")
acf(model_11$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-11 ACF")

pacf(model_4$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-4 PACF")
acf(model_4$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-4 ACF")

pacf(model_6$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-6 PACF")
acf(model_6$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-6 ACF")
hist(model_6$residuals)

pacf(model_2$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-2 PACF")
acf(model_2$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-2 ACF")



btc_data[252:261]
#Forecasting next 10 year values for log return on bitcoins
#install.packages("forecast")
library(forecast)

model_1_pred <- forecast(model_1, h = 10, level = c(90, 95))
head(model_1_pred)

model_11_pred <- forecast(model_11, h = 10, level = c(90, 95))
head(model_11_pred)

model_6_pred <- forecast(model_6, h = 10, level = c(90, 95))

model_2_pred <- forecast(model_2, h = 10, level = c(90, 95))

model_4_pred <- forecast(model_4, h = 10, level = c(90, 95))

plot(model_1_pred)
plot(model_11_pred)
plot(model_6_pred)
plot(model_2_pred)
plot(model_4_pred)

#Calculating the Error in Forecasting
actuals <- btc_data[252:261]
actuals
length(actuals)
length(model_1_pred)

#errors
#Model_8
model_1_error <-((actuals - model_1_pred$mean)/actuals)
head(model_1_error)

#Model_3
model_11_error <- ((actuals - model_11_pred$mean)/actuals)

#Model_9
model_6_error <- ((actuals - model_6_pred$mean)/actuals)

model_2_error <- ((actuals - model_2_pred$mean)/actuals)
model_4_error <- ((actuals - model_4_pred$mean)/actuals)

mean_error <- c(mean(model_1_error), mean(model_11_error), mean(model_6_error),mean(model_2_error), mean(model_4_error))
mean_error

#Calculating the Root Mean Squared Error for the forecasted values
#install.packages("Metrics")
library(Metrics)

rmse(actuals, model_1_pred$mean)

rmse(actuals, model_11_pred$mean)

rmse(actuals, model_6_pred$mean)

rmse(actuals, model_2_pred$mean)

rmse(actuals, model_4_pred$mean)

#model 11 has lowest Bic But doesnt not perform so well in rmse or mean errors. model _6 has lowest rmse

#Comparing the plots of the Actuals v/s the Forecasts
#install.packages("ggplot2")
library(ggplot2)

week<-c(252:261)
week
ggplot() + geom_line(aes(x = week, y = actuals, colour = 'red')) + geom_line(aes(x= week, y = model_1_pred$mean, colour = 'yellow')) + geom_line(aes(x= week, y = model_11_pred$mean, colour = 'blue'))+ geom_line(aes(x= week, y = model_6_pred$mean, colour = 'orange'))+ geom_line(aes(x= week, y = model_2_pred$mean, colour = 'voilet')) + geom_line(aes(x= week, y = model_4_pred$mean, colour = 'green')) + ylab(" bitcoins") + xlab("week")
#clearly somemodels couldbe left out

ggplot() + geom_line(aes(x = week, y = actuals, colour = 'red')) + geom_line(aes(x= week, y = model_11_pred$mean, colour = 'blue'))+ geom_line(aes(x= week, y = model_6_pred$mean, colour = 'orange'))+ geom_line(aes(x= week, y = model_4_pred$mean, colour = 'green')) + ylab(" bitcoins") + xlab("week")
# i prefer model with lowest BIC ie. Model 11.
#hence our model is ARIMA(4,1,0)


## model on the whole data
btc_model_1 <- arima(btc_data,order=c(4,1,0))
btc_model_2 <- arima(btc_data,order=c(3,1,0))
btc_model_3 <- arima(btc_data,order=c(0,1,6))

library(forecast)

btc_model_1_pred <- forecast(btc_model_1, h = 10, level = c(90, 95))

btc_model_2_pred <- forecast(btc_model_2, h = 10, level = c(90, 95))

btc_model_3_pred <- forecast(btc_model_3, h = 10, level = c(90, 95))

weeks<- c(262:271)
week
#forecasting future prices
plot(btc_model_3_pred)

ggplot()+ geom_line(aes(x= weeks, y = btc_model_1_pred$mean, colour = 'blue'))+ geom_line(aes(x= weeks, y = btc_model_2_pred$mean, colour = 'orange'))+ geom_line(aes(x= weeks, y = btc_model_3_pred$mean, colour = 'green')) + ylab(" bitcoins") + xlab("week")

ggplot()+ geom_line(aes(x= weeks, y = btc_model_1_pred$mean, colour = 'blue')) + ylab(" bitcoins forecast") + xlab("week")


model_11
