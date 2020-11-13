
#importing the required libraries
#install.packages("tseries")
#install.packages("forecast")
#install.packages("vars")


library(tsm)
library(vars)

# Import the dataset
library(readxl)
data <- read_excel("BTC-Nifty.xlsx")

# Checking the first few rows
head(data)

# Defining the series
lnstk_data<-ts(data[8], start = 2015, frequency = 52)
lnstk_data<-na.omit(lnstk_data)
head(lnstk_data)

lnbtc_data<-ts(data[7], start =2015, frequency = 52)
lnbtc_data<- na.omit(lnbtc_data)
head(lnbtc_data)

#viewing the data
plot(cbind(lnstk_data, lnbtc_data))

#undersanding the time series
lnstk.acf <- acf(lnstk_data, main = "Returns on Nifty")
lnbtc.acf<-acf(lnbtc_data,main="Returns on Bitcoins")

#checking for stationarity
#adf test on nifty
tseries::adf.test(lnstk_data)
#Log stock price(nifty) are clearly stationary

#adf test on nifty
tseries::adf.test(lnbtc_data)
#log bitcoins is also stationary at levels

#model selection and estimation
dat.bv <- cbind(lnstk_data, lnbtc_data)
colnames(dat.bv) <- c("lnstk_data", "lnbtc_data")

info.bv <- VARselect(dat.bv, lag.max = 12, type = "const")
info.bv$selection

# a lag of 1 is taken as appropriate while estimating the model
bv.est <- VAR(dat.bv, p = 1, type = "const", season = NULL, exog = NULL)
summary(bv.est)

#tocheck for autocorrelation of the model
bv.serial <- serial.test(bv.est, lags.pt = 12, type = "PT.asymptotic")
bv.serial

plot(bv.serial, names = "lnstk_data")
#results indicate that there is no serialcorrelation.

#granger causality
bv.cause.lnstk <- causality(bv.est, cause = "lnstk_data")
bv.cause.lnstk

bv.cause.lnbtc <- causality(bv.est, cause = "lnbtc_data")
bv.cause.lnbtc
# thus, although pvalue is slightly above 5%, we could say that 
#returns on (nifty)stock market granger causes bitcoin retruns(at 10%) 
#but the vice versa is not true
#we could not reject the null hypothesis that there is no instantaneous relationship bw them



##IMPULSE RESPONSE FUNCTION
irf.lnstk <- irf(bv.est, impulse = "lnbtc_data", response = "lnstk_data", 
               n.ahead = 40, boot = TRUE)
plot(irf.lnstk, ylab = "Nifty Returns", main = "Shock from returns on bitcoins")


irf.lnbtc <- irf(bv.est, impulse = "lnstk_data", response = "lnbtc_data", 
                 n.ahead = 40, boot = TRUE)
plot(irf.lnstk, ylab = "Bitcoin Returns", main = "Shock from returns on Nifty")

bv.vardec <- fevd(bv.est, n.ahead = 10)
plot(bv.vardec)

predictions <- predict(bv.est, n.ahead = 8, ci = 0.95)
plot(predictions, names = "lnstk_data")

predictions <- predict(bv.est, n.ahead = 8, ci = 0.95)
plot(predictions, names = "lnbtc_data")

fanchart(predictions, names = "lnstk_data")
fanchart(predictions, names = "lnbtc_data")
