setwd("D:/Mr. Nerd/Stevens/Spring 2018/Financial Econometrics/Project")

#install.packages("dygraphs")

library(quantmod)
library(tseries)
library(fBasics)
library(dygraphs)
library(forecast)
library(urca)


getSymbols("CL", from = "1983-03-02", to = "2018-03-02")
dim(CL)
tail(CL)




####RETURNS ON THE CLOSE PRICES####


prices <- CL$CL.Close

#Daily Returns
ret_oil <- dailyReturn(prices, lag=1)
dim(ret_oil)

#Daily Log Returns
ret_oil_log <- dailyReturn(log(prices), lag=1)
dim(ret_oil_log)

#Monthly Returns
ret_oil_m <- monthlyReturn(prices)
head(ret_oil_m)

#Monthly Log Returns
ret_oil_m_log <- monthlyReturn(prices)
head(ret_oil_m_log)

#Weekly Returns
ret_oil_w <- diff(prices, lag=5)
head(ret_oil_w)

#Weekly Log Returns
ret_oil_w_log <- diff(log(prices), lag=5)
head(ret_oil_w_log)



####DISCRIBTIVE STATS####

#Discribtive Stats - Prices, Volume
basicStats(CL)

#Discribtive Stats - Daily Returns And Log Returns
basicStats(ret_oil)
basicStats(ret_oil_log)

#Discribtive Stats - Monthly Returns And Log Returns
basicStats(ret_oil_m)
basicStats(ret_oil_m_log)

#Discribtive Stats - Weekly Returns And Log Returns
basicStats(ret_oil_w)
basicStats(ret_oil_w_log)



####VISUALIZATIONS####

#Visualizations - Prices, Volume (Daily Basis)
chartSeries(CL,show.grid = TRUE, log.scale = TRUE)
dygraph(CL)

#Visualizations - Daily, Weekly and Monthly Log Returns
chartSeries(ret_oil_log)
dygraph(ret_oil_log,xlab = "Time", ylab = "Return", main = "Plot of returns vs time")

chartSeries(ret_oil_w_log)
dygraph(ret_oil_w_log,xlab = "Time", ylab = "Return", main = "Plot of returns vs time")

chartSeries(ret_oil_m_log)
dygraph(ret_oil_m_log,xlab = "Time", ylab = "Return", main = "Plot of returns vs time")



####TESTS ON RETURNS####

#Checking PACF and ACf of the daily log returns.
acf(ret_oil_log)
pacf(ret_oil_log, lag = 100)

#Ljung box test performance on the Log Returns.
Box.test(ret_oil_log,lag=10,type="Ljung")

# tbats model testing weekly, Monthly, annual seasonality
# This test will automatically determine if a seasonal pattern is present.

x <- ts(ret_oil_log, frequency=365/7)
fit <- tbats(x)
seasonal <- !is.null(fit$seasonal)
seasonal

# The result is FALSE if not present and TRUE if it is present.

x <- ts(ret_oil_log, frequency=365/30)
fit <- tbats(x)
seasonal <- !is.null(fit$seasonal)
seasonal

x <- ts(ret_oil_log, frequency=365/365)
fit <- tbats(x)
seasonal <- !is.null(fit$seasonal)
seasonal

#ADF test on the log returns
adf.test(ret_oil_log)



####TESTS ON PRICES####
# Experiment on the prices here.
acf(CL$CL.Close, lag.max = 10)
pacf(CL$CL.Close, lag.max = 10)
Box.test(CL$CL.Close, lag = 10, type = "Ljung")

x <- ts(prices, frequency=365/7)
fit <- tbats(x)
seasonal <- !is.null(fit$seasonal)
seasonal

x <- ts(prices, frequency=365/30)
fit <- tbats(x)
seasonal <- !is.null(fit$seasonal)
seasonal

x <- ts(prices, frequency=365/365)
fit <- tbats(x)
seasonal <- !is.null(fit$seasonal)
seasonal

#ADF test on CL$CL.Price
adf.test(prices)

# AR Order Selection
ar_order <- ar(ret_oil_log, order.max=20)
ar_order



####ARIMA using Forecast Method####
final_arima <- auto.arima(ret_oil_log, max.p = 5, trace = TRUE, seasonal = TRUE, parallel = TRUE, stepwise = FALSE)
summary(final_arima)
names(final_arima)

#Polynomial Creation
po <- c(1,-final_arima$coef[1:5])
po

#Polynomial Roots
Mod(polyroot(po))


#Prediction of the Future 6 values with their upper and lower prediction limits
pr <- predict(final_arima, n.ahead = 6)
pr
lcl = pr$pred-1.96*pr$se
ucl = pr$pred+1.96*pr$se
cl <- cbind(lcl,ucl)
cl
