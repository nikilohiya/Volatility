setwd("D:/Mr. Nerd/Stevens/Spring 2018/Financial Econometrics/Project")
#install.packages("dygraphs")
library(quantmod)
library(tseries)
library(fBasics)
library(dygraphs)


getSymbols("CL", from = "2000-01-01", to = "2018-02-02")
dim(CL)
tail(CL)

prices <- CL$CL.Close

ret_oil <- dailyReturn(prices, lag=1)

ret_oil_log <- dailyReturn(log(prices), lag=1)
dim(ret_oil_log)

basicStats(ret_oil)
basicStats(ret_oil_log)

#Visualizations - Daily Returns And Log Returns
chartSeries(ret_oil_log)
dygraph(ret_oil_log,xlab = "Time", ylab = "Return", main = "Plot of returns vs time")
chartSeries(ret_oil)
dygraph(ret_oil)


#Weekly Returns
ret_oil_w <- diff(log(prices), lag=5)
dim(ret_oil_w)
chartSeries(ret_oil_w)
basicStats(ret_oil_w)
dygraph(ret_oil_w)
