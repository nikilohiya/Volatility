setwd("D:/Mr. Nerd/Stevens/Spring 2018/Financial Econometrics/Project")

library(quantmod)
library(tseries)
library(fBasics)
library(dygraphs)
library(forecast)
library(urca)
library(xts)
library(MTS)

#### Importing Data####
a <- read.csv(file = "Futures_Data_Oil.csv")
CL_F <- a[,-1]
rownames(CL_F) <- a[,1]
CL_F <- as.xts(CL_F)
remove(a)
head(CL_F)

getSymbols("CL", from = "1983-01-30", to = "2018-02-21")
dim(CL)
tail(CL)


#### Performing VAR Operation####

