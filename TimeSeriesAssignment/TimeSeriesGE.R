rm(list=ls(all=TRUE))
library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
library(plyr)
library(zoo)
library(Quandl)
library(DMwR)

#data_GE = Quandl("BSE/BOM532309", api_key="LhxfCFDc4L_QCMrx91sA")

data_GE =  data_GE[nrow(data_GE):1,]
row.names(data_GE) = paste(1:nrow(data_GE))

names(data_GE)
head(data_GE,10)
tail(data_GE, 10)

summary(data_GE)
str(data)

data_Data_removed =  data_GE[, ! names(data_GE) %in% c("Date"), drop = F]
data_GE_imputed = knnImputation(data_Data_removed, k = 7)
data_GE = cbind(data_GE$Date, data_GE_imputed)
names(data_GE)[names(data_GE) == 'data_GE$Date'] = 'Date'
View(data_GE)

test_rows = which(data_GE$Date >= "2017-05-31")
train_data = data_GE[-test_rows,]
test_data = data_GE[test_rows,]

Price = ts(train_data$High,frequency =5)

plot(Price)
