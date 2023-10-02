#######################################################################################
#PROPHET MODELLING

library("zoo")
library('astsa')
library("readr")
library("forecast")
library("dplyr")
library("pracma")
library("xts")
library("marima")
library("tidyverse")
library("zoo")
library("prophet")
library("data.table")
library("ggplot2")
library("reshape2")
library("bsts")

data <- read.csv("/Users/pinkienguyen/Documents/GSU - MSA:MAS/MSA 8200/Final Project/archive/State_time_series.csv")
data <- data%>%filter(RegionName == 'Georgia', !is.na(ZHVI_AllHomes))

#Renaming the columns to use in the Prophet model
data = rename(data, ds = Date)
data = rename(data, y = ZHVI_AllHomes)

#Splitting the test and training datasets 80:20.
train = data[1:209,]
test = data[210:261,]


acf(train$y, lag.max = 100)

#Prophet Model training
P = prophet(train, n.changepoints = 48)

future <- make_future_dataframe(P, periods = 52, freq = 'month')
forecast <- predict(P, future)

#Plotting Prophet Model components
prophet_plot_components(P, forecast)

#Plotting the forecasted sales
plot(P, forecast, xlab="Week", ylab="Sales") +
  add_changepoints_to_plot(P, threshold = 0.1, cp_color = "red", cp_linetype="dashed", trend = TRUE)

#Calculating MSPE
MSPE1 = mean(abs((tail(forecast$yhat,52)-test$y)/test$y))*100
MSPE1


#BSTS Model Training
#1. Apply Trend Only

ss <- AddLocalLinearTrend(list(), train$y)
model1 <- bsts(train$y,
               state.specification = ss,
               niter = 1000)
plot(model1)
lines(train$y)
plot(model1, "components")


pred1 <- predict(model1, horizon = 52)
plot(pred1, plot.original = 209)

#Calculating MSPE
MSPE2 = mean(abs((tail(pred1$mean,52)-test$y)/test$y))*100
MSPE2


#2. Apply Seasonality only
ss <- AddSeasonal(ss, data$y, nseasons = 12)
model2 <- bsts(train$y,
               state.specification = ss,
               niter = 1000)
plot(model2)
lines(train$y)
plot(model2, "components")


pred2 <- predict(model2, horizon = 52)
plot(pred2, plot.original = 209)


#Calculating MSPE
MSPE3 = mean(abs((tail(pred2$mean,52)-test$y)/test$y))*100
MSPE3

#3. Apply Both
ss <- AddLocalLinearTrend(list(), train$y)
ss <- AddSeasonal(ss, data$y, nseasons = 12)
model3 <- bsts(train$y,
               state.specification = ss,
               niter = 1000)
plot(model3)
lines(train$y)
plot(model3, "components")


pred3 <- predict(model3, horizon = 52)
plot(pred3, plot.original = 209)


#Calculating MSPE
MSPE4 = mean(abs((tail(pred3$mean,52)-test$y)/test$y))*100
MSPE4
