##Name- Shahaji Jadhav
#Forecast the CocaCola price.
#Prepare a document for each model explaining how many dummy variables you have created and RMSE value for each model
#Reading data- CocaCola_Sales_Rawdata
library(readxl)
CC_Sales <- read_excel(file.choose())
View(CC_Sales)
str(CC_Sales)
#Library required
library(forecast)
library(fpp)
library(smooth) 
library(tseries)

#Converting dataset into time series object
ts_sales<-ts(CC_Sales$Sales,frequency = 4, start=c(42))
str(ts_sales)
View(tssales)
plot(ts_sales) 
# Visualization shows that it has level, trend and Additive seasonality

#Dividing entire data into training and testing data 
#Since data is in quarterly manner we will keep 4 quarters for testing dataset.
training_set<-ts_sales[1:38]
test_set<-ts_sales[39:42] 

# converting time series object
training_set<-ts(training_set,frequency = 4)
test_set<-ts(test_set,frequency = 4)

##USING HoltWinters function 

# Without optimum values 
hw_na<-HoltWinters(training_set,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test_set)*100
hwna_mape#9.09

# with default values for alpha = 0.2, beta = 0.15, gamma = 0.05 
# since time series data has level,trend and seasonality 
hw_abg<-HoltWinters(training_set,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
plot(forecast(hw_abg,h=4))
# by looking at the plot the characters of forecasted values are closely following historical data
hwabg_mape<-MAPE(hwabg_pred$fit,test_set)*100
hwabg_mape#3.58


############## USING ses,holt,hw functions ##########################
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 

ses_a<-ses(train,alpha = 0.2) # 
ses_a
sesa_pred<-data.frame(predict(ses_a,h=4))
plot(forecast(ses_a,n.ahead=4))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100
sesa_mape

# with alpha = 0.2, beta = 0.1

holt_ab<-holt(train,alpha = 0.2,beta = 0.15)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=4))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100
holtab_mape

# with alpha = 0.2, beta = 0.1, gamma = 0.05 

hw_abg_new<-hw(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 4))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100
hwabg_mape_new

# With out optimum values 

# simple exponential method

ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 4))
sesna_pred
plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100
sesna_mape

# Holts winter method 

holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=4))
holtnab_pred
plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100
holtnab_mape

# Holts winter Exponential method

hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=4))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100
hwnabg_mape

df_mapes_new<-data.frame(c("sesa_mape","holtab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesa_mape,holtab_mape,hwabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters 

new_model <- hw(amts,alpha = NULL,beta = NULL,gamma = NULL)

plot(forecast(new_model,h=4))

# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,h=4))