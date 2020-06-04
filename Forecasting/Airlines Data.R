##Name- Shahaji Jadhav
#Forecast Airlines Passengers data set
#Prepare a document for each model explaining how many dummy variables you have created and RMSE value for each model
library(readxl)
library(forecast)
Airlines_Data <- read_xlsx(file.choose())
View(Airlines_Data)
str(Airlines_Data)
summary(Airlines_Data)

#Its a yearly data so need to create 12 dummy veriable
Dummy <- data.frame(outer(rep(month.abb, length = nrow(Airlines_Data)), month.abb,"==")+0)
View(Dummy)
colnames(Dummy) <- month.abb         
Combined_Airlines_data <- cbind(Airlines_Data,Dummy)
View(Combined_Airlines_data)

#Preprocession-Formation of t,tsquare,log of Passanger
Combined_Airlines_data['t'] <- seq(nrow(Combined_Airlines_data))
Combined_Airlines_data['tsquare'] <- Combined_Airlines_data['t']*Combined_Airlines_data['t']
Combined_Airlines_data['Log_Passanger'] <- log(Combined_Airlines_data["Passengers"])
View(Combined_Airlines_data)
attach(Combined_Airlines_data)

#Partitioning Dataset
Training_Data <- Combined_Airlines_data[1:84,]
Test_Data <- Combined_Airlines_data[85:96,]
View(Training_Data)
View(Test_Data)

##Linear Model
Linear_Model <- lm(Passengers~t, Training_Data)
summary(Linear_Model)#Rsquare-0.79
Linear_Predict <- data.frame(predict(Linear_Model, Test_Data,interval = 'predict'))
rmse_Linear <- sqrt(mean(Test_Data$Passengers-Linear_Predict$fit)^2)
rmse_Linear#28

##Exponencial Model
Expo_Model <- lm(Log_Passanger~t,Training_Data)
Expo_Predict <- data.frame(predict(Expo_Model,Test_Data, interval = "predict"))
rmse_Expo <- sqrt(mean(Test_Data$Passengers-Expo_Predict$fit)^2)
rmse_Expo #322.47

##Quadratic Model
Quad_Model <- lm(Passengers~t+tsquare,Training_Data)
Quad_Predict <- data.frame(predict(Quad_Model, Test_Data, interval = 'predict'))
rmse_Quad <- sqrt(mean(Test_Data$Passengers-Quad_Predict$fit)^2)
rmse_Quad #15.97

## Additive Seasonality
add_sea_Model <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = Training_Data)
add_sea_Predict <- data.frame(predict(add_sea_Model,Test_Data, interval ='predict'))
rmse_add_sea <- sqrt(mean(Test_Data$Passengers-add_sea_Predict$fit)^2)
rmse_add_sea #130.90

## Additive Seasonality with Quadratic
quad_add_sea_Model <- lm(Passengers~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, Training_Data)
quad_add_sea_Predict <- data.frame(predict(quad_add_sea_Model, Test_Data, interval='predict'))
rmse_qaud_add_sea <- sqrt(mean(Test_Data$Passengers-quad_add_sea_Predict$fit)^2)
rmse_qaud_add_sea #14.04

## Multiplicative Seasonality 
multi_sea_Model <- lm(Log_Passanger~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, Training_Data)
multi_sea_Predict <- data.frame(predict(multi_sea_Model,Test_Data , interval='predict'))
rmse_multi_sea <- sqrt(mean((Test_Data$Passengers-exp(multi_sea_Predict$fit))^2))
rmse_multi_sea #140.06

# Preparing table on model and it's RMSE values 

RMSE_Table <- data.frame(c('Linear_Model','Expo_Model','Quad_Model','add_sea_Model','quad_add_sea_Model','multi_sea_Model'),
                         c(rmse_Linear, rmse_Expo,rmse_Quad,rmse_add_sea,rmse_qaud_add_sea,rmse_multi_sea))
colnames(RMSE_Table) <- c("Model","RMSE")
View(RMSE_Table)

## Combining Training & test data to build Additive seasonality using Quadratic Trend 

quad_add_sea_Model_final <- lm(Passengers ~ t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, Combined_Airlines_data)
summary(quad_add_sea_Model_final)#R-squared:  0.9645,	Adjusted R-squared:  0.9588 
plot(quad_add_sea_Model_final)

## Predicting new data 
pred_new <- as.data.frame(predict(quad_add_sea_Model_final, newdata = Test_Data, interval = 'predict'))
View(pred_new)


acf(quad_add_sea_Model_final$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

Arima1 <- arima(quad_add_sea_Model_final$residuals, order = c(1,0,0))
Arima1$residuals

ARerrors <- Arima1$residuals

acf(ARerrors, lag.max = 10)

# predicting next 12 months errors using arima( order =c(1,0,0))

library(forecast)
Future_Errors <- data.frame(forecast(ARerrors, h = 12))
View(Future_Errors)
Future_Errors1 <- Future_Errors$Point.Forecast

# predicted values for new data + future error values 

Predicted_New_Values <- pred_new + Future_Errors1
View(Predicted_New_Values)

write.csv(Predicted_New_Values, file = "Predicted_New_Values.csv", row.names = F)
getwd()

