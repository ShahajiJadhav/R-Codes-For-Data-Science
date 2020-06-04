##Name- Shahaji Jadhav
##Forecast the CocaCola prices and Airlines Sales data set. 
##Prepare a document for each model explaining 
library(forecast)
PlasticSales <- read.csv(file.choose())
View(PlasticSales)
str(PlasticSales)
summary(PlasticSales)

#Its a yearly data so need to create 12 dummy veriable
Dummy <- data.frame(outer(rep(month.abb, length = nrow(PlasticSales)), month.abb,"==")+0)
colnames(Dummy) <- month.abb         
Combined_PlasticSales <- cbind(PlasticSales,Dummy)
View(Combined_PlasticSales)

#Preprocession-Formation of t,tsquare,log of Passanger
Combined_PlasticSales['t'] <- seq(nrow(Combined_PlasticSales))
Combined_PlasticSales['tsquare'] <- Combined_PlasticSales['t']*Combined_PlasticSales['t']
Combined_PlasticSales['Log_Passanger'] <- log(Combined_PlasticSales["Sales"])
View(Combined_PlasticSales)
attach(Combined_PlasticSales)

#Partitioning Dataset
Training_Data <- Combined_PlasticSales[1:48,]
Test_Data <- Combined_PlasticSales[49:60,]
View(Training_Data)
View(Test_Data)

##Linear Model
Linear_Model <- lm(Sales~t, Training_Data)
summary(Linear_Model)#Rsquare-0.33
Linear_Predict <- data.frame(predict(Linear_Model, Test_Data,interval = 'predict'))
rmse_Linear <- sqrt(mean(Test_Data$Sales-Linear_Predict$fit)^2)
rmse_Linear#116.34

##Exponencial Model
Expo_Model <- lm(Log_Passanger~t,Training_Data)
Expo_Predict <- data.frame(predict(Expo_Model,Test_Data, interval = "predict"))
rmse_Expo <- sqrt(mean(Test_Data$Sales-Expo_Predict$fit)^2)
rmse_Expo #1315.8

##Quadratic Model
Quad_Model <- lm(Sales~t+tsquare,Training_Data)
Quad_Predict <- data.frame(predict(Quad_Model, Test_Data, interval = 'predict'))
rmse_Quad <- sqrt(mean(Test_Data$Sales-Quad_Predict$fit)^2)
rmse_Quad #183.51

## Additive Seasonality
add_sea_Model <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = Training_Data)
add_sea_Predict <- data.frame(predict(add_sea_Model,Test_Data, interval ='predict'))
rmse_add_sea <- sqrt(mean(Test_Data$Sales-add_sea_Predict$fit)^2)
rmse_add_sea #200.89

## Additive Seasonality with Quadratic
quad_add_sea_Model <- lm(Sales~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, Training_Data)
quad_add_sea_Predict <- data.frame(predict(quad_add_sea_Model, Test_Data, interval='predict'))
rmse_qaud_add_sea <- sqrt(mean(Test_Data$Sales-quad_add_sea_Predict$fit)^2)
rmse_qaud_add_sea #161.84

## Multiplicative Seasonality 
multi_sea_Model <- lm(Log_Passanger~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, Training_Data)
multi_sea_Predict <- data.frame(predict(multi_sea_Model,Test_Data , interval='predict'))
rmse_multi_sea <- sqrt(mean((Test_Data$Sales-exp(multi_sea_Predict$fit))^2))
rmse_multi_sea #239.65

# Preparing table on model and it's RMSE values 
RMSE_Table <- data.frame(c('Linear_Model','Expo_Model','Quad_Model','add_sea_Model','quad_add_sea_Model','multi_sea_Model'),
                         c(rmse_Linear, rmse_Expo,rmse_Quad,rmse_add_sea,rmse_qaud_add_sea,rmse_multi_sea))
colnames(RMSE_Table) <- c("Model","RMSE")
View(RMSE_Table)

## Combining Training & test data to build Additive seasonality using Quadratic Trend 
LinearModel_final <-lm(Sales~t, Training_Data)
summary(quad_add_sea_Model_final) ##R-squared:  0.9435,	Adjusted R-squared:  0.9275 

## Predicting new data 
pred_new <- as.data.frame(predict(LinearModel_final, newdata = Test_Data, interval = 'predict'))
View(pred_new)


acf(quad_add_sea_Model_final$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

Arima1 <- arima(LinearModel_final$residuals, order = c(1,0,0))
Arima1$residuals

acf(ARerrors, lag.max = 10)

# predicting next 12 months errors using arima( order =c(1,0,0))

library(forecast)
Future_Errors <- data.frame(forecast(Arima1$residuals, h = 12))
View(Future_Errors)
Future_Errors1 <- Future_Errors$Point.Forecast

# predicted values for new data + future error values 

Predicted_New_Values <- pred_new + Future_Errors1
View(Predicted_New_Values)

write.csv(Predicted_New_Values, file = "Predicted_New_Values.csv", row.names = F)
getwd()

