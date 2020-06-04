##Name-Shahaji Jadhav
###predict delivery time using sorting time 
d_time <- read.csv(file.choose())
View(d_time)
str(d_time)
summary(d_time)
head(d_time)
attach(d_time)

#PLOTTING
plot(Delivery.Time)
boxplot(Delivery.Time)
plot(Sorting.Time)
boxplot(Sorting.Time)
plot(d_time)


#Normality Test
qqnorm(Sorting.Time)
qqline(Sorting.Time)

qqnorm(Delivery.Time)
qqline(Delivery.Time)
qqplot(Sorting.Time,Delivery.Time)

cor(Delivery.Time,Sorting.Time)#0.82

#Models 
# Simple Linear Regression model
Rsquare1 <- lm(Delivery.Time~Sorting.Time, d_time)
summary(Rsquare1)$r.squared#0.68
rmse_1 <- sqrt(mean(Rsquare1$residuals^2))
rmse_1#2.79
p1 <- as.data.frame(predict(Rsquare1,interval = 'predict'))
head(p1)
plot(Rsquare1)

##there is 9th, 21st,5th obs seems outlier 
d_time$Sorting.Time[c(9,21)] <- c(7,9)
d_time$Delivery.Time[5] <- 21.25

##After replacing values
# Simple Linear Regression model
Rsquare1 <- lm(Delivery.Time~Sorting.Time, d_time)
summary(Rsquare1)$r.squared#0.85
rmse_1 <- sqrt(mean(Rsquare1$residuals^2))
rmse_1#1.63
p1 <- as.data.frame(predict(Rsquare1,interval = 'predict'))
head(p1)
plot(Rsquare1)


# Response Var-Square Root Linear Regression model
Rsquare2 <- lm(Delivery.Time~sqrt(Sorting.Time), d_time)
summary(Rsquare2)$r.squared#0.86
rmse_2 <- sqrt(mean(Rsquare2$residuals^2))
rmse_2#1.56
p2 <- as.data.frame(predict(Rsquare2,interval = 'predict'))
head(p2)
plot(Rsquare2)

# Response Var-Square Root Linear Regression model
Rsquare3 <- lm(sqrt(Delivery.Time)~Sorting.Time, d_time)
summary(Rsquare3)$r.squared#0.84
rmse_3 <- sqrt(mean(Rsquare3$residuals^2))
rmse_3#0.21
p3 <- as.data.frame(predict(Rsquare3,interval = 'predict'))
head(p3)
plot(Rsquare3)

# Logrithamic Model
Rsquare4 <- lm(Delivery.Time~log(Sorting.Time), d_time)
summary(Rsquare3)$r.squared#0.84
rmse_4 <- sqrt(mean(Rsquare4$residuals^2))
rmse_4#1.60
p4 <- as.data.frame(predict(Rsquare4,interval = 'predict'))
head(p4)
plot(Rsquare4)

#Both Side Log Model
Rsquare5 <- lm(log(Delivery.Time)~log(Sorting.Time),d_time)
summary(Rsquare5)$r.squared#0.88
rmse_5 <- sqrt(mean(Rsquare5$residuals^2))
rmse_5#0.09
p5 <- as.data.frame(predict(Rsquare5,interval = 'predict'))
head(p5)
plot(Rsquare5)

# Polynomial model with 2 degree (quadratic model)
Rsquare6 <- lm(Delivery.Time~Sorting.Time+I(Sorting.Time*Sorting.Time), d_time)
summary(Rsquare6)$r.squared#0.86
rmse_6 <- sqrt(mean(Rsquare6$residuals^2))
rmse_6#1.56
p6 <- as.data.frame(predict(Rsquare6,interval = 'predict'))
head(p6)
plot(Rsquare6)

cor((predict_data$fit),Delivery.Time)
#Correlation value for model 5 is highest R square value (0.88) and lowest rmse(0.09) value.
#so model 5 is better regression madel to go
lo <- exp(p5)
lo
