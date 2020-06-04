##Name-Shahaji Jadhav
###predict weight gained using calories consumed
calories_consumed <- read.csv(file.choose())
View(calories_consumed)
colnames(calories_consumed)
colnames(calories_consumed) <- c('Weight_Gained_Grams','Calories_Consumed')
str(calories_consumed)
summary(calories_consumed)
head(calories_consumed)
attach(calories_consumed)

#PLOTTING
plot(Calories_Consumed)
boxplot(Calories_Consumed)
plot(Weight_Gained_Grams)
boxplot(Weight_Gained_Grams)
plot(calories_consumed)

#Normality Test
qqnorm(Weight_Gained_Grams)
qqline(Weight_Gained_Grams)

qqnorm(Calories_Consumed)
qqline(Calories_Consumed)
qqplot(Weight_Gained_Grams,Calories_Consumed)

cor(Calories_Consumed,Weight_Gained_Grams)#0.94

#Rsquares 
# Simple Linear Regression Rsquare
Rsquare1 <- lm(Calories_Consumed~Weight_Gained_Grams, calories_consumed)
summary(Rsquare1)$r.squared#0.89
rmse_1 <- sqrt(mean(Rsquare1$residuals^2))
rmse_1#232
p1 <- as.data.frame(predict(Rsquare1,interval = 'predict'))
head(p1)
plot(Rsquare1)

# Response Var-Square Root Linear Regression Rsquare
Rsquare2 <- lm(Calories_Consumed~sqrt(Weight_Gained_Grams), calories_consumed)
summary(Rsquare2)$r.squared#0.91
rmse_2 <- sqrt(mean(Rsquare2$residuals^2))
rmse_2#212
p2 <- as.data.frame(predict(Rsquare2,interval = 'predict'))
head(p2)
plot(Rsquare2)

##there is 1nd, 7th, 8th, 13th obs seems out of bound 
calories_consumed[c(1,7,8,9,13),]
calories_consumed$Calories_Consumed[c(1,7,9,13)] <- c(1600,1750,2580,2674)
calories_consumed$Weight_Gained_Grams[c(8,13)] <- c(150,320)
# Simple Linear Regression Rsquare
Rsquare1 <- lm(Calories_Consumed~Weight_Gained_Grams, calories_consumed)
summary(Rsquare1)$r.squared#0.91
rmse_1 <- sqrt(mean(Rsquare1$residuals^2))
rmse_1#202
p1 <- as.data.frame(predict(Rsquare1,interval = 'predict'))
head(p1)
plot(Rsquare1)


# Response Var-Square Root Linear Regression Rsquare
Rsquare2 <- lm(Calories_Consumed~sqrt(Weight_Gained_Grams), calories_consumed)
summary(Rsquare2)$r.squared#0.93
rmse_2 <- sqrt(mean(Rsquare2$residuals^2))
rmse_2#174
p2 <- as.data.frame(predict(Rsquare2,interval = 'predict'))
head(p2)
plot(Rsquare2)

# Response Var-Square Root Linear Regression Rsquare
Rsquare3 <- lm(sqrt(Calories_Consumed)~Weight_Gained_Grams, calories_consumed)
summary(Rsquare3)$r.squared#0.88
rmse_3 <- sqrt(mean(Rsquare3$residuals^2))
rmse_3#2.2
p3 <- as.data.frame(predict(Rsquare3,interval = 'predict'))
head(p3)
plot(Rsquare3)

# Logrithamic Rsquare
Rsquare4 <- lm(Calories_Consumed~log(Weight_Gained_Grams), calories_consumed)
summary(Rsquare3)$r.squared#0.88
rmse_4 <- sqrt(mean(Rsquare4$residuals^2))
rmse_4#176
p4 <- as.data.frame(predict(Rsquare4,interval = 'predict'))
head(p4)
plot(Rsquare4)

#Both Side Log Rsquare
Rsquare5 <- lm(log(Calories_Consumed)~log(Weight_Gained_Grams),calories_consumed)
summary(Rsquare5)$r.squared#0.94
rmse_5 <- sqrt(mean(Rsquare5$residuals^2))
rmse_5#0.06
p5 <- as.data.frame(predict(Rsquare5,interval = 'predict'))
head(p5)
plot(Rsquare5)

# Polynomial Rsquare with 2 degree (quadratic Rsquare)
Rsquare6 <- lm(Calories_Consumed~Weight_Gained_Grams+I(Weight_Gained_Grams*Weight_Gained_Grams), calories_consumed)
summary(Rsquare6)$r.squared#0.91
rmse_6 <- sqrt(mean(Rsquare6$residuals^2))
rmse_6#195
p6 <- as.data.frame(predict(Rsquare6,interval = 'predict'))
head(p6)
plot(Rsquare6)

#for Rsquare5 is highest R square value (0.94) and low RMSE (0.06) value.
#so Rsquare5 is better regression madel to go
Predicted_Values <- exp(p5)
Predicted_Values
