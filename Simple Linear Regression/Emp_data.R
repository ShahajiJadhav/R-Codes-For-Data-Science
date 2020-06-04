##Name-Shahaji Jadhav
###Build a prediction Rsquare for Churn_out_rate 
Emp_data <- read.csv(file.choose())
View(Emp_data)
str(Emp_data)
summary(Emp_data)
head(Emp_data)
attach(Emp_data)

#PLOTTING
plot(Churn_out_rate)
boxplot(Churn_out_rate)
plot(Salary_hike)
boxplot(Salary_hike)
plot(Emp_data)


#Normality Test
qqnorm(Salary_hike)
qqline(Salary_hike)

qqnorm(Churn_out_rate)
qqline(Churn_out_rate)
qqplot(Salary_hike,Churn_out_rate)

cor(Churn_out_rate,Salary_hike)#0.-91

#Rsquares 
# Simple Linear Regression Rsquare
Rsquare1 <- lm(Churn_out_rate~Salary_hike, Emp_data)
summary(Rsquare1)$r.squared#0.83
rmse_1 <- sqrt(mean(Rsquare1$residuals^2))
rmse_1#3.99
p1 <- as.data.frame(predict(Rsquare1,interval = 'predict'))
head(p1)
plot(Rsquare1)

# Response Var-Square Root Linear Regression Rsquare
Rsquare2 <- lm(Churn_out_rate~sqrt(Salary_hike), Emp_data)
summary(Rsquare2)$r.squared#0.84
rmse_2 <- sqrt(mean(Rsquare2$residuals^2))
rmse_2#3.89
p2 <- as.data.frame(predict(Rsquare2,interval = 'predict'))
head(p2)
plot(Rsquare2)

# Response Var-Square Root Linear Regression Rsquare
Rsquare3 <- lm(sqrt(Churn_out_rate)~Salary_hike, Emp_data)
summary(Rsquare3)$r.squared#0.85
rmse_3 <- sqrt(mean(Rsquare3$residuals^2))
rmse_3#0.21
p3 <- as.data.frame(predict(Rsquare3,interval = 'predict'))
head(p3)
plot(Rsquare3)

# Logrithamic Rsquare
Rsquare4 <- lm(Churn_out_rate~log(Salary_hike), Emp_data)
summary(Rsquare3)$r.squared#0.85
rmse_4 <- sqrt(mean(Rsquare4$residuals^2))
rmse_4#3.78
p4 <- as.data.frame(predict(Rsquare4,interval = 'predict'))
head(p4)
plot(Rsquare4)

#Both Side Log Rsquare
Rsquare5 <- lm(log(Churn_out_rate)~log(Salary_hike),Emp_data)
summary(Rsquare5)$r.squared#0.88
rmse_5 <- sqrt(mean(Rsquare5$residuals^2))
rmse_5#0.04
p5 <- as.data.frame(predict(Rsquare5,interval = 'predict'))
head(p5)
plot(Rsquare5)

# Polynomial Rsquare with 2 degree (quadratic Rsquare)
Rsquare6 <- lm(Churn_out_rate~Salary_hike+I(Salary_hike*Salary_hike), Emp_data)
summary(Rsquare6)$r.squared#0.97
rmse_6 <- sqrt(mean(Rsquare6$residuals^2))
rmse_6#1.57
p6 <- as.data.frame(predict(Rsquare6,interval = 'predict'))
head(p6)
plot(Rsquare6)

#for Rsquare 5 is highest R square value (0.88) and lowest rmse(0.04) value.
#so Rsquare 5 is better regression madel to go
lo <- exp(p5)
lo
