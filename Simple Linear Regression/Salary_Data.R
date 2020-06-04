##Name-Shahaji Jadhav
###Build a prediction model for YearsExperience
Salary_Data <- read.csv(file.choose())
View(Salary_Data)
str(Salary_Data)
summary(Salary_Data)
head(Salary_Data)
attach(Salary_Data)

#PLOTTING
plot(Salary)
boxplot(Salary)
plot(YearsExperience)
boxplot(YearsExperience)
plot(Salary_Data)


#Normality Test
qqnorm(YearsExperience)
qqline(YearsExperience)

qqnorm(Salary)
qqline(Salary)
qqplot(YearsExperience,Salary)

cor(Salary,YearsExperience)#0.97

#Rsquares 
# Simple Linear Regression Rsquare
Rsquare1 <- lm(Salary~YearsExperience, Salary_Data)
summary(Rsquare1)$r.squared#0.95
rmse_1 <- sqrt(mean(Rsquare1$residuals^2))
rmse_1#5592
p1 <- as.data.frame(predict(Rsquare1,interval = 'predict'))
head(p1)
plot(Rsquare1)


##there is 2nd, 18th, 20th, 24th,9th obs seems outlier 
Salary_Data[c(2,18,9,20,24),]
Salary_Data$Salary[c(2,18,9,20,24)] <- c(44055,82359,62341,88555,108550)

# Simple Linear Regression Rsquare
Rsquare1 <- lm(Salary~YearsExperience, Salary_Data)
summary(Rsquare1)$r.squared#0.97
rmse_1 <- sqrt(mean(Rsquare1$residuals^2))
rmse_1#4764
p1 <- as.data.frame(predict(Rsquare1,interval = 'predict'))
head(p1)
plot(Rsquare1)


# Response Var-Square Root Linear Regression Rsquare
Rsquare2 <- lm(Salary~sqrt(YearsExperience), Salary_Data)
summary(Rsquare2)$r.squared#0.94
rmse_2 <- sqrt(mean(Rsquare2$residuals^2))
rmse_2#6480
p2 <- as.data.frame(predict(Rsquare2,interval = 'predict'))
head(p2)
plot(Rsquare2)

# Response Var-Square Root Linear Regression Rsquare
Rsquare3 <- lm(sqrt(Salary)~YearsExperience, Salary_Data)
summary(Rsquare3)$r.squared#0.96
rmse_3 <- sqrt(mean(Rsquare3$residuals^2))
rmse_3#9.5
p3 <- as.data.frame(predict(Rsquare3,interval = 'predict'))
head(p3)
plot(Rsquare3)

# Logrithamic Rsquare
Rsquare4 <- lm(Salary~log(YearsExperience), Salary_Data)
summary(Rsquare3)$r.squared#0.96
rmse_4 <- sqrt(mean(Rsquare4$residuals^2))
rmse_4#9888
p4 <- as.data.frame(predict(Rsquare4,interval = 'predict'))
head(p4)
plot(Rsquare4)

#Both Side Log Rsquare
Rsquare5 <- lm(log(Salary)~log(YearsExperience),Salary_Data)
summary(Rsquare5)$r.squared#0.91
rmse_5 <- sqrt(mean(Rsquare5$residuals^2))
rmse_5#0.10
p5 <- as.data.frame(predict(Rsquare5,interval = 'predict'))
head(p5)
plot(Rsquare5)

# Polynomial Rsquare with 2 degree (quadratic Rsquare)
Rsquare6 <- lm(Salary~YearsExperience+I(YearsExperience*YearsExperience), Salary_Data)
summary(Rsquare6)$r.squared#0.97
rmse_6 <- sqrt(mean(Rsquare6$residuals^2))
rmse_6#4756
p6 <- as.data.frame(predict(Rsquare6,interval = 'predict'))
head(p6)
plot(Rsquare6)

#for Rsquare3 is highest R square value (0.96) and low RMSE (9.5) value.
#so Rsquare3 is better regression madel to go
Square <- (p3)*(p3)
Square
