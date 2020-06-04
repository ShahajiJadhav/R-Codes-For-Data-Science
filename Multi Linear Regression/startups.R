##Name- Shahaji Jadhav
#Prepare a prediction model for profit of 50_startups data.Do transformations for getting better 
#predictions of profit and make a table containing R^2 value for each prepared model.

#Reading Data
startups <- read.csv(file.choose())
View(startups)
head(startups)
str(startups)
summary(startups)
attach(startups)

#Reordering the colomn
startups <- startups[,c(4,5,1,2,3)]
head(startups)
names(startups)[names(startups)=='R.D.Spend'] <- 'RnD_Spend'
names(startups)[names(startups)=='Marketing.Spend'] <- 'Marketing_Spend'

#EDA
#Checking Normality
plot(RnD_Spend)
qqnorm(RnD_Spend)
qqline(RnD_Spend)
hist(RnD_Spend)
boxplot(RnD_Spend)

plot(Administration)
qqnorm(Administration)
qqline(Administration)
hist(Administration)
boxplot(Administration)

plot(Marketing_Spend)
qqnorm(Marketing_Spend)
qqline(Marketing_Spend)
hist(Marketing_Spend)
boxplot(Marketing_Spend)

plot(Profit)
qqnorm(Profit)
qqline(Profit)
hist(Profit)
boxplot(Profit)

#Finding Correlation Values
head(startups)
cor(Profit, RnD_Spend)#97, Strongly correlated
cor(Profit, Marketing_Spend)#74, Strongly correlated
cor(Profit, Administration)#20, weakly correlated
cor(startups[-1])

#Converting states into factor since its categorical in nature
startups$State <- factor(startups$State,
                         levels =c("New York" , "California","Florida"),
                         labels = c(1,2,3))

# The Linear Model of interest
model.startups <- lm(Profit~., data = startups) 
summary(model.startups)#Rsq- 0.950,ARsq- 0.945

#Checking for infuencial data
library(MASS)
library(car)
stepAIC(model.startups)
plot(model.startups)
influence.measures(model.startups)
influenceIndexPlot(model.startups) 
influencePlot(model.startups)
#Removing those data points
startups <- startups[-c(46,49,50),]

attach(startups)
#Chacking importance of each variable
RnD<-lm(Profit~RnD_Spend)
summary(RnD)
#P<0.05, Rsq- 0.958

adm <- lm(Profit~Administration)
summary(adm)
#p>0.05,Rsq- 0.01,Not at all significunt

mkt <- lm(Profit~Marketing_Spend)
summary(mkt)
#p<0.05, Rsq- 0.49

model_All <-lm(Profit~RnD_Spend+Administration+Marketing_Spend+State,data = startups ) 
summary(model_All)
#R-squared:  0.9645,	Adjusted R-squared:  0.9602
#only RnD_Spend is significunt.

model1 <-lm(Profit~RnD_Spend+Administration+Marketing_Spend,data = startups ) 
summary(model1)
#R-squared:0.9641,	Adjusted R-squared:  0.9616 

model2 <-lm(Profit~RnD_Spend+Marketing_Spend,data = startups ) 
summary(model2)
#p<0.05 R-squared:  0.9622,	Adjusted R-squared:  0.9605 

model3 <-lm(Profit~RnD_Spend+Administration,data = startups ) 
summary(model3)
#p<0.05, R-squared:  0.9622,	Adjusted R-squared:  0.9605 

model4 <-lm(Profit~Administration+Marketing_Spend,data = startups ) 
summary(model4)
#R-squared:  0.5438,	Adjusted R-squared:  0.523, Very low R-squared SO model 4 is not significunt.

Rsquare_Models <- data.frame(Models = c('model_All','model1','model2','model3','model4'),
                             Rsquare = c(0.9645, 0.9641, 0.9622, 0.9622, 0.5438),
                             A_Rsquare = c(0.9602, 0.9616, 0.9605, 0.9605, 0.523))
Rsquare_Models
#by looking at results, we can go ahead with model1 which gives highest Adjusted R-Square Value
