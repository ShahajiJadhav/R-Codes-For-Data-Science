##Name- Shahaji Jadhav
#prepare a prediction model for predicting Price.

#Reading Data
ToyotaCorolla <- read.csv(file.choose())
#Consider only the below columns
Corolla<-ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
names(Corolla)[names(Corolla)=='Age_08_04'] <- 'Age'
View(Corolla)
head(Corolla)
str(Corolla)
summary(Corolla)

cor(Corolla)


# The Linear Model of interest
model.Corolla <- lm(Price~., data = Corolla) 
summary(model.Corolla)
#R-squared:  0.8638,	Adjusted R-squared:  0.863 

#Checking for infuencial data
library(MASS)
library(car)
stepAIC(model.Corolla)
plot(model.Corolla)
influence.measures(model.Corolla)
influenceIndexPlot(model.Corolla) 
influencePlot(model.Corolla)
#Removing those data points
Corolla <- Corolla[-c(222,81,602,961),]
colnames(Corolla)
#Price Age KM HP cc Doors Gears Quarterly_Tax Weight

model.Corolla <- lm(Price~Age+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = Corolla) 
summary(model.Corolla)
# R-squared:  0.8894,	Adjusted R-squared:  0.8888 , R-Squared value is improved.
#there are Gears and Quartely_Tax has slight weak impact, p value is 0.08,0.06 resp.

summary(lm(Price ~ Gears, data= Corolla))
#p>0.05, R-squared:  0.004018,	Adjusted R-squared:  0.003322 , Not enough significunt.

summary(lm(Price ~ Quarterly_Tax, data= Corolla))
#p<0.05, R-squared:  0.04765,	Adjusted R-squared:  0.04699 

model1 <- lm(Price~Age+KM+HP+cc+Doors+Gears+Quarterly_Tax, data = Corolla) 
summary(model1)
#R-squared:  0.8405,	Adjusted R-squared:  0.8397, CC does not show any significunce

model2 <- lm(Price~Age+KM+HP+cc+Doors+Gears+Weight, data = Corolla) 
summary(model2)
#R-squared:  0.8889,	Adjusted R-squared:  0.8883 

model3 <- lm(Price~Age+KM+HP+cc+Doors+Quarterly_Tax+Weight, data = Corolla) 
summary(model3)
#R-squared:  0.8889,	Adjusted R-squared:  0.8883

model4 <- lm(Price~Age+KM+HP+cc+Gears+Quarterly_Tax+Weight, data = Corolla) 
summary(model4)
#R-squared:  0.8882,	Adjusted R-squared:  0.8877

model5 <- lm(Price~Age+KM+HP+Doors+Gears+Quarterly_Tax+Weight, data = Corolla) 
summary(model5)
#R-squared:  0.8774,	Adjusted R-squared:  0.8768 , Quarterly_Tax has low impact

model6 <- lm(Price~Age+KM+HP+Weight, data = Corolla) 
summary(model6)
#R-squared:  0.8757,	Adjusted R-squared:  0.8754 

model7 <- lm(Price~Age+KM+cc+Doors+Gears+Quarterly_Tax+Weight, data = Corolla) 
summary(model7)
#R-squared:  0.877,	Adjusted R-squared:  0.8764 

model8 <- lm(Price~Age+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = Corolla) 
summary(model.Corolla)
#R-squared:  0.8894,	Adjusted R-squared:  0.8888 

model9 <- lm(Price~KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = Corolla) 
summary(model9)
#R-squared:0.731,	Adjusted R-squared:  0.7297

model10 <- lm(Price~Age+KM+HP+cc+Doors+Weight+I(Gears*Quarterly_Tax), data = Corolla) 
summary(model10)
#R-squared:  0.889,	Adjusted R-squared:  0.8885 

model11 <- lm(Price~Age+KM+HP+cc+Doors+Weight, data = Corolla) 
summary(model11)
#R-squared:  0.8883,	Adjusted R-squared:  0.8878


Models = c('model_Corolla','model1','model2','model3','model4','model5','model6','model7','model8',
             'model9','model10','model11')

Rsquare = c(summary(model.Corolla)$r.squared, summary(model1)$r.squared, summary(model2)$r.squared,
            summary(model3)$r.squared, summary(model4)$r.squared, summary(model5)$r.squared, summary(model6)$r.squared,
            summary(model7)$r.squared, summary(model8)$r.squared, summary(model9)$r.squared,
            summary(model10)$r.squared, summary(model11)$r.squared
            )

A_Rsquare = c(summary(model.Corolla)$adj.r.squared, summary(model1)$adj.r.squared, summary(model2)$adj.r.squared,
              summary(model3)$adj.r.squared, summary(model4)$adj.r.squared, summary(model5)$adj.r.squared,
              summary(model6)$adj.r.squared, summary(model7)$adj.r.squared, summary(model8)$adj.r.squared,
              summary(model9)$adj.r.squared, summary(model10)$adj.r.squared, summary(model11)$adj.r.squared)

Models_Comparison <- data.frame(Models, Rsquare, A_Rsquare)
Models_Comparison
max(Models_Comparison$Rsquare)#0.8894
max(Models_Comparison$A_Rsquare)#0.8887

#by looking at results, we can go ahead with model_Corolla which gives highest Adjusted R-Square Value
#SO we can go with that model



