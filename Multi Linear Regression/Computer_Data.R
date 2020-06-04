##Name- Shahaji Jadhav
#Prepare a prediction model for the Price of the computer

#Reading Data
Computer_Data1 <- read.csv(file.choose())
View(Computer_Data)
Computer_Data <- Computer_Data1[-1]
head(Computer_Data)
str(Computer_Data)
summary(Computer_Data)
colnames(Computer_Data)#10 Varibles
#"price" "speed" "hd" "ram" "screen" "ads" "trend" "cd" "multi" "premium"
#Reordering the colomn
Computer_Data <- Computer_Data[,c(1,2,3,4,5,9,10,6,7,8)]
head(Computer_Data)

#Converting states into factor since its categorical in nature
head(as.factor(Computer_Data$cd))
Computer_Data$cd <- factor(Computer_Data$cd,
                              levels =c("no" , "yes"),
                              labels = c(1,2))
head(as.factor(Computer_Data$multi))
Computer_Data$multi <- factor(Computer_Data$multi,
                              levels =c("no" , "yes"),
                              labels = c(1,2))
head(as.factor(Computer_Data$premium))
Computer_Data$premium <- factor(Computer_Data$premium,
                              levels =c("no" , "yes"),
                              labels = c(1,2))

str(Computer_Data)
summary(Computer_Data)
cor(Computer_Data[-c(8,9,10)])

# The Linear Model of interest
model.Computer_Data <- lm(price~., data = Computer_Data) 
plot(model.Computer_Data)
summary(model.Computer_Data)
#R-squared:  0.7756,	Adjusted R-squared:  0.7752 

#Checking for infuencial data
library(MASS)
library(car)
influence.measures(model.Computer_Data)
influenceIndexPlot(model.Computer_Data) 
influencePlot(model.Computer_Data)
#Removing those data points
Computer_Data <- Computer_Data[-c(20,1441,1701,4478,3784),]

model.Computer_Data <- lm(price~., data = Computer_Data) 
summary(model.Computer_Data)
#R-squared:  0.7779,	Adjusted R-squared:  0.7776 
#it seems all variable impacts price

