##Name- Shahaji Jadhav  
##Use decision trees to prepare a model on fraud data 
##treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

Fraud_Data <- read.csv(file.choose(), stringsAsFactors = T)
View(Fraud_Data)
str(Fraud_Data)
FD <- Fraud_Data
summary(FD)

#Data preprocessing
FD$Taxable.Income <- ifelse(FD$Taxable.Income<=30000, 'Risky', 'Good')
View(FD)
FD$Taxable.Income <- factor(FD$Taxable.Income)
summary(FD)
FD <- FD[c(3,4,5,1,2,6)]
str(FD)
#Splitting data into test and train
library(caTools)
set.seed(123)
dataset <- sample.split(FD$Taxable.Income,SplitRatio = 0.7)
training_set <- subset(FD, dataset==TRUE)
testing_set <- subset(FD, dataset==FALSE)
View(testing_set)
str(testing_set)
#Feature scaling
training_set[2:3] <- scale(training_set[2:3])
testing_set[2:3] <- scale(testing_set[2:3])
View(testing_set)
#Preparing Decision tree model
library(C50)
classifier = C5.0(Taxable.Income ~ .,
                   data = training_set, trails= 10)

# Predicting the Test set results
y_pred = predict.C5.0(classifier, newdata = testing_set)
y_pred
cm <- table(testing_set$Taxable.Income, y_pred);cm
accuracy <- sum(diag(cm)/sum(cm));accuracy


