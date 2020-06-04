##Name- Shahaji JAdhav
##Use Random Forest to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

Fraud_check <- read.csv(file.choose(), stringsAsFactors = T)
View(Fraud_check)
CD <- Fraud_check[c("Taxable.Income", "City.Population","Work.Experience","Undergrad", "Marital.Status","Urban")]
#Data preprocessing
CD$Taxable.Income <- ifelse(CD$Taxable.Income<=30000, 'Risky','Good')#Median : 7.490, considering 10 and above is high
CD$Taxable.Income <- factor(CD$Taxable.Income)
summary(CD)#Taxable_Income- High: 78 , Low :322
View(CD)
str(CD)
class(CD)
#Create testing set and training set
library(caTools)
set.seed(123)
RF <- sample.split(CD$Taxable.Income,  SplitRatio = 0.75)
training_set <- subset(CD, RF==T)
testing_set  <- subset(CD, RF==F)

training_set[,c(2,3)] = scale(training_set[,c(2,3)])
testing_set[,c(2,3)] = scale(testing_set[,c(2,3)])
View(training_set)

#fitting data 
library(randomForest)
Classifier <- randomForest(Taxable.Income ~ . , data= training_set, ntree = 1500)
plot(Classifier)
Classifier$confusion
#Testing Data
FC_pred <- predict(Classifier, testing_set)
View(CD_pred)
cm <- table(testing_set$Taxable.Income , FC_pred);cm
Accuracy <- sum(diag(cm))/sum(cm);Accuracy#0.78
