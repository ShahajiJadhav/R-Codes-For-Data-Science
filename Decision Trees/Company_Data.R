##Name- Shahaji Jadhav  
##A cloth manufacturing company is interested to know about the segment 
##or attributes causes high sale.


Company_Data <- read.csv(file.choose(), stringsAsFactors = T)
View(Company_Data)
Company_Data <- Company_Data[c('Sales', 'CompPrice', 'Income', 'Advertising', 'Population', 'Age', 'Education','ShelveLoc','Urban','US')]
CD <- Company_Data
summary(CD)

#Data preprocessing
CD$Sales <- ifelse(CD$Sales<=10, 'Low', 'High')#Median : 7.490, considering 10 and above is high
CD$Sales <- factor(CD$Sales)
summary(CD)#Sales- High: 78 , Low :322
View(CD)
str(CD)
#Splitting data into test and train
library(caTools)
set.seed(123)
dataset <- sample.split(CD$Sales, SplitRatio = 0.75)
training_set <- subset(CD, dataset==TRUE )
testing_set <- subset(CD, dataset==FALSE)
View(testing_set)
str(testing_set)
#Feature scaling
training_set[2:7] <- scale(training_set[2:7])
testing_set[2:7] <- scale(testing_set[2:7])
View(training_set)

#Preparing Decision tree model
library(C50)
classifier = C5.0(Sales ~ . , training_set, trails= 50)

plot(classifier)
summary(classifier)

# Predicting the Test set results
y_pred = predict.C5.0(classifier, newdata = testing_set)
head(y_pred)
cm <- table(testing_set$Sales,y_pred);cm
accuracy <- sum(diag(cm)/sum(cm));accuracy#0.81


