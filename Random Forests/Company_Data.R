##Name- Shahaji JAdhav
##A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
#Read Data- Company_Data

Company_Data <- read.csv(file.choose(), stringsAsFactors = T)
View(Company_Data)
CD <- Company_Data[c('Sales', 'CompPrice', 'Income', 'Advertising', 'Population', 'Age', 'Education','ShelveLoc','Urban','US')]

#Data preprocessing
CD$Sales <- ifelse(CD$Sales<=10, 'Low', 'High')#Median : 7.490, considering 10 and above is high
CD$Sales <- factor(CD$Sales)
summary(CD)#Sales- High: 78 , Low :322
View(CD)
str(CD)

#Create testing set and training set
library(caTools)
set.seed(123)
RF <- sample.split(CD$Sales,  SplitRatio = 0.75)
training_set <- subset(CD, RF==T)
testing_set  <- subset(CD, RF==F)

training_set[2:7] = scale(training_set[2:7])
testing_set[2:7] = scale(testing_set[2:7])
View(training_set)

#fitting data 
library(randomForest)
Classifier <- randomForest(Sales ~ ., training_set, ntree = 20)
plot(Classifier)
Classifier$confusion
#Testing Data
CD_pred <- predict(Classifier, testing_set)
View(CD_pred)
cm <- table(testing_set$Sales, CD_pred);cm
Accuracy <- sum(diag(cm))/sum(cm);Accuracy#0.80
varImpPlot(Classifier)
#ShelveLoc highest importance and that causes highest impact on sale following Income and Age.