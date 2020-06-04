##Name- Shahaji Jadhav
##Prepare a classification model using Naive Bayes for salary data

#Reading Data
training_set <- read.csv(file.choose(),stringsAsFactors = T)
testing_set <- read.csv(file.choose(), stringsAsFactors = T)
View(testing_set)
str(testing_set)

#Data Preprocessing
#some of the columns are not required we will eliminate those
colnames(training_set)
training_set <- training_set[c("Salary","age","capitalgain","capitalloss","hoursperweek","workclass","sex" ,"occupation","education")]
testing_set <- testing_set[c("Salary","age","capitalgain","capitalloss","hoursperweek","workclass","sex" ,"occupation","education")]
View(testing_set)

#Building model
library(e1071)
classifier <- naiveBayes(Salary ~ . ,training_set)
classifier

##  Evaluating model performance ----
pred <- predict(classifier, testing_set[-1])
table(pred)

cm = table(testing_set$Salary, pred);cm
Accuracy <- sum(diag(cm))/sum(cm);Accuracy#0.78



