##Name- Shahaji Jadhav
#Prepare a model for zoo classification using KNN.

#Read File
zoo <- read.csv(file.choose())
View(zoo)
str(zoo)
table(zoo$type)
sum(is.na(zoo))
zoo1 <- zoo[,c(-1,-18)]
zoo1

#Normalising Data
zoo_Norm <- scale(zoo1)
View(zoo_Norm)

#Data Partition
training_Data <- zoo_Norm[1:75,]
training_label <- zoo[1:75,18];
testing_Data <- zoo_Norm[76:101,]
test_label <- zoo[76:101,18]

#Building Model
library(class)

zoo_Predict <- knn(train = training_Data, test = testing_Data, cl=training_label, k=7)
View(zoo_Predict)

##Evaluating model performance 
# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = test_label, y = zoo_Predict, prop.chisq=FALSE)


#Improve Model
zoo_Predict1 <- knn(train = training_Data, test = testing_Data, cl=training_label, k=10)
View(zoo_Predict1)
CrossTable(x = test_label, y = zoo_Predict1, prop.chisq=FALSE)
