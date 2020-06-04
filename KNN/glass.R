##Name- Shahaji Jadhav
#Prepare a model for glass classification using KNN.

#Read File
glass <- read.csv(file.choose())
View(glass)
str(glass)
summary(glass)
table(glass$Type)
apply(is.na(glass),2,sum)

#Normalising Data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
glass1 <- glass[-10]
glass_Norm <- as.data.frame(lapply(X = glass1,FUN = normalize)) 
View(glass_Norm)

#Data Partition
training_Data <- glass_Norm[1:150,]
training_label <- glass[1:150,10];
testing_Data <- glass_Norm[151:214,]
test_label <- glass[151:214,10]

#Building Model
library(class)

glass_Predict <- knn(train = training_Data, test = testing_Data, cl=training_label, k=5)
View(glass_Predict)

##Evaluating model performance 
# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = test_label, y = glass_Predict,
           prop.chisq=FALSE)


#Improve Model
glass_Predict1 <- knn(train = training_Data, test = testing_Data, cl=training_label, k=14)
View(glass_Predict1)
CrossTable(x = test_label, y = glass_Predict1, prop.chisq=FALSE)
