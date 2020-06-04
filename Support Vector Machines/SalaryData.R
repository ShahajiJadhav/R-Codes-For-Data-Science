##Name- Shahaji Jadhav
##classify the Size_Categorie using SVM
#Dataset- Salary_Data

Salary_Data <- read.csv(file.choose())
View(Salary_Data)

#Re-ordering dataset
SalaryData <- Salary_Data[c(14,1,10,11,12,4,2,5,6,9,13)]
View(SalaryData)
str(SalaryData)
table(SalaryData$Salary)

SalaryData$Salary <- factor(SalaryData$Salary, levels = c('>50K', '<=50K'), labels = c(0,1))

#Converting to factor
SalaryData$Salary <- as.factor(SalaryData$Salary)
SalaryData[7:11] <- lapply(SalaryData[7:11], factor)
str(SalaryData)

#Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(SalaryData$Salary, SplitRatio = 0.75)
train_set = subset(SalaryData, split == TRUE)
test_set = subset(SalaryData, split == FALSE)

# Feature Scaling
train_set[2:6] = scale(train_set[2:6])
test_set[2:6] = scale(test_set[2:6])
View(test_set)
#Building Model
library(kernlab)
model_vanilladot <- ksvm(Salary ~ . , data= train_set, kernal= 'vanilladot')
model_vanilladot
pred_vanilla <- predict(model_vanilladot, test_set[-1])
pred_vanilla
CM = table(test_set$Salary, pred_vanilla);CM
Accuracy <- sum(diag(CM))/sum(CM);Accuracy#0.85

#Model besseldot
model_besseldot <- ksvm(Salary ~ . , data= train_set, kernal= 'besseldot')
model_besseldot
pred_besseldot <- predict(model_besseldot, test_set[-1])
pred_besseldot
CM = table(test_set$Salary, pred_besseldot);CM
Accuracy <- sum(diag(CM))/sum(CM);Accuracy#085


#Most of the model gives same result with 85% accuracy
plot(model_vanilladot)
