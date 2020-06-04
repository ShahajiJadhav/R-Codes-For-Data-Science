##Name- Shahaji Jadhav
##PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS
##Response variable is area

#Read Data- forestfires
forestfires <- read.csv(file.choose(), stringsAsFactors = T)
View(forestfires)
str(forestfires)

dataset <- forestfires[,-c(1,2)]
head(dataset)
#Reodering dataset
dataset <- dataset[,c(9,1:8,10:29)]
View(dataset)
summary(dataset)
colnames(dataset)
#Recoding categorical variable as factor
dataset$size_category <- as.numeric(factor(dataset$size_category, levels= c('small','large' ), labels= c(3,4)))
dataset$size_category <- ifelse(dataset$size_category=='small', 0,1)

str(dataset)
View(dataset)
table(dataset$size_category)
#Partitioning data
library(caTools)
set.seed(50)
data_split <- sample.split(dataset$area, SplitRatio = 0.7)
training_set <- subset(dataset, data_split==T)
testing_set <- subset(dataset, data_split==F)
View(testing_set)

#Need to normalise 1st to 9th colomns
training_set <- scale(training_set)
testing_set <- scale(testing_set)
View(testing_set)


#Training with single hidden layer
library(neuralnet)
train_1L <-neuralnet(area~ .,
                     data = training_set, hidden = 4)

plot(train_1L)
train_1L$net.result

#Testing model on test data
result_1L <- compute(train_1L, testing_set)
result_1L$net.result

cor(result_1L$net.result, testing_set$area)





