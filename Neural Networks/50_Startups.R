##Name- Shahaji Jadhav
##Build a Neural Network model for 50_startups data to predict profit 
##Response variable is Profit

#Read Data- 50_Startups
startups <- read.csv(file.choose())
str(startups)
summary(startups)

##Reordering
startups <- startups[,c(4,1,2,3,5)]
View(startups)

#Data scalling
startups_scaled <-as.data.frame(scale(startups[-1]))
View(startups_scaled)

#Partitioning data
train <- startups_scaled[1:40,]
test <- startups_scaled[41:50,]

library(neuralnet)
#Training with single hidden layer
train_1L <-neuralnet(formula =Profit~Marketing.Spend+Administration+R.D.Spend,data = train)
plot(train_1L)
#Testing model on test data
result_1L <- compute(train_1L,test)
profit_1L <- result_1L$net.result
cor(profit_1L,test$Profit)#0.77

##Training with three hidden layer
train_3L <- neuralnet(formula = Profit~Marketing.Spend+Administration+R.D.Spend,data = train, hidden = 3)
plot(train_3L)
result_3L <- compute(train_3L,test)
cor(result_3L$net.result,test$Profit)#0.73

##Training with Five hidden layer
train_5L <- neuralnet(formula = Profit~Marketing.Spend+Administration+R.D.Spend,data = train, hidden = 5)
plot(train_5L)
result_5L <- compute(train_5L,test)
cor(result_5L$net.result,test$Profit)#0.70

##Training with six hidden layer
train_6L <- neuralnet(formula = Profit~Marketing.Spend+Administration+R.D.Spend,data = train, hidden = 6)
plot(train_6L)
result_6L <- compute(train_6L,test)
cor(result_6L$net.result,test$Profit)#0.44

##Training with Seven hidden layer
train_7L <- neuralnet(formula = Profit~Marketing.Spend+Administration+R.D.Spend,data = train, hidden = 7)
plot(train_7L)
result_7L <- compute(train_7L,test)
cor(result_7L$net.result,test$Profit)#0.07


##AS we see COR value significuntly descreased as NN layer increased from 5 to 6 and 6 to 7
##SO only one layer gives highest COR value. so one layer is significunt.