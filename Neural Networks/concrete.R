##Name- Shahaji Jadhav
##Prepare a model for strength of concrete data using Neural Networks 
##Response variable is Strength

#Read Data- concrete
concrete <- read.csv(file.choose())
View(concrete)
sum(is.na(concrete))
str(concrete)
summary(concrete)

#Data scalling
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
  
concrete_scaled <-as.data.frame(lapply(X = concrete, normalise))
View(concrete_scaled)

#Partitioning data
train <- concrete_scaled[1:721,]
test <- concrete_scaled[722:1030,]

library(neuralnet)
#Training with single hidden layer
train_1L <-neuralnet(formula =strength ~ cement + slag + ash + water + superplastic +coarseagg + fineagg + age,
                     data = train)
plot(train_1L)
#Testing model on test data
result_1L <- compute(train_1L,test)
strength_1L <- result_1L$net.result
cor(strength_1L,test$strength)#0.80

##Training with three hidden layer
train_3L <- neuralnet(formula =strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                      data = train, hidden = 3)
plot(train_3L)
result_3L <- compute(train_3L,test)
cor(result_3L$net.result,test$strength)#0.92

##Training with Five hidden layer
train_5L <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                      data = train, hidden = 5)
plot(train_5L)
result_5L <- compute(train_5L,test)
cor(result_5L$net.result,test$strength)#0.91

##Training with six hidden layer
train_6L <- neuralnet(formula =  strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                      data = train, hidden = 6)
plot(train_6L)
result_6L <- compute(train_6L,test)
cor(result_6L$net.result,test$strength)#0.933

##Training with Seven hidden layer
train_7L <- neuralnet(formula =  strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                      data = train, hidden = 7)
plot(train_7L)
result_7L <- compute(train_7L,test)
cor(result_7L$net.result,test$strength)#0.93


##AS we see COR value of 6 and 7 are nearly equal. bur 6 has marginaly more COR.
##so six layers is significunt.