##Name- Shahaji Jadhav
##Whether the client has subscribed a term deposit or not 
#Reading Data- BAnk
bank= read.csv(file.choose(), sep = ';')
View(bank)#45211 obs. 17 var
summary(bank)

#Changing dependantvariabl name
colnames(bank)[17]= 'Subscribed'
table(bank$Subscribed)

#Droping unwanted variables that are-
Bank_Final = subset(bank, select = -c(job,day,month,duration,contact,campaign,pdays,previous))
head(Bank_Final)
colnames(Bank_Final)
#"age" "job" "marital" "education" "default" "balance" "housing" "loan" "poutcome" "Subscribed"

# Encoding the categorical variables as factors
Bank_Final$poutcome = factor(Bank_Final$poutcome,
                                      levels = c('failure','success','other','unknown'),
                                      labels = c(1, 2, 3, 4))

Bank_Final$default = factor(Bank_Final$default,
                                   levels = c('no', 'yes'),
                                   labels = c(0, 1))

Bank_Final$housing = factor(Bank_Final$housing,
                                       levels = c('no', 'yes'),
                                       labels = c(0, 1))
Bank_Final$loan = factor(Bank_Final$loan,
                                       levels = c('no', 'yes'),
                                       labels = c(0, 1))

Bank_Final$Subscribed <- factor(Bank_Final$Subscribed)
                                
Bank_Final$marital <- factor(Bank_Final$marital,
                                levels = c('divorced', 'married', 'single'),
                                labels = c(1, 2, 3))
Bank_Final$education <- factor(Bank_Final$education,
                                  levels = c('primary', 'secondary', 'tertiary','unknown'),
                                  labels = c(1, 2, 3, 4))
Bank_Final$Subscribed <- factor(Bank_Final$Subscribed,
                                levels = c('no', 'yes'),
                                labels = c(0, 1))

View(Bank_Final)
summary(Bank_Final)
str(Bank_Final)
# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(Bank_Final$Subscribed, SplitRatio = 0.8)
training_set = subset(Bank_Final, split == TRUE)
test_set = subset(Bank_Final, split == FALSE)

#Scalling Dataset
training_set[c(1,5)] <- scale(training_set[c(1,5)])
View(training_set)
test_set[c(1,5)] <- scale(test_set[c(1,5)])
View(test_set)

#building logistic model
model <- glm(Subscribed ~ ., data = Bank_Final, family = 'binomial')
model
pred <- predict(model, newdata = test_set[-9], type = 'response')
y_pred = ifelse(pred > 0.5, 1, 0)

cm = table(test_set[, 7], y_pred > 0.5)
cm
Accuracy<-sum(diag(cm)/sum(cm))#0.82
Accuracy
plot(model)
