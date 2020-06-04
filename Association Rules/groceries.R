##Name-Shahaji Jadhav
##Prepare rules for the all the data sets 
##1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
##2) Change the minimum length in apriori algorithm
##3) Visulize the obtained rules using different plots 

groceries <- read.csv(file.choose())
View(groceries)
str(groceries)
as.data.frame(groceries)
View(groceries)

#There are 167 products are there in citrus.Fruit 
arules1 <- apriori(groceries, parameter = list(support=0.008,confidence=0.8))
arules1 #86

arules2 <- apriori(groceries, parameter = list(support=0.005,confidence=0.6))
arules2 #148

arules3 <- apriori(groceries, parameter = list(support=0.009,confidence=0.5))
arules3#101

arules4 <- apriori(groceries, parameter = list(support=0.008,confidence=0.85))
arules4 #80

arules5 <- apriori(groceries, parameter = list(support=0.0092,confidence=0.7))
arules5#79

arules6 <- apriori(groceries, parameter = list(support=0.010,confidence=0.8))
arules6#68

arules7 <- apriori(groceries, parameter = list(support=0.011,confidence=0.9))
arules7#49

arules8 <- apriori(groceries, parameter = list(support=0.015,confidence=0.8))
arules8 #37

arules9 <- apriori(groceries, parameter = list(support=0.020,confidence=0.3))
arules9# 22

arules10 <- apriori(groceries, parameter = list(support=0.012,confidence=0.98))
arules10# 45 Rules

inspect(sort(arules10,by="lift")) # to view we use inspect

# Arules10 seem perfect for a grocery store having 45 rules
#Visualizing Rules
plot(arules10)
plot(arules10,method="grouped")
