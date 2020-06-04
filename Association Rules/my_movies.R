##Name-Shahaji Jadhav
##Prepare rules for the all the data sets 
##1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
##2) Change the minimum length in apriori algorithm
##3) Visulize the obtained rules using different plots 

my_movies <- read.csv(file.choose())
View(my_movies)
str(my_movies)
as.data.frame(my_movies)
View(my_movies)
hist(my_movies$Sixth.Sense)
#There are 167 products are there in citrus.Fruit 
arules1 <- apriori(as.matrix(my_movies), parameter = list(support=0.01,confidence=0.5))
arules1 #86

arules2 <- apriori(my_movies, parameter = list(support=0.005,confidence=0.6))
arules2 #148

arules3 <- apriori(my_movies, parameter = list(support=0.009,confidence=0.5))
arules3#101

arules4 <- apriori(my_movies, parameter = list(support=0.008,confidence=0.85))
arules4 #80

arules5 <- apriori(my_movies, parameter = list(support=0.0092,confidence=0.7))
arules5#79

arules6 <- apriori(my_movies, parameter = list(support=0.010,confidence=0.8))
arules6#68

arules7 <- apriori(my_movies, parameter = list(support=0.011,confidence=0.9))
arules7#49

arules8 <- apriori(my_movies, parameter = list(support=0.015,confidence=0.8))
arules8 #37

arules9 <- apriori(my_movies, parameter = list(support=0.020,confidence=0.3))
arules9# 22

arules10 <- apriori(my_movies, parameter = list(support=0.012,confidence=0.98))
arules10# 45 Rules

inspect(sort(arules10,by="lift")) # to view we use inspect

# Arules10 seem perfect for a grocery store having 45 rules
#Visualizing Rules
plot(arules10)
plot(arules10,method="grouped")