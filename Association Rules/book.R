##Name-Shahaji Jadhav
##Prepare rules for the all the data sets 
##1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
##2) Change the minimum length in apriori algorithm
##3) Visulize the obtained rules using different plots 
library("arules")
library("arulesViz")
#Read File
book <- read.csv(file.choose())
View(book)
str(book)
#There are 11 types of books available in database
library(plyr)
No.Books <- sapply(book, margin=2,sum)
No.Books
#Child books=848, cook books=864, geo books=554, Doit book=566, Ital Art book= 99

#Generating Rules
rules1 <-  apriori(as.matrix(book),parameter=list(support=0.05,confidence=0.4))
rules1 #252 Rules.it is considerably more count 
plot(rules1,method = "group")

rules2 <-  apriori(as.matrix(book),parameter=list(support=0.08,confidence=0.6))
rules2 #62 Rules
plot(rules2,method = "group")

rules3 <-  apriori(as.matrix(book),parameter=list(support=0.03,confidence=0.6))
rules3 #248 Rules
plot(rules3,method = "group")

rules4 <-  apriori(as.matrix(book),parameter=list(support=0.06,confidence=0.6))
rules4 #85 Rules
plot(rules4,method = "group")

rules5 <-  apriori(as.matrix(book),parameter=list(support=0.075,confidence=0.8))
rules5 #32 Rules

rules5 <-  apriori(as.matrix(book),parameter=list(support=0.075,confidence=0.82))
rules5 #19 Rules


#Plotting
plot(rules5,method="group")
inspect((sort(rules5,by="lift"))) # to view we use inspect 

#Rule no 5 seems better for the book store