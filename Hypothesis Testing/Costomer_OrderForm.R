##Hypothesis Testing Exercise
##TeleCall uses 4 centers around the globe to process customer order forms. 
#They audit a certain %  of the customer order forms. 
#Any error in order form renders it defective and has to be reworked before processing.  
#The manager wants to check whether the defective %  varies by centre. 
#Please analyze the data at 5% significance level and help the manager draw appropriate inferences

#Ho- defective % => "Phillippines"="Indonesia"="Malta"="India" 
#Ha- defective => any of country  "Phillippines" !="Indonesia" != "Malta" != "India" 

Cust_OF<-read.csv(file.choose()) 
View(Cust_OF) # countries are in their own columns; so we need to stack the data 
stacked_Cust_OF<-stack(Cust_OF)
attach(stacked_Cust_OF)
View(stacked_Cust_OF)

table(stacked_Cust_OF$ind,stacked_Cust_OF$values)
#Here Y and X are descret in 3 or more category. So we wil perform Chi-Square test
chisq.test(table(stacked_Cust_OF$ind,stacked_Cust_OF$values))
#P-Value => 0.27 > 0.05. So we will accept null hypothesis


#So the defective % does not varies by centre accross globe.



