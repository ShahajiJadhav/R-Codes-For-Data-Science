##Hypothesis Testing Exercise
##Fantaloons Sales managers commented that % of males versus females walking in to the store differ based on 
#day of the week. Analyze the data and determine whether there is evidence at 5 % significance 
#level to support this hypothesis.State the assumptions and tests that you carried out to check validity of the assumptions.

#Ho- % of males versus females walking in to the store differ based on weekdays and weekends
#H1- % of males versus females walking in to the store same on weekdays and weekends
Faltoons <- read.csv(file.choose(), stringsAsFactors = T)
View(Faltoons)
#Here Y and X are discrete in two categories
table(Faltoons$Weekdays,Faltoons$Weekend)
prop.test(x=c(66, 47),n=c(167, 120),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
#P-Value= 0.95>0.05. So we will accept null hyphothesis
#So % of males versus females walking in to the store differ based on weekdays and weekends as per test at 95% confidence.









