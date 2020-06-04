##Hypothesis Testing Exercise
##  Sales of products in four different regions is tabulated for males and females. 
#Find if male-female buyer rations are similar across regions.

#Ho- All proportions are equal
#Ha- Not all Proportions are equal

BuyerRatio <- read.csv(file.choose())
View(BuyerRatio)
colnames(BuyerRatio)#"Observed.Values" "East" "West""North""South" 
attach(BuyerRatio)
#Here Y is descrete in 2 and X is descrete in 3 So we will go with Chi-Square test
BR <- stack(BuyerRatio,select=c(East=East,West=West,North=North,South=South ))
chisq.test(BR$ind, BR$values)
#P-Value=0.29 > 0.05. Accept null hypothesis

#As per test, Sales of products in four different regions for males and females are equal.
