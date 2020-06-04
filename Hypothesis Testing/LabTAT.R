##Hypothesis Testing Exercise
##A hospital wants to determine whether there is any difference in the average Turn Around Time (TAT)
#of reports of the laboratories on their preferred list. They collected a random sample and recorded TAT for reports of 4 laboratories. 
#TAT is defined as sample collected to report dispatch. Analyze the data and determine whether there is 
#any difference in average TAT among the different laboratories at 5% significance level.

#Ho- average Turn Around Time (TAT) =>"Laboratory.1" = "Laboratory.2" ="Laboratory.3" ="Laboratory.4"
#Ha- average Turn Around Time (TAT) (any one of them or all) =>"Laboratory.1" != "Laboratory.2" !="Laboratory.3" !="Laboratory.4"

lab <- read.csv(file.choose())
View(lab)
colnames(lab)# "Laboratory.1" "Laboratory.2" "Laboratory.3" "Laboratory.4"
attach(lab)
#test for normality
shapiro.test(Laboratory.1)$p.value#0.55>0.05, Normally Distributed
shapiro.test(Laboratory.2)$p.value#0.86>0.05, Normally Distributed
shapiro.test(Laboratory.3)$p.value#0.42>0.05, Normally Distributed
shapiro.test(Laboratory.4)$p.value#0.66>0.05, Normally Distributed
detach(lab)

#Since data are in 4 columns we have to stack
lab_stacked <- stack(lab)
View(lab_stacked)
#Variance test for X is descrete in 3 or more and Y is continuous.
bartlett.test(lab_stacked$values, lab_stacked$ind)
#p-value=> 0.10 > 0.05. variances are equal
#since variance are equal we will do anova test.
summary(aov(values~ind, data = lab_stacked))
#p-value => 2e-16 < 0.05. So we will reject the Null hypothesis

# average Turn Around Time (TAT) for any one of them or all of them are not equal.








