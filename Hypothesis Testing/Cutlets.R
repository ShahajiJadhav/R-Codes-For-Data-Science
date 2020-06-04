#Hypothesis Testing Exercise
#A F&B manager wants to determine whether there is any significant difference in the diameter of the cutlet
#between two units. A randomly selected sample of cutlets was collected from both units and measured 
#Analyze the data and draw inferences at 5% significance level. 
#Please state the assumptions and tests that you carried out to check validity of the assumptions.

#Ho- There is no significance difference between two units.
#H1-There is significance difference between two units.

#Read Data
cutlets <-read.csv(file.choose()) 
View(cutlets)
str(cutlets)
attach(cutlets)
summary(cutlets)

##Both Variables are continuous 
#Checking For Normality 
shapiro.test(Unit.A)#P value=0.32>0.05. follow normal distribution
shapiro.test(Unit.B)#P value=0.52>0.05 follow normal distribution
#Both units follows normal distribution

#Checking for Variance
var.test(Unit.A, Unit.B)
#P-Value= 0.31>0.05. Variance of both units are equal
#Since X has two category we will do t-test
t.test(Unit.A,Unit.B,alternative = 'two.sided',conf.level = 0.95, correct = TRUE)
#P-Value= 0.47>0.05, Unit A and B has equal Mean
# Ho is accepted
#As per test,  There is no significance difference between two units.

