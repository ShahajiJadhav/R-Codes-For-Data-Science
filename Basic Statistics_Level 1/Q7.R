#Reading DScorea
Q7 <- read.csv(file.choose(),header = TRUE,sep = ",", stringsAsFactors = TRUE)
View(Q7)
Scoretach(Q7)
names(Q7)#"X"  "Points" "Score"  "Weigh" 
plot(Q7)

#Ploting
plot(Points)
plot(Points, type = 'o',xlab ="Frequancy",ylab = 'Points',  main = 'Points')
boxplot(Points)
boxplot(Points,notch =T, horizontal = T, col = "pink")
hist(Points)
hist(Points,col = "blue", border = T, breaks = 15, axes = T, labels = T, density = 5)

#Score
plot(Score, xlab ="Frequancy",ylab = 'Score',  main = 'Score')
boxplot(Score)
hist(Points, col = "light blue", border = F, breaks =10, labels = T, main = "HIST Score")

#Weigth
plot(Weigh)
boxplot(Weigh)

plot(Points, Score)
plot(X, Points, las= 2)

