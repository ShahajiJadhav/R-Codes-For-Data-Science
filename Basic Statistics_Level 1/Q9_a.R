#Reading Ddista
Q9 <- read.csv(file.choose(),header = TRUE,sep = ",", stringsAsFactors = TRUE)
Q9 <- Q9[-1]
View(Q9)
names(Q9)#"speed" "dist" 
plot(Q9)
attach(Q9)
#Ploting
plot(speed)
plot(speed, type = 'o',xlab ="Frequancy",ylab = 'speed',  main = 'speed')
boxplot(speed)
boxplot(speed, horizontal = T, col = "orange")
hist(speed)
hist(speed, col = "blue", border = T, breaks = 15,labels = T, density = 7)

#dist
plot(dist, xlab ="Frequancy",ylab = 'dist',  main = 'dist')
boxplot(dist)
hist(dist, border = F, breaks =10, labels = T, main = "HIST dist")


plot( dist, speed)
