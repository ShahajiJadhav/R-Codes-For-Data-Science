#Reading Data
cars <- read.csv(file.choose(),header = TRUE,sep = ",", stringsAsFactors = TRUE)
View(cars)
attach(cars)
names(cars)

#Ploting Data
#HP
plot(HP, type = 'l')
plot(HP, type = 'p')
plot(HP, type = 's',ylab = "HP")
plot(HP, type = 'o',xlab ="Frequancy",ylab = 'HP',  main = 'Boxplot HP')
boxplot(HP)
boxplot(HP,range = 110)
boxplot(HP,outline = F)
boxplot(HP,notch =T,outline = F)
boxplot(HP,varwidth = T)
hist(HP)
hist(HP,breaks = 10)
hist(HP,density = 5)
hist(HP,col = T,border = F)
hist(HP, axes = F)
hist(HP,labels = T, breaks = 10)
hist(HP,col = "blue", border = F, breaks = 9)

#MPG
plot(MPG)
boxplot(MPG)
hist(MPG)
hist(HP,col = "red", border = F, breaks =7, labels = T, main = "HIST MPG")

#VOL
plot(VOL)
plot(VOL, type = 'h')
boxplot(VOL)
boxplot(VOL,notch =T, outline = F, col = 'skyblue')
hist(VOL)
hist(VOL,col = "orange", border = F, breaks = 9,labels = T, main = "HIST VOL")

#SP
plot(SP)
plot(SP, type = 'b')
boxplot(SP)
boxplot(SP, notch =T, outline = T)
hist(SP)
hist(VOL,col = "yellow", border = F, labels = T, main = "HIST SP")

#WT
plot(WT)
plot(WT, type = 'p')
boxplot(WT)
boxplot(WT, notch =T, outline = T)
hist(WT)
hist(WT,col = "red", border = F,breaks = 5,labels = T, main = "HIST WT")


























































