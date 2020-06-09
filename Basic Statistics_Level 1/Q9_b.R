#Reading DWTa
Q9 <- read.csv(file.choose(),header = TRUE,sep = ",", stringsAsFactors = TRUE)
Q9 <- Q9[-1]
View(Q9)
names(Q9)#"SP" "WT"
plot(Q9)
attach(Q9)
#Ploting
plot(SP)
plot(SP, type = 'o',xlab ="Frequancy",ylab = 'SP',  main = 'SP')
boxplot(SP)
boxplot(SP, horizontal = T, col = "orange")
hist(SP)
hist(SP, col = "blue", border = T, breaks = 15,labels = T, density = 7)

#WT
plot(WT, xlab ="Frequancy",ylab = 'WT',  main = 'WT')
boxplot(WT)
hist(WT, border = F, breaks =10, labels = T, main = "HIST WT")


plot( WT, SP)
