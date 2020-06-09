#Reading Data
wc_at <- read.csv(file.choose(),header = TRUE,sep = ",", stringsAsFactors = TRUE)
wc_at <- wc_at[-1]
View(wc_at)
attach(wc_at)
names(wc_at)
plot(wc_at)
#Ploting Data
#Waist
plot(Waist)
plot(Waist, type = 'p',xlab ="Frequancy",ylab = 'Waist',  main = 'Waist')
boxplot(Waist)
boxplot(Waist,notch =T,outline = F, horizontal = T, col = "light blue")
hist(Waist)
hist(Waist,col = "blue", border = T, breaks = 10, axes = T, labels = T, density = 5)

#AT
plot(AT, xlab ="Frequancy",ylab = 'AT',  main = 'AT')
boxplot(AT)
hist(Waist, col = "pink", border = F, breaks =10, labels = T, main = "HIST AT")

plot(Waist, AT)
plot(AT, Waist)
