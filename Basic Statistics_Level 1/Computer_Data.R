#Reading Data
Computer_Data <- read.csv(file.choose(),header = TRUE,sep = ",", stringsAsFactors = TRUE)
Computer_Data <- Computer_Data[-1]
View(Computer_Data)
attach(Computer_Data)
names(Computer_Data)
plot(Computer_Data)
#Ploting Data
#screeneed
plot(screeneed)
plot(screeneed, type = 'p',xlab ="Frequancy",ylab = 'screeneed',  main = 'screeneed')
boxplot(screeneed)
boxplot(screeneed,notch =T,outline = F, range = 6000, horizontal = T, col = "red")
hist(screeneed)
hist(screeneed,col = "blue", border = T, breaks = 10, axes = T, labels = T, density = 5)
scatter(screeneed)
#hd
plot(hd, xlab ="Frequancy",ylab = 'hd',  main = 'hd')
boxplot(hd)
hist(screeneed, col = "red", border = F, breaks =10, labels = T, main = "HIST hd")

#ads
plot(ads)
plot(ads, type = 'p')
boxplot(ads, outline = T, col = 'skyblue', horizontal = T)
hist(ads)
hist(ads,col = "orange", border = F, breaks = 15,labels = T, main = "HIST ads")

#screen
plot(screen)
boxplot(screen)
hist(screen)
hist(screen, col = "yellow", border = F, labels = T,breaks = 5, main = "HIST screen")

plot(trend~I(hd+cd))
plot(premium, ads)
plot(premium, ads+price)
plot(premium, price+screen)
plot(premium, I(ram+screen))
plot(multi, price)
plot(ram,x= price)
boxplot(ram)
