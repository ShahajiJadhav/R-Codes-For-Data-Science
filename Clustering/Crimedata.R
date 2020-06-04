##Name-Shahaji Jadhav
##Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

#HClustering
crimedata <- read.csv(file.choose())
View(crimedata)  
str(crimedata)
summary(crimedata)
apply(is.na(crimedata),2,sum)

names(crimedata)[1] <- 'State.Name'
head(crimedata)

crimedata1 <- crimedata[,-1]
head(crimedata1)

#Normalise Data
norm.crime <- scale(crimedata1) 
head(norm.crime)

#Distance Calculation
distance <- dist(norm.crime, method = 'euclidean')
#table(distance)

#Clustering - Comlete Linkage
fit.complete <- hclust(distance, method = 'complete')
plot(fit.complete, hang = -1, main = ' Complete Linkage')
rect.hclust(fit.complete, k = 4,border = 'red')

Clust.No <- as.matrix(cutree(tree = fit.complete, h = 4))
plot(Clust.No)

final1 <- data.frame(crimedata,Clust.No)
#Reordering Colomns
final1 <- final1[,c(6,1,2,3,4,5)]
head(final1)
final1[2] <- as.factor(final1$State.Name)

with(final1,plot(x = State.Name,y = Rape, las=2))

aggregate(final1[,3:6], by=list(final1$Clust.No), FUN=sum)

aggregate(final1[,3:6], by=list(final1$State.Name), FUN=sum)



#Single Linkage
#Clustering - Comlete Linkage
fit.single <- hclust(distance, method = 'single')
plot(fit.single, hang = -1, sub = ' Single Linkage')
rect.hclust(fit.single, k = 6,border = 'red')

Clust.No2 <- as.matrix(cutree(tree = fit.single, k = 6))
(Clust.No2)

final2 <- data.frame(crimedata,Clust.No2)
head(final2)
#Reordering Colomns
final2 <- final2[,c(6,1,2,3,4,5)]
head(final2)
final2[2] <- as.factor(final2$State.Name)

with(final2,plot(x = State.Name,y = Rape, las=2))

aggregate(final2[,3:6], by=list(final2$Clust.No2), FUN=sum)
