##Name-Shahaji Jadhav
#Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 
#Draw the inferences from the clusters obtained.

library(readxl)
airlines <- read_xlsx(file.choose(), sheet="data")
View(airlines)
names(airlines)[12] <- 'Awards'
str(airlines)
summary(airlines)
apply(is.na(airlines),2,sum)
airlines_data <- airlines[-1]
View(airlines_data)

#Data Normalisation
Norm_airlines1 <- scale(airlines_data)
head(Norm_airlines1)

#Distance Calculation
Distance <- dist(Norm_airlines1, method = 'euclidean')

#Obtaining opltimal Cluster number
wss = (nrow(Norm_airlines1)-1)*sum (apply(Norm_airlines1, 2, var))
for (i in 1:15) wss[i] = sum(kmeans(Norm_airlines1, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", sub = "K-Means Clustering Scree-Plot")
# Shows 10 no of clusters

#Using Kselection
library(kselection)
k <- kselection(Norm_airlines1, parallel = TRUE, k_threshold = 0.9)
k #SUgested 10 clusters

#Fit model- Hierarchical clustering
#Clustering - Complete Linkage
fit.complete <- hclust(Distance, method = 'complete' )
plot(fit.complete, hang = -1, main = ' Complete Linkage')
rect.hclust(fit.complete, k = 10,border = 'red')

Clust.No <- as.matrix(cutree(tree = fit.complete, h = 10))
plot(Clust.No)

final1 <- data.frame(Clust.No,airlines)
View(final1)


#Clustering - Single Linkage
fit.single <- hclust(Distance, method = 'single' )
plot(fit.single, hang = -1, main = ' Single Linkage')
rect.hclust(fit.single, k = 10,border = 'red')

Clust.No2 <- as.matrix(cutree(tree = fit.complete, h = 10))
plot(Clust.No)

final2 <- data.frame(Clust.No2,airlines)
View(final2)


#Clustering- Kmeans
k_clustering <- kmeans(Distance,centers = 10)
K_final <- data.frame(k_clustering$cluster, airlines)
head(K_final)
table(K_final$k_clustering.cluster)
#1   2   3   4   5   6   7   8   9  10 
#33 512 358 856 472 288 150 678 595  57 
