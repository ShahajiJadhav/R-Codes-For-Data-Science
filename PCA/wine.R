#Perform Principal component analysis and perform clustering using first3 principal component scores 
#(both heirarchial and k mean clustering(scree plot or elbow curve)
#obtain optimum number of clusters and check whether we have obtained same number of clusters with 
#the original data (class column we have ignored at the begining who shows it has 3 clusters)df
                              
wine <- read.csv(file.choose(), sep = ',')
head(wine)
wine_data <- wine[-1]
str(wine_data)
summary(wine_data)

#PCA
PCA_Wine <- princomp(wine_data, cor = T, scores = T, covmat = NULL)
summary(PCA_Wine)

loadings(PCA_Wine)
plot(PCA_Wine)
#After plotting 8 components contains 94% of information
#Collecting PCA Score
head(PCA_Wine$score)

PCA_scores <- PCA_Wine$scores[,1:8]
View(PCA_scores)

#Normalising PCA Score
Norm_PCAScore <- scale(PCA_scores)
Dist_Norm_PCA <- dist(Norm_PCAScore, method = 'euclidean')

#forming Cluster No.
Clusterdata <- hclust(Dist_Norm_PCA,method = 'complete',)
plot(Clusterdata)

ClusterNO <- as.matrix(cutree(Clusterdata,7))#7 cluster Formed
Hclust_Wine <- data.frame(ClusterNO,wine)
head(Hclust_Wine)
table(Hclust_Wine$Type,Hclust_Wine$ClusterNO)
##  1  2  3  4  5  6  7
#1 37 19  3  0  0  0  0
#2 35 15  2  7  3  8  1
#3  0 34  6  6  0  0  2


#k mean clustering
#elbow curve
library(factoextra)
set.seed(50)
fviz_nbclust(Norm_PCAScore, kmeans, method = "wss")
#By looking at graph we can opt for 7 or 8 no. of cluster
set.seed(30)
kmeans_Cluster <- kmeans(Dist_Norm_PCA, centers = 7)
wine_kmeans <- data.frame(kmeans_Cluster$cluster, wine)
Ktable <-  table(wine_kmeans$Type,wine_kmeans$kmeans_Cluster.cluster)
Ktable
colnames(wine_kmeans)
aggregate(wine_kmeans[-2],by = list(wine_kmeans$kmeans_Cluster.cluster),mean)#performing function by list

