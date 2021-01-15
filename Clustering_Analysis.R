print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(df[labels==i,c("CustomerID","Gender","Age","Income","Score")])
  }
}

df <- read.csv('Mall_Customers.csv')
names(df)[4] <- "Income"
names(df)[5] <- "Score"
sapply(df, class)
summary(df)

#1. EDA plots
library(ggplot2)
ggplot(df, mapping = aes(x=Gender)) + geom_bar()
ggplot(df, mapping = aes(x=Age)) + geom_histogram(binwidth = 0.3)
ggplot(df, mapping = aes(x=Score)) + geom_freqpoly()
ggplot(df, mapping = aes(x=Income)) + geom_freqpoly()
ggplot(df, mapping = aes(x=Gender, y=Age)) + geom_boxplot()
ggplot(df, mapping = aes(x=Gender, y=Income)) + geom_boxplot()
ggplot(df, aes(x=Age, y=Score, color=Income)) + geom_point()

#2. Male vs Female customers
ggplot(df, aes(x=as.factor(Gender), y=Score)) + geom_boxplot()

#3. Distance matrix
df$Gender <- as.numeric(df$Gender)
vars.to.use <- colnames(df)[-1]
pmatrix <- scale(df[,vars.to.use], center = TRUE, scale = TRUE)
d <- dist(pmatrix, method="euclidean")

#4. Hierarchical clustering
pfit1 <- hclust(d, method="ward.D")
plot(pfit1, labels=df$CustomerID)
rect.hclust(pfit1, k=5)

#5. WSS
library(factoextra) 
fviz_nbclust(df[,-1], FUN = hcut, method = "wss")

#6. Bootstrap evaluation
library(fpc)
cboot1 <- clusterboot(pmatrix,clustermethod=hclustCBI,method="ward.D", k=4)
groups1<-cboot1$result$partition
print_clusters(groups1, 4)
cboot1$bootmean #Jaccard coeff
cboot1$bootbrd # of dissolves

#7. K-means clustering
kmm <- kmeans(pmatrix, 4, nstart=100, iter.max=100)
groups <- kmm$cluster
print_clusters(groups, 4)
# Clusterboot with k-means
cboot2<-clusterboot(pmatrix, clustermethod=kmeansCBI,
                   runs=100,iter.max=100,
                   krange=4, seed=15555)
groups2 <- cboot2$result$partition
print_clusters(groups2, 4)
cboot2$bootmean
cboot2$bootbrd

#8.
ggplot(df, aes(x=Income, y=Score, color=Age)) + geom_point()
#principal component analysis and clusters plot
princ <- prcomp(pmatrix)
nComp <- 2
project <- predict(princ, newdata=pmatrix)[,1:nComp] #Rotate data in PC space
project.plus <- cbind(as.data.frame(project), 
                       cluster=as.factor(groups2), 
                       CustomerID=df$CustomerID)
ggplot(project.plus, aes(x=PC1, y=PC2)) + geom_point(aes(shape=cluster))