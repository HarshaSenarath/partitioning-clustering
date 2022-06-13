# loading required libraries
library(readxl)
library(factoextra)
library(NbClust)

# reading whitewine_v2 data set
whitewine.v2 <- read_excel("Whitewine_v2.xlsx")

# summary of the data set before pre-processing
summary(whitewine.v2)

# box plot of data set before pre-processing
boxplot(whitewine.v2[,-12])

# iqr method to identify outliers
outliers = c()

for (i in 1:11) {
  quartile1 <- quantile(whitewine.v2[[i]], probs = .25)
  quartile3 <- quantile(whitewine.v2[[i]], probs = .75)
  
  # calculating interquartile range in each column 
  iqr <- quartile3 - quartile1
  
  # storing the outliers which are located less or more than 1.5 * iqr range
  b.outliers = which(whitewine.v2[[i]] < quartile1 - (iqr * 1.5))
  t.outliers = which(whitewine.v2[[i]] > quartile3 + (iqr * 1.5))
  
  # storing the outliers that are not already inside outlier vector
  outliers = c(outliers, b.outliers[!b.outliers %in% outliers])
  outliers = c(outliers, t.outliers[!t.outliers %in% outliers])
}

# removing the identified outliers from the dataset
clean.whitewine.v2 = as.data.frame(whitewine.v2[-outliers, ])

# scaling features
scaled.features = as.data.frame((scale(clean.whitewine.v2[,-12])))

# data set after pre-processing
scaled.whitewine.v2 <- as.data.frame(cbind(scaled.features, clean.whitewine.v2['quality']))

# summary of the data set after pre-processing
summary(scaled.whitewine.v2)

# box plot of data set after pre-processing
boxplot(scaled.whitewine.v2[,-12])

# NbClust
# euclidean
set.seed(42)

nbc1 = NbClust(scaled.whitewine.v2[,-12], distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# manhattan
set.seed(42)

nbc2 = NbClust(scaled.whitewine.v2[,-12], distance = "manhattan", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# maximum
set.seed(42)
nbc3 = NbClust(scaled.whitewine.v2[,-12], distance = "maximum", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# elbow method
set.seed(42)
fviz_nbclust(scaled.whitewine.v2[,-12], kmeans, method = "wss") + geom_vline(xintercept = 2, linetype = 2)

# silhouette method
set.seed(42)
fviz_nbclust(scaled.whitewine.v2[,-12], kmeans, method = "silhouette")

# k = 2
set.seed(42)
kmeans2 <- kmeans(scaled.whitewine.v2[,-12], centers = 2, nstart = 25)

# cluster plot k = 2
fviz_cluster(kmeans2, data = scaled.whitewine.v2[,-12],
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

# CM table k = 2
table(kmeans2$cluster, scaled.whitewine.v2$quality)

# k = 3
set.seed(42)
kmeans3 <- kmeans(scaled.whitewine.v2[,-12], centers = 3, nstart = 25)

# cluster plot k = 3
fviz_cluster(kmeans3, data = scaled.whitewine.v2[,-12],
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

# CM table k = 3
table(kmeans3$cluster, scaled.whitewine.v2$quality)

# k = 4
set.seed(42)
kmeans4 <- kmeans(scaled.whitewine.v2[,-12], centers = 4, nstart = 25)

# cluster plot k = 4
fviz_cluster(kmeans4, data = scaled.whitewine.v2[,-12],
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

# CM table k = 4
table(kmeans4$cluster, scaled.whitewine.v2$quality)

# pca
pca = prcomp(clean.whitewine.v2[,-12], center = TRUE, scale = TRUE)

# pca summary
summary(pca)

# creating new data set
transformed.whitewine.v2 = as.data.frame(-pca$x[,9:11])

# applying k means to the new transformed data set
set.seed(42)
kmeans.pca <- kmeans(transformed.whitewine.v2, centers = 2, nstart = 25)

# cluster plot
fviz_cluster(kmeans.pca, data = transformed.whitewine.v2,
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())