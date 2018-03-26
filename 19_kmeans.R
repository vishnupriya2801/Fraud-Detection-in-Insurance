library(factoextra)
# Set the target variable as a factor
data_c=data2[,-19]

# Feature Scaling
data_c= scale(data_c[c(7,11)])
###########
kmeans.result <- kmeans(data_c, centers=2)
kmeans.result$centers
kmeans.result$cluster
centers <- kmeans.result$centers[kmeans.result$cluster, ] # "centers" is a data frame of 3 centers but the length of iris dataset so we can canlculate distance difference easily.
distances <- sqrt(rowSums((data_c - centers)^2))
outliers <- order(distances, decreasing=T)[1:15]
print(outliers) # these r

print(data_c[outliers,])
colnames(data_c)

#######################################
# Using the elbow method to find the optimal number of clusters
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(data_c, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = data_c, centers =3,nstart=25 )
kmeans
fviz_cluster(kmeans, data = data_c)
