# load the libraries
library(mlbench)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(data2, method=c("center", "scale", "pca"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, data2)
transformed$FraudFound_P=data2[,19]
transformed=transformed[14:19]
# summarize the transformed dataset
summary(transformed)
#plot first 2 components
library(lattice)
pca.plot <- xyplot(transformed[,2] ~ transformed[,3])
pca.plot$xlab <- "First Component"
pca.plot$ylab <- "Second Component"
pca.plot

#scatterplot3d
library(scatterplot3d)
scatterplot3d(transformed[,1], transformed[,2], transformed[,3], highlight.3d=TRUE,
              col.axis="blue", col.grid="lightblue",
              main="scatterplot3d - 2", pch=20)

data$FraudFound_P=as.numeric(data$FraudFound_P)
