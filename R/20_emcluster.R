library(mclust)
set.seed(13)
d_clust2 <- Mclust(as.matrix(data_c), G=1:20)
plot(d_clust2)

library('EMCluster')
emcluster=init.EM(data_c, nclass = 2, EMC = .EMC,
                  min.n = NULL, min.n.iter = 5,
                  method = "Rnd.EM")

emcluster
plotem(emcluster,data_c)
plot2d(emobj=NULL,data_c)

