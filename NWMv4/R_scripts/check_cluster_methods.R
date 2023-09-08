#
rm(list=ls())

require(cluster)
require(vegan)
require(mclust)
require(apcluster)
require(NbClust)
require(fpc)

mydata <- get(load("data/clustering_test_data.Rdata"))
cmax <- 20

## pam: too slow (k.best=4)
asw <- NULL
for (k in 2:cmax) {
  print(k)
  asw <- c(asw, pam(mydata, k) $ silinfo $ avg.width)
}
k.best <- which.max(asw)


## pamk: too slow
set.seed(7777)
fit = pamk(mydata,krange = 1:10,criterion = "asw",usepam = TRUE)
clusters <- fit$pamobject$clustering
  
## vegan: speed okay (k.best=6)
fit <- cascadeKM(scale(mydata, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))

  
## mclust: speed okayish (m.best=20)
d_clust <- Mclust(as.matrix(mydata), G=1:20)
m.best <- dim(d_clust$z)[2]

## apcluster: way too slow; R session stuck
d.apclus = apcluster(negDistMat(r=2), mydata)

## clusGap: very slow (number of clusters = 1)
clusGap = clusGap(mydata, kmeans, 10, B = 100, verbose = interactive())

## NbClust: extremely slow
setseed(7777)
nbc = NbClust(mydata, diss=NULL, distance = "euclidean",
                    method = "kmeans", min.nc=2, max.nc=10, 
                    index = "alllong", alphaBeale = 0.1)


# Unit: milliseconds
# expr         min          lq        mean      median          uq         max neval
# wss    16.83938    16.83938    16.83938    16.83938    16.83938    16.83938     1
# fpc   221.99490   221.99490   221.99490   221.99490   221.99490   221.99490     1
# fpc_1    43.10493    43.10493    43.10493    43.10493    43.10493    43.10493     1
# vegan  1096.08568  1096.08568  1096.08568  1096.08568  1096.08568  1096.08568     1
# mclust  1531.69475  1531.69475  1531.69475  1531.69475  1531.69475  1531.69475     1
# d.apclus    28.56100    28.56100    28.56100    28.56100    28.56100    28.56100     1
# clusGap  1096.50680  1096.50680  1096.50680  1096.50680  1096.50680  1096.50680     1
# NbClust 10940.98807 10940.98807 10940.98807 10940.98807 10940.98807 10940.98807     1