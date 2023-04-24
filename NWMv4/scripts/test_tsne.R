library(Rtsne)
library(dplyr)
library(magrittr)
library(fpc)
library(data.table)

# all attributes
load("../data/all_attrs.Rdata")

# attributes to use for clustering
attrs <- c("prcFlatLowland","prcFlatUpland","prcFlatTotal","relief","cidx","aridity","sand_frac",
           "clay_frac","soil_depth","forest_frac","cropland_frac","urban_frac")
dtAttr1 <- dtAttrAll %>% select(all_of(attrs))
dtAttr1 <- na.omit(dtAttr1)

# dimension reduction
kk <- 0
ndims <- c(2)
nps <- c(50,100,150,200)
its <- c(500,1000,2500,5000)
for (nd1 in ndims) {
for (np1 in nps) {
for (nt1 in its) {
  
tit1 <- paste0("t-SNE: dims=",nd1,", perplexity=",np1,", iteration=",nt1)
message("=============================")
message(tit1) 

tsne <- Rtsne(dtAttr1, dims = nd1, perplexity=np1, verbose=FALSE, max_iter = nt1)
colnames(tsne$Y) <- paste0("Dim ",1:nd1)
#plot(tsne$Y)
#scatterplot3d(tsne$Y)

# k-mediods clustering
set.seed(7777)
fit2 = pamk(tsne$Y,krange = 2:30,criterion = "multiasw",usepam = FALSE,ns=10,critout = FALSE,seed=777)
clusters <- fit2$pamobject$clustering

# visualize
kk <- kk+1
png(paste0("../figs/tsne_testing_",kk,".png"),width=6.5,height = 6,units="in",res=300)
plot(tsne$Y,t="n",main=tit1)
#scatterplot3d(tsne$Y,color=colors[clusters],main="tsne")
colors = rainbow(length(unique(clusters)))
names(colors) = unique(clusters)
text(tsne$Y, labels=clusters, col=colors[clusters])
dev.off()
}}}
