plot_loadings <- function(pca1) {

print("PCA loadings ...")
library(RColorBrewer)
png("figs/pca_loadings.png",width=12.5,height=7,units="in",res=300)
original.parameters<- par( no.readonly = TRUE )
par(xaxt="n")
n1 <- dim(pca1$loadings)[1]; n2 <- dim(pca1$loadings)[2]
#colors <- rev(brewer.pal(n2,"Blues"))
colors <- c("blue","red","green","pink","orange","purple")
barplot(t(pca1$loadings),col=colors,beside=TRUE,horiz=F,las=1,
  main="Standardized loadings",ylim=c(-1.0,1.5),axes=FALSE)
axis(2,at=seq(-1.0,1.0,0.5),labels=seq(-1.0,1.0,0.5))
lablist <- row.names(pca1$loadings)
axis(1, at=seq(3, n1*(n2+1), by=n2+1), labels = FALSE)
text(seq(3, n1*(n2+1), by=n2+1), par("usr")[3], labels = lablist, srt = 45, pos = 2, xpd = TRUE)
legend("top",colnames(pca1$loadings),fill=colors,bty="n",ncol=n2)
abline(v=seq(1, n1*(n2+1), by=n2+1),col="darkgrey",lty=2)
par(original.parameters)
dev.off()
}

