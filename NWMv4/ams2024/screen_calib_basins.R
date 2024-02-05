# determine which calibration basins have satifactory performance to serve as donors in regionalization
# In this example, we require KGE>0.2 and NSE>0

library(data.table)

# calibration stats
dtStat <- as.data.table(read.csv("../data/calibration_results/huc01_v1.2/cfe_noah_metrics.csv", colClasses=c(rep("character",2),rep("numeric",16))))

# all calibrated basins
gages0 <- unique(dtStat$site_no)
write.table(gages0,file="../data/donor_gages_huc01_all.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)

# basins with satisfactory performance
gages <- unique(subset(dtStat, period=="valid" & KGE>0.2 & NSE>0)$site_no)
write.table(gages,file="../data/donor_gages_huc01_screened.csv",quote=FALSE,row.names=FALSE,col.names=FALSE)
