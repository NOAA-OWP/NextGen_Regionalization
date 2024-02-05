# new zonal function to calcualte areal mean; if data coverage is below 
# the minimum coverage requirement, areal mean is set to NA 

zonal_new <- function(r0,r1,func="mean",min_coverage=0.8,missing_value=-999.0) {

library(raster)

if (nrow(r0)!=nrow(r1) | ncol(r0)!=ncol(r1)) stop("ERROR: source raster and zone raster not the same size!")

crs(r0) <- crs(r1)
extent(r0) <- extent(r1)
res(r0) <- res(r1)

r0[r0==missing_value] <- NA
r0[is.infinite(r0)] <- NA

# first compute zonal function as uaual
dt1 <- data.table(zonal(r0,r1,func,na.rm=TRUE))
names(dt1) <- c("id","value")

# determine fraction of data coverage
# first calculate total number of cells in each basin (i.e., rough area)
a1 <- as.data.frame(table(as.vector(r1)))
names(a1) <- c("id","ncell")
a1$id <- as.numeric(as.character(a1$id))
dt1 <- merge(dt1,a1,by="id")

# then number of cells with valid value in each basin
r2 <- r0
values(r2)[!is.na(values(r0))] <- 1
ncell1 <- data.table(zonal(r2,r1,"sum",na.rm=TRUE))
names(ncell1) <- c("id","coverage")
dt1 <- merge(dt1,ncell1,by="id")
dt1[,coverage:=coverage/ncell]

# set areal mean to NA if data coverage does not meet minimum coverage
dt1[,value:=ifelse(coverage>=min_coverage,value,NA)]
dt1$ncell <- dt1$coverage <- NULL

return(dt1)
}
