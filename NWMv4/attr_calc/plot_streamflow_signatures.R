# plot streamflow signatures and compare with the CAMELS paper (Addor et al 2017)

rm(list=ls())

library(maptools)
library(maps)
library(mapdata)
library(lattice)
library(latticeExtra)
library(RColorBrewer)

dt1 <- get(load("../../output/ss_attrs_donors_v3.Rdata"))
dt1 <- rbind(dt1,get(load("../../output/ss_attrs_ss_basins.Rdata")))
pars <- names(dt1)
pars <- pars[-(1:3)]

cuts <- list(q_mean=c(0,0.3,0.8,1,1.5,2,Inf),
             runoff_ratio=c(0,0.2,0.3,0.4,0.5,0.6,1),
             hfd_mean=c(0,155,163,174,197,224,366),
             slope_fdc=c(-Inf,2,2.5,3,4,5,Inf),
             baseflow_index=c(0,0.3,0.4,0.5,0.6,0.7,1),
             stream_elas=c(-Inf,1,1.5,2,2.5,3,Inf),
             high_q_freq=c(0,4,8,15,30,50,Inf),
             high_q_dur=c(0,2,5,10,15,Inf),
             Q95=c(0,1,3,4,5,8,Inf),
             low_q_freq=c(0,15,50,100,150,200),
             low_q_dur=c(0,10,13,20,40,Inf),
             Q5=c(0,0.04,0.1,0.2,0.4,Inf),
             zero_q_freq=c(0,10,20,30,40,50,100))

colors <- list(q_mean=brewer.pal(length(cuts[["q_mean"]])-1,"BrBG"),
               runoff_ratio=brewer.pal(length(cuts[["runoff_ratio"]])-1,"BrBG"),
               hfd_mean=brewer.pal(length(cuts[["slope_fdc"]])-1,"RdBu"),
               slope_fdc=rev(brewer.pal(length(cuts[["slope_fdc"]])-1,"PiYG")),
               baseflow_index=rev(brewer.pal(length(cuts[["baseflow_index"]])-1,"PuOr")), 
               stream_elas=rev(brewer.pal(length(cuts[["stream_elas"]])-1,"RdBu")),
               high_q_freq=brewer.pal(length(cuts[["high_q_freq"]])-1,"YlGnBu"),
               high_q_dur=brewer.pal(length(cuts[["high_q_dur"]])-1,"Blues"),
               Q95=brewer.pal(length(cuts[["Q95"]])-1,"Blues"),
               low_q_freq=brewer.pal(length(cuts[["low_q_freq"]])-1,"YlOrBr"),
               low_q_dur=brewer.pal(length(cuts[["low_q_dur"]])-1,"YlOrBr"),
               Q5=rev(brewer.pal(length(cuts[["Q5"]])-1,"YlOrBr")),               
               zero_q_freq=rev(brewer.pal(length(cuts[["zero_q_freq"]])-1,"PuOr")))               

# define map background
usmap    <- map(database="state", exact=F, boudnary=FALSE, plot=FALSE, resolution=0)
wldmap   <- map(database="worldHires", exact=F, plot=FALSE, resolution=0, xlim=c(-135,-60), ylim=c(10,60))
us.sp    <- map2SpatialLines(usmap, proj4string=CRS("+proj=longlat +datum=WGS84"))
wld.sp   <- map2SpatialLines(wldmap, proj4string=CRS("+proj=longlat +datum=WGS84"))
pj4str <- "+proj=longlat +datum=WGS84"
us.proj  <- spTransform(us.sp, CRS(pj4str))
wld.proj <- spTransform(wld.sp, CRS(pj4str))
map.lns <- list("sp.lines", us.proj, lwd=0.6, col="white")
map.wld <- list("sp.lines", wld.proj, lwd=0.6, col="black")
panel.layout <- list(map.lns,map.wld)
mpixel  <- 5e6

dt1 <- na.omit(dt1)

# get gage meta data
meta_calib <- get(load("../../data/meta_donors_v3.Rdata"))
meta_ss <- get(load("../../data/meta_ss_basins.Rdata"))
meta <- rbind(meta_calib[,c("ID","lat_cent","lon_cent")],meta_ss[,c("ID","lat_cent","lon_cent")])

for (par1 in pars) {

coord1 <- data.frame(lon=as.numeric(meta$lon[match(dt1$ID,meta$ID)]),
                     lat=as.numeric(meta$lat[match(dt1$ID,meta$ID)]))
data1 <- as.data.frame(dt1[[par1]])
names(data1) <- par1
data1[[par1]] <- cut(data1[[par1]],cuts[[par1]])
spdf1 <- SpatialPointsDataFrame(coords=coord1,data=data1,proj4string = CRS(pj4str))
color1 <- colors[[par1]]

png(filename=paste0("../../figs/map_",par1,"_donors_ss_basins.png"),width=6,height=3.5,res=300,units="in")
print(spplot(spdf1,col.regions=color1,pch=16,cex=0.5,
   ylim=c(26,51),xlim=c(-126,-65), main = par1,
   sp.layout=panel.layout, key.space="right", maxpixels=mpixel,
   par.settings=list(panel.background=list(col="grey"))))

dev.off()

}
