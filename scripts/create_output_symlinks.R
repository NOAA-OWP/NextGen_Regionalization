rm(list=ls())

library(data.table)

dir0 <- "/local/ngen/data/fihm/output/"

# read all optimal paramete sets
pars0 <- as.data.table(read.csv("../data/headwater_cat_optimal_params.csv",header = TRUE))

# use optimal parameter sets from validation period
pars <- subset(pars0, period=="valid")

# parmaeter names for CFE and Topmodel
cfe_pars <- names(pars)[4:12]
topmod_pars <- names(pars)[13:17]
par_cfe <- pars[model_type=="CFE", c("catid",cfe_pars), with=F]
par_topmod <- pars[model_type=="TOPMODEL", c("catid",topmod_pars), with=F]
par_mosaic <- pars[model_type=="CFE+TOPMODEL",c("catid",cfe_pars,topmod_pars), with=F]

# donor pairing
load("../output/donor_camels.Rdata")
donors <- unique(dtDonorAll$donor)
for (d1 in donors) dtDonorAll <- rbind(dtDonorAll,data.table(id=d1,donor=d1,distAttr=0,distSpatial=0,tag="donor"))

print(length(unique(dtDonorAll$id)))

links <- NULL
for (id1 in dtDonorAll$id) {
  donor <- dtDonorAll$donor[match(id1,dtDonorAll$id)]
  model <- ifelse(is.na(par_mosaic$b[match(donor,par_mosaic$catid)]),"TOPMODEL","CFE")
  cat_num <- gsub("cat-","",id1)
  links <- c(links,paste0("ln -s ",dir0,"camels_noah_owp_",model,"_donated/cat-",cat_num,".csv ",
                          dir0,"camels_noah_owp_mosaic_donated/cat-",cat_num, ".csv"))
  links <- c(links,paste0("ln -s ",dir0,"camels_noah_owp_",model,"_donated/nex-",cat_num,"_output.csv ",
                          dir0,"camels_noah_owp_mosaic_donated/nex-",cat_num, "_output.csv"))
  links <- c(links,paste0("ln -s ",dir0,"camels_noah_owp_",model,"_donated/tnx-",cat_num,"_output.csv ",
                          dir0,"camels_noah_owp_mosaic_donated/tnx-",cat_num, "_output.csv"))
}

writeLines(links,"../output/mosaic_output_symlinks.sh")
