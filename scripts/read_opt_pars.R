#
rm(list=ls())
setwd("C:/Users/yuqiong.liu/Desktop/Ngen/regionalization/scripts")

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