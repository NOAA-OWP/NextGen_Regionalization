# convert Rdata to csv format for python event detection
rm(list=ls())

library(data.table)

runs <- c("camels","hlr")
models <- c("CFE","TOPMODEL","CFE+TOPMODEL")

runs <- "default"
models <- c("CFE","TOPMODEL")
for (m1 in models) {
for (r1 in runs) {

scenario <- paste0(m1,".",r1)
message(scenario)
f1 <- paste0("../output/flow_20081001_20140930_noah_owp_",scenario,".Rdata")
load(f1)
for (c1 in names(dtFlow))
    if (c1 != "validTime") dtFlow[[c1]] <- round(dtFlow[[c1]],2)
write.csv(dtFlow,file=gsub(".Rdata",".csv",f1),quote=FALSE,row.names=FALSE)

}}
# observation
#f1 <- "../output/usgs_hourly_flow_20081001_20140930_huc01.Rdata"
#load(f1)
#obsDt1 <- dcast(obsDt, validTime ~ site_no, value.var="q_cms")
#for (c1 in names(obsDt1))
#    if (c1 != "validTime") obsDt1[[c1]] <- round(obsDt1[[c1]],2)
#write.csv(obsDt1,file=gsub(".Rdata",".csv",f1),quote=FALSE,row.names=FALSE)