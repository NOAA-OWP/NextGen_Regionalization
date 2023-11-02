library(data.table)

files <- list.files("../data/nwm", pattern=".Rdata", full.names=FALSE)
for (f1 in files) {
    print(f1)
    b1 <- gsub("_proj_data.Rdata","",f1)
    load(paste0("../data/nwm/",f1))
    write.csv(x_archive,paste0("../data/nwm/",b1,"_pars.csv"),row.names=FALSE,quote=FALSE)
    dt1 <- data.table(POSIXct=chrt.obj.1[['POSIXct']])
    for (i in 1:300) {
        dt0 <- get(paste0("chrt.obj.",i))[,c('POSIXct','q_cms'),with=F]
        dt0$q_cms <- round(dt0$q_cms,2)
        dt1 <- merge(dt1,dt0,by='POSIXct',all.x=TRUE)
        names(dt1)[length(names(dt1))] <- paste0('m',i)
    }
    write.csv(dt1,paste0("../data/nwm/",b1,"_flow.csv"),row.names=FALSE,quote=FALSE)
}
