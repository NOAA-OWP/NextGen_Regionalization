# if there are catchments of GEOMETRYCOLLECTION type (e.g., polygon + linestring), 
# remove the linestring, otherwise zonal does not work

correct_geojson <- function(huc_no, huc) {
    if (huc_no == "17") {
        huc1 <- huc[which(st_geometry_type(huc$geometry)=="GEOMETRYCOLLECTION"),]
        for (i1 in 1:nrow(huc1)) huc1$geometry[i1] <- huc1$geometry[i1][[1]][[1]]
        huc <- huc[which(st_geometry_type(huc$geometry)!="GEOMETRYCOLLECTION"),]
        huc <- rbind(huc,huc1)
        huc <- huc[order(huc$id),]
    }
    return(huc)
}