# if there are catchments of GEOMETRYCOLLECTION type (e.g., polygon + linestring), 
# remove the linestring, otherwise zonal does not work

correct_geojson <- function(huc_no, huc) {
    huc1 <- huc[which(st_geometry_type(huc$geometry)=="GEOMETRYCOLLECTION"),]
    for (i1 in 1:nrow(huc1)) {
        for (j1 in 1:length(huc1$geometry[i1][[1]])) {
            if(st_geometry_type(huc1$geometry[i1][[1]][[j1]]) == "POLYGON") {
                huc1$geometry[i1] <- huc1$geometry[i1][[1]][[j1]]; break
            }
        }        
    }
    huc <- huc[which(st_geometry_type(huc$geometry)!="GEOMETRYCOLLECTION"),]
    huc <- rbind(huc,huc1)
    huc <- huc[order(huc$id),]
    return(huc)
}