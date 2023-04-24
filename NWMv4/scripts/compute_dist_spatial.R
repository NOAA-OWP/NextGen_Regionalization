compute_dist_spatial <- function(file1,donors,receivers) {

  require(sf)
  ver_receiver <- "v0"
  ver_donor <- "v1.2"

  # receiver shapefile
  shps_rec <- st_read(paste0("../../datasets/gpkg_",ver_receiver,"/catchment_data.geojson"))
  ix1 <- match(receivers, shps_rec$id)
  shps_rec <- shps_rec[ix1,]
  
  # donor shapefile
  shps_don <- st_read(paste0("../../datasets/gpkg_",ver_donor,"/catchment_data.geojson"))
  ix1 <- match(donors, shps_don$id)
  shps_don <- shps_don[ix1,]  

  # compute centroids
  sf_use_s2(FALSE)
  cent_receivers <- st_centroid(shps_rec)
  cent_donors <- st_centroid(shps_don)

  # compute distance between donors and all receivers (km)
  # row: receivers; column: donors
  distSpatial0 <- st_distance(cent_donors, cent_receivers)
  distSpatial0 <- matrix(round(as.numeric(distSpatial0)/1000),ncol=length(donorsAll),byrow = TRUE)
  row.names(distSpatial0) <- receiversAll
  colnames(distSpatial0) <- donorsAll
  save(distSpatial0, file=file1)
  return(distSpatial0)
}
