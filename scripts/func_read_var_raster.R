# Function to read a variable from WRF-HYDRO geogrid file into a raster
 
getVarRaster <- function (geoFile,varName,nlayer=1) {

  suppressMessages(library(ncdf4))
  suppressMessages(library(raster))

  coordNC <- tryCatch(suppressWarnings(nc_open(geoFile)),
                      error=function(cond) {message(cond); return(NA)})

  coordvarList = names(coordNC[['var']])
  if ("XLONG_M" %in% coordvarList & "XLAT_M" %in% coordvarList) {
    inNCLon <- ncvar_get(coordNC, "XLONG_M")
    inNCLat <- ncvar_get(coordNC, "XLAT_M")
  } else if ("XLONG" %in% coordvarList & "XLAT" %in% coordvarList) {
    inNCLon <- ncvar_get(coordNC, "XLONG")
    inNCLat <- ncvar_get(coordNC, "XLAT")
  } else if ("lon" %in% coordvarList & "lat" %in% coordvarList) {
    inNCLon <- ncvar_get(coordNC, "lon")
    inNCLat <- ncvar_get(coordNC, "lat")
  } else {
    stop('Error: Latitude and longitude fields not found (tried: XLAT_M/XLONG_M, XLAT/XLONG, lat/lon')
  }

  nrows <- dim(inNCLon)[2]
  ncols <- dim(inNCLon)[1]

  # Reverse column order to get UL in UL
  x <- as.vector(inNCLon[,ncol(inNCLon):1])
  y <- as.vector(inNCLat[,ncol(inNCLat):1])

  coords <- as.matrix(cbind(x, y))

  # Get geogrid and projection info
  map_proj <- ncatt_get(coordNC, varid=0, attname="MAP_PROJ")$value
  cen_lat <- ncatt_get(coordNC, varid=0, attname="CEN_LAT")$value
  cen_lon <- ncatt_get(coordNC, varid=0, attname="STAND_LON")$value
  truelat1 <- ncatt_get(coordNC, varid=0, attname="TRUELAT1")$value
  truelat2 <- ncatt_get(coordNC, varid=0, attname="TRUELAT2")$value
  if (map_proj==1) {
    geogrd.proj <- paste0("+proj=lcc +lat_1=",
                          truelat1, " +lat_2=", truelat2, " +lat_0=",
                          cen_lat, " +lon_0=", cen_lon,
                          " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
  } else {
    stop('Error: Projection type not supported (currently this tool only works for Lambert Conformal Conic projections).')
  }
  
  dx <- ncdf4::ncatt_get(coordNC, varid=0, attname="DX")$value
  dy <- ncdf4::ncatt_get(coordNC, varid=0, attname="DY")$value
  if ( dx != dy ) {
    stop(paste0('Error: Asymmetric grid cells not supported. DX=', dx, ', DY=', dy))
  }

  projcoords <- rgdal::project(coords, geogrd.proj)

  # coordinates here refer to the cell center,
  # We need to calculate the boundaries for the raster file

  xmn <- projcoords[1,1] - dx/2.0   # Left border
  ymx <- projcoords[1,2] + dy/2.0   # upper border
  xmx <- xmn + ncols*dx             # Right border
  ymn <- ymx - nrows*dy             # Bottom border

 
  # create a raster stack reading layer by layer 
  if (nlayer==1) {
    s <- raster::raster(geoFile,varname=varName)
  } else {
    s <- stack()
    for (i in 1:nlayer) s<-raster::stack(s,raster::raster(geoFile,varname=varName,level=i))
  }
  extent(s) <- c(xmn,xmx,ymn,ymx)
  crs(s) <- geogrd.proj
  res(s) <- dx
   
  return(s)
}

