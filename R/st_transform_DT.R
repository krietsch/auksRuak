#' Change the projection within a data table
#'
#' @param DT   Name of the data.table
#' @param lat  Name of the column with latitude (as.character)
#' @param lon  Name of the column with longitude (as.character)
#' @param from Projection of the data (default is longlat)
#' @param to   Projection that the data should get (default is equal area with centre Barrow)
#'
#' @return     Data.table as it was with changed projection
#' @export
#'
#' @examples
#' require(data.table)
#' require(sf)
#'
#' DT = data.table(name = 'Utqiagvik',
#'                 lat  = 71.320854,
#'                 lon  = -156.648210)
#'
#' st_transform.DT(DT)

st_transform.DT <- function(DT, lat = 'lat', lon = 'lon',
                            from = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
                            to = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ') {

  if(nrow(DT) > 0) {
    setnames(DT, c(lat, lon), c('lat', 'lon'))
    pp = st_point(c(DT$lon, DT$lat)) %>% st_sfc(., crs = from)
    pp = st_transform(pp, crs = to) %>% st_coordinates
    DT[, ':=' (lon =  pp[, 1], lat =  pp[, 2]) ]
    setnames(DT, c('lat', 'lon'), c(lat, lon))
    setattr(DT, 'proj4string', to)
  }
}







