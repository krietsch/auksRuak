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
#' @import     data.table
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom magrittr %>%
#'
#' @examples
#' require(data.table)
#'
#' DT = data.table(name = c('NARL', 'Utqiagvik'),
#'                 lat  = c(71.320854, 71.290246),
#'                 lon  = c(-156.648210, -156.788622))
#'
#' st_transform_DT(DT)

st_transform_DT <- function(DT, lat = 'lat', lon = 'lon',
                            from = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
                            to = paste0('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0',
                                        ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')){

  if(nrow(DT) > 0) {
    setnames(DT, c(lat, lon), c('lat', 'lon'))
    pp = st_as_sf(DT, coords = c('lon', 'lat'), crs = from)
    pp = st_transform(pp, crs = to) %>% st_coordinates
    DT[, ':=' (lon =  pp[, 1], lat =  pp[, 2]) ]
    setnames(DT, c('lat', 'lon'), c(lat, lon))
    setattr(DT, 'proj4string', to)
  }
}





