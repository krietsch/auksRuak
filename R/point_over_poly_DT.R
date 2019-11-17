#' Check if points are over a polygon (e.g. study site)
#'
#' @param DT     Name of the data.table
#' @param lat    Name of the column with latitude (as.character)
#' @param lon    Name of the column with longitude (as.character)
#' @param poly   Name of the polygon to overlap the data with (changes projection to PROJ)
#' @param buffer Buffer distance (see st_buffer)
#' @param projection   Projection of the data (default is equal area with centre Barrow)
#'
#' @return       Data.table with additional conditional column poly_overlap
#' @export
#'
#' @import       data.table
#' @importFrom   sf st_as_sf st_transform st_join st_buffer st_intersects
#' @importFrom   magrittr %>%
#' @importFrom   ggplot2 ggplot
#'
#' @examples
#' require(data.table)
#'
#' # load study site
#' data(study_site)
#'
#' # create table with one inside and one outside the study site
#' DT = data.table(name = c('NARL', 'Utqiagvik'),
#'                 lat  = c(71.320854, 71.290246),
#'                 lon  = c(-156.648210, -156.788622))
#'
#' # change projection
#' st_transform_DT(DT)
#' point_over_poly_DT(DT, lat = 'lat', lon = 'lon', poly = study_site)
#'
#' \dontrun{
#' # check data
#' require(ggplot2)
#' ggplot() +
#'   geom_sf(data = study_site, fill = 'grey95') +
#'   geom_point(data = DT, aes(lon, lat, color = poly_overlap))
#' }

point_over_poly_DT <- function(DT, lat = 'lat', lon = 'lon', poly,  buffer = NA,
                               projection = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '){

  poly_overlap = NULL

  if(nrow(DT) > 0) {

    setnames(DT, c(lat, lon), c('lat', 'lon'))

    # change projection
    poly = st_transform(poly, crs = projection)

    # buffer
    if(!is.na(buffer)) {poly = st_buffer(poly, dist = buffer) } # in meter

    # assign points in study site
    st_DT = st_as_sf(DT, coords = c('lon','lat'), crs = projection)
    DT[, poly_overlap := st_intersects(st_DT, poly, sparse = FALSE)]

    setnames(DT, c('lat', 'lon'), c(lat, lon))

  }
}

