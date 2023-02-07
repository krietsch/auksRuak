globalVariables(c('.', 'osm_land', 'osm_lakes', 'osm_rivers', 'osm_roads', 'osm_buildings', 'osm_landuse'))

#' Create colored base map
#'
#' Use open street map data to create a basemap with the extent of the data
#'
#' @param DT          Name of the data.table
#' @param lat         Name of the column with latitude (as.character)
#' @param lon         Name of the column with longitude (as.character)
#' @param buffer      Buffer around the data
#' @param aspect      Set aspect ratio of the base map
#' @param sc_dist     Distance of the scale
#' @param sc_location Where to put the scale bar ("tl" for top left, etc.)
#' @param sc_cex      Label size as numeric character expansion factor
#' @param projection  Projection of the data (default is equal area with centre Barrow)
#'
#' @return            bm, a ggplot2 base map
#' @export
#'
#' @import            data.table
#' @importFrom        sf st_as_sf st_transform st_join st_buffer st_intersects st_geometry st_intersection st_bbox st_as_sfc st_crs
#' @importFrom        magrittr %>%
#' @importFrom        ggplot2 ggplot geom_sf coord_sf aes theme element_line element_rect element_blank unit
#' @importFrom        ggspatial annotation_scale
#' @importFrom        sfext st_bbox_ext
#'
#' @examples
#' # create table with two points
#' DT = data.table(name = c('NARL', 'Utqiagvik'),
#'                 lat  = c(71.320854, 71.290246),
#'                 lon  = c(-156.648210, -156.788622))
#'
#' # change projection
#' st_transform_DT(DT)
#'
#' # create base map
#' bm = create_colored_bm(DT)
#' bm

create_colored_bm = function(DT, lat = 'lat', lon = 'lon', buffer = 1000, aspect = '16:9', sc_dist,
                             sc_location = 'br', sc_cex = 0.7,
                             projection = paste0('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0',
                                                 ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')){

  if(nrow(DT) > 0) {

    setnames(DT, c(lat, lon), c('lat', 'lon'))

    # create bounding box
    st_d = st_as_sf(DT[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = projection)
    rs_extent = st_d %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_buffer(buffer) %>%
      st_bbox_ext(asp = aspect, crs = projection) %>% st_as_sfc %>% st_geometry
    rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
    bb = st_bbox(rs_extent) %>% data.table

    # create base map
    bm =
      ggplot() +
      geom_sf(data = osm_land, fill = '#faf5ef') + #f6eee2  #f8f2e9
      geom_sf(data = osm_lakes[osm_lakes$fclass == 'wetland', ], fill = '#faf5ef', alpha = 0.6, colour = '#faf5ef') +
      geom_sf(data = osm_lakes[osm_lakes$fclass == 'water', ], fill = '#f3fafd', colour = 'grey80') + # #D7E7FF
      geom_sf(data = osm_roads, color = 'grey70') +
      geom_sf(data = osm_buildings, color = 'grey30') +
      coord_sf(expand = FALSE, xlim = c(bb$.[1], bb$.[3]), ylim = c(bb$.[2], bb$.[4])) +
      ggspatial::annotation_scale(aes(location = sc_location), sc_cex = 2) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            panel.grid.minor = element_line(colour = "transparent"),
            panel.background = element_rect(fill = '#D7E7FF'),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(fill = NA, colour = "black"),
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
            axis.title = element_blank(), plot.margin = unit(rep(0, 4), "lines"))

    setnames(DT, c('lat', 'lon'), c(lat, lon))

    bm

  }
}
