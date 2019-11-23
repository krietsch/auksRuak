globalVariables(c('.', 'osm_land', 'osm_lakes', 'osm_rivers', 'osm_roads', 'osm_buildings', 'osm_landuse'))

#' Create base map
#'
#' Use open street map data to create a basemap with the extent of the data
#'
#' @param DT         Name of the data.table
#' @param lat        Name of the column with latitude (as.character)
#' @param lon        Name of the column with longitude (as.character)
#' @param buffer     Buffer around the data
#' @param sc_dist    Distance of the scale
#' @param projection Projection of the data (default is equal area with centre Barrow)
#'
#' @return           bm, a ggplot2 base map
#' @export
#'
#' @import           data.table
#' @importFrom       sf st_as_sf st_transform st_join st_buffer st_intersects st_geometry st_intersection st_bbox st_as_sfc
#' @importFrom       magrittr %>%
#' @importFrom       ggplot2 ggplot geom_sf coord_sf aes theme element_line element_rect element_blank unit
#' @importFrom       ggspatial annotation_scale
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
#' bm = create_bm(DT)
#' bm

create_bm = function(DT, lat = 'lat', lon = 'lon', buffer = 1000, sc_dist,
                     projection = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '){

  if(nrow(DT) > 0) {

    setnames(DT, c(lat, lon), c('lat', 'lon'))

    st_d = st_as_sf(DT[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = projection)
    rs_extent = st_d %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_geometry
    bb = st_bbox(rs_extent) %>% data.table

    # make
    st_agr(osm_land) = 'constant'
    st_agr(osm_lakes) = 'constant'
    st_agr(osm_rivers) = 'constant'
    st_agr(osm_roads) = 'constant'
    st_agr(osm_buildings) = 'constant'

    # crop data
    land      = st_intersection(osm_land, rs_extent)
    lakes     = st_intersection(osm_lakes, rs_extent)
    rivers    = st_intersection(osm_rivers, rs_extent)
    roads     = st_intersection(osm_roads, rs_extent)
    buildings = st_intersection(osm_buildings, rs_extent)

    bm =
      ggplot() +
      geom_sf(data = land, fill = 'grey95') +
      geom_sf(data = lakes[lakes$fclass == 'wetland', ], fill = 'honeydew2', alpha = 0.6, colour = 'grey50') +
      geom_sf(data = lakes[lakes$fclass == 'water', ], fill = '#D7E7FF', colour = 'grey50') +
      geom_sf(data = roads, color = 'grey70') +
      geom_sf(data = buildings, color = 'grey30') +
      coord_sf(expand = FALSE, xlim = c(bb$.[1], bb$.[3]), ylim = c(bb$.[2], bb$.[4])) +
      ggspatial::annotation_scale(aes(location = 'br'), text_cex = 2) +
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
