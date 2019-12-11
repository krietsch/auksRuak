# create table with two points
DT = data.table(name = c('NARL', 'Utqiagvik'),
                lat  = c(71.320854, 71.290246),
                lon  = c(-156.648210, -156.788622))

# change projection
st_transform_DT(DT)

# create base map
bm = create_bm(DT)
bm

require(sf)

projection = paste0('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0',
                    ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
buffer = 1000


st_d = st_as_sf(DT[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = projection)
rs_extent = st_d %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_geometry
bb = st_bbox(rs_extent) %>% data.table


PROJ = paste0('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0',
              ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
st_crs(osm_land) <- PROJ


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
