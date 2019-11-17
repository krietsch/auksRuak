#========================================================================================================================
# Create open street map (OSM) polygons for study site
#========================================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sf', 'sdb', 'ggplot2'),
        require, character.only = TRUE)

# Projection
proj = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')
d = dbq(con, 'select * FROM NANO_TAGS')
DBI::dbDisconnect(con)

#------------------------------------------------------------------------------------------------------------------------
# OSM polygons with extent of GPS tag data
#------------------------------------------------------------------------------------------------------------------------

# change projection
st_transform_DT(d)

# get extent of GPS data
st_d = st_as_sf(d[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = PROJ)
bbox = st_bbox(st_d) %>% st_as_sfc %>% st_buffer(., dist = 10000)

# check data
ggplot() +
  geom_sf(data = bbox) +
  geom_point(data = d, aes(lon, lat))

# transform in lat lon
bbox_latlon = st_transform(bbox, crs = proj)


# load OSM data
wd = '//ds/raw_data_kemp/COMPARATIVE/REMOTE_SENSING/OpenStreetMap/DATA_SOURCES/'

land_       = st_read(paste0(wd, 'land-polygons-complete-4326/land_polygons.shp'))
lakes_      = st_read(paste0(wd, 'alaska-latest-free.shp/gis_osm_water_a_free_1.shp'))
rivers_     = st_read(paste0(wd, 'alaska-latest-free.shp/gis_osm_waterways_free_1.shp'))
roads_      = st_read(paste0(wd, 'alaska-latest-free.shp/gis_osm_roads_BRW.shp'))
buildings_  = st_read(paste0(wd, 'alaska-latest-free.shp/gis_osm_buildings_a_free_1.shp'))

# crop data
land      = st_intersection(land_, bbox_latlon)
lakes     = st_intersection(lakes_, bbox_latlon)
rivers    = st_intersection(rivers_, bbox_latlon)
roads     = st_intersection(roads_, bbox_latlon)
buildings = st_intersection(buildings_, bbox_latlon)

# change projection
osm_land      = st_transform(land, crs = PROJ)
osm_lakes     = st_transform(lakes, crs = PROJ)
osm_rivers    = st_transform(rivers, crs = PROJ)
osm_roads     = st_transform(roads, crs = PROJ)
osm_buildings = st_transform(buildings, crs = PROJ)


# check data
ggplot() +
  geom_sf(data = bbox) +
  geom_sf(data = land) +
  geom_sf(data = lakes) +
  geom_point(data = d, aes(lon, lat))

# save data
save(osm_land, file = './data/osm_land.RData', compress = 'xz')
save(osm_lakes, file = './data/osm_lakes.RData', compress = 'xz')
save(osm_rivers, file = './data/osm_rivers.RData', compress = 'xz')
save(osm_roads, file = './data/osm_roads.RData', compress = 'xz')
save(osm_buildings, file = './data/osm_buildings.RData', compress = 'xz')





