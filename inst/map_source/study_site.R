#========================================================================================================================
# Create polygon for study site
#========================================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sf'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# study area
file_path = system.file('map_src', 'edge.kml', package = 'wadeR')
# file_path = system.file('map_source', 'study_site.kml', package = 'auksruak')
study_site = st_read(file_path, quiet  = TRUE) %>% st_geometry %>% st_zm(drop = TRUE)

# change projection
study_site = st_transform(study_site, crs = PROJ)

# save
# save(study_site, file = './data/study_site.RData')



