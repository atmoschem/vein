library(geofabrik) # Downloads OSM data from geofabrik
# customize this script
# Check all geofabrick links
dir.create("OSM")
head(urlgeo)
names(urlgeo)
# convert to data.table
# search africa
af <- grep("Ecuador", urlgeo$Subregion)
urlgeo[af, ]

download.file(url = "https://download.geofabrik.de/south-america/ecuador-210101-free.shp.zip", 
              destfile = "OSM/ecuador.shp.zip")

unzip(
  zipfile = paste0(
    gsub(" ", "", file_down),
    ".shp.zip"
  ),
  exdir = "network"
)

# This approach may be faster with QGIS
# let us read
roads <- st_read("network/gis_osm_roads_free_1.shp")
# let us select the streets that we need
st <- c(
  "motorway", "motorway_link", "trunk", "trunk_link",
  "primary", "primary_link", "secondary", "secondary_link",
  "tertiary", "tertiary_link"
)
roads <- roads[roads$fclass %in% st, ]
# save roads on network
saveRDS(roads, "network/osm_roads.rds")

