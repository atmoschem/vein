

if(download_osm) {
  # get OSM
  message(paste0("Downloading OSM\n"))
  
  file.exists("network/sudeste")
  
  if( file.exists(file_down))
    
  get_osm(region = "sudeste", file = file_down)
  
  osm <- osmdata_sf(
    add_osm_feature(
      opq(bbox = st_bbox(st_transform(net, 4326)), timeout = 50),
      key = 'highway'))$osm_lines[, c("highway")]
  st <- c("motorway", "motorway_link", "trunk", "trunk_link",
          "primary", "primary_link", "secondary", "secondary_link",
          "tertiary", "tertiary_link")
  osm <- osm[osm$highway %in% st, ]
  
}
