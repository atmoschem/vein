library(osmdata)
library(vein)
library(sf)
b = 123.153365
l = 41.601566
u = 123.719161
r = 41.9998

q <- opq(bbox = c(b, l, u, r))

not_so_much_data <- q %>%
    add_osm_feature(key = 'highway') %>%
    add_osm_feature(key = 'name') %>%
    osmdata_sf()

net <- not_so_much_data$osm_lines

# plot(net$geometry, axes = T)
net$lkm <- units::set_units(st_length(net), "km")

plot(net["lkm"], axes = T, bg = "black")

summary(net$length)

net <- st_transform(net, 3857)


hy <- c("motorway","motorway_link",
        "trunk", "trunk_link",
        "primary", "primary_link",
        "secondary", "secondary_link",
        "tertiary", "tertiary_link",
        "residential")

nett <- net[net$highway %in% hy, ]
nett$id <- 1:nrow(nett)
st_write(nett, 
         "network/shenyang_osm_epsg3857.gpkg",
         delete_layer = TRUE)




g <- make_grid(net, 100)


netg <- st_intersection(
    net[net$highway %in% hy, ], 
    g)

netg$lkm <- units::set_units(st_length(netg), "km")
#plot(netg["lkm"], axes = T, bg = "black")

st_write(netg, 
         "network/shenyang_epsg3857.gpkg",
         delete_layer = TRUE)

