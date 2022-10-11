library(sf)
library(data.table)

switch(language,
       "portuguese" = cat("\nLendo: traffic_volume_3.csv\n"),
       "english" = cat("Reading: traffic_volume_3.csv\n"),
       "spanish" = cat("Leyendo: traffic_volume_3.csv\n")
)

x <- fread("network/traffic_volume_3.csv", 
           encoding="UTF-8")
x$id <- 1:nrow(x)
geo <- x$coordinate
head(geo)
sgeo <- strsplit(x = geo, split = ";")
sgeo[[1]]

lsgeo <- lapply(sgeo, strsplit, split = ":")
lsgeo[[1]]

switch(language,
       "portuguese" = cat("\nConstruindo geometria\n"),
       "english" = cat("Building geometry\n"),
       "spanish" = cat("Construyendo geometria\n")
)

lgeo <- lapply(seq_along(geo), function(i) {
  m <- as.data.frame(do.call("rbind", lsgeo[[i]]))
  for(j in 1:2) m[, j] <- as.numeric(m[, j])
  st_linestring(as.matrix(m))
})

llgeo <- st_as_sfc(lgeo, crs = 4326)

net <- st_sf(x, geometry = llgeo)

net$id <- 1:nrow(net)

net$time <- as.POSIXct(net$updatetime, 
                       format = "%Y-%m-%d_%H:%M:%S",
                       tz = "UTC")
net$h <- hour(net$time)
net <- net[net$h == 9, 
            c("linkId", 
              "speed",
              "travelTime",
              "road_class",
              "traffic_volume")]



# here we need to identify the following
# categories:

# 01. PV_TAXI	
# 02. PV_3W	
# 03. PV_MINI	
# 04. PV_SMALL	
# 05. PV_MEDIUM
# 06. PV_LARGE
# 07. BUS_URBAN	
# 08. BUS_COACH	
# 09. TRUCKS_MINI	
# 10. TRUCKS_LIGHT
# 11. TRUCKS_MEDIUM	
# 12. TRUCKS_HEAVY	
# 13. TRUCKS_LOWSPEED
# 14. MC_ORDINARY
# 15. MC_LIGHT

# here we need to improve this characterization
# based on traffic counts
net$PV_TAXI <- net$traffic_volume*0.0029159
net$PV_3W <- net$traffic_volume*0.0014580
net$PV_MINI <- net$traffic_volume*0.0291591
net$PV_SMALL <- net$traffic_volume*0.7839591
net$PV_MEDIUM <- net$traffic_volume*0.0270321
net$PV_LARGE <- net$traffic_volume*0.0264136
net$BUS_URBAN <- net$traffic_volume*0.0264136
net$BUS_COACH <- net$traffic_volume*0.0264136
net$TRUCKS_MINI <- net$traffic_volume*0.0016935
net$TRUCKS_LIGHT <- net$traffic_volume*0.0649532
net$TRUCKS_MEDIUM <- net$traffic_volume*0.0195176
net$TRUCKS_HEAVY <- net$traffic_volume*0.0315447
net$TRUCKS_LOWSPEED <- net$traffic_volume*0.0315447
net$MC_ORDINARY <- net$traffic_volume*0.0017495
net$MC_LIGHT <- net$traffic_volume*0.0011664

st_crs(net) <- 4326

switch(language,
       "portuguese" = cat("\nSalvando net em:\n"),
       "english" = cat("Saving net in:\n"),
       "spanish" = cat("Guardando net en:\n")
)

cat("network/net.gpkg\n")
st_write(net, "network/net.gpkg", delete_layer = T)

suppressWarnings(
  rm(x, geo, llgeo, net, lsgeo, sgeo))
invisible(gc())
