library(sf)
library(data.table)

language <- "english" # portuguese english spanish

switch(language,
       "portuguese" = cat("\nLendo: traffic_volume_4.csv\n"),
       "english" = cat("Reading: traffic_volume_4.csv\n"),
       "spanish" = cat("Leyendo: traffic_volume_4.csv\n")
)

x <- readLines("network/2023-06-12_09-04-06_traffic_volume.csv")

x <- fread("network/2023-06-12_09-04-06_traffic_volume.csv", 
           encoding="UTF-8")

x$id <- 1:nrow(x)

geo <- x$coordinate
x$coordinate <- NULL

sgeo <- strsplit(x = geo, split = ";")

lsgeo <- lapply(sgeo, strsplit, split = ":")

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
summary(net$time)
net <- net[net$h == 9, 
            c("id", 
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
pveh <- data.frame(veh = sapply(veh[, 2:ncol(veh)], sum))
pveh$names <- row.names(pveh)
pveh$per <- pveh$veh / sum(pveh$veh)

for(i in 1:nrow(pveh)){
  net[[pveh$names[i]]] <- net$traffic_volume*pveh$per[i]
}
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
