library(sf)
library(data.table)
language <- "english" # portuguese english spanish

switch(language,
       "portuguese" = cat("\nLendo: traffic_volume\n"),
       "english" = cat("Reading: traffic_volume\n"),
       "spanish" = cat("Leyendo: traffic_volume\n")
)

net <- st_read("network/shenyang_osm_epsg3857.gpkg",
               crs = 3857)

x <- fread("network/2023-06-12_09-04-06_traffic_volume.csv", 
           encoding="UTF-8")

geo <- x$coordinate

x$coordinate <- NULL

x$updatetime <- as.POSIXct(x$updatetime, 
                           format = "%Y-%m-%d_%H:%M:%S",
                           tz = "UTC")

sgeo <- strsplit(x = geo, split = ";")

lsgeo <- lapply(sgeo, strsplit, split = ":")

rbindlist(lapply(1:nrow(x), function(i) {
  df <- as.data.frame(do.call("rbind", lsgeo[[i]]))
  
  df$name <- x$name[i]
  df$status <- x$level[i]
  df$speed <- x$speed[i]
  df$updatetime <- x$updatetime[i]
  df$volume <- x$traffic_volume[i]
  df$id_vol <- i
  df
})) -> df
df$V1 <- as.numeric(df$V1)
df$V2 <- as.numeric(df$V2)

sdf <- st_as_sf(df[volume > 0], 
                coords = c("V1", "V2"),
                crs = 4326)

sdf <- st_transform(sdf, 3857)

# convert to pseudo mercator
sdfb <- st_buffer(x = sdf, dist = 20)

sdfbi <- st_intersection(net["id"], sdfb)

idvol <- as.data.table(
  sdfbi)[,
         lapply(.SD, mean, na.rm = TRUE),
         .SDcols = c("speed", 
                     "volume", 
                     "updatetime"),
         by = id]

netv <- merge(st_set_geometry(net, NULL), 
              idvol, 
              by = "id", 
              all.x =T)

netv <- st_sf(netv, geometry = st_geometry(net)) 

# CHECK FOR NA, INTERPOLATE!
summary(netv$volume)

netv$volume[is.na(netv$volume)] <- 0
netv$speed[is.na(netv$speed)] <- mean(netv$speed, na.rm = T)

# plot(netv["volume"])
if(plots){
  png(filename = "images/volume.png", 
      width = 2000, height = 2000, res = 300)
  plot(netv["volume"], 
       axes = T, 
       bg = "black", 
       logz = F)
  dev.off()
  
  png(filename = "images/speed.png", 
      width = 2000, height = 2000, res = 300)
  plot(netv["speed"], 
       axes = T, 
       bg = "black", 
       logz = F)
  dev.off()
  
  
} 

pveh <- data.frame(veh = sapply(veh[, 2:ncol(veh)], sum))
pveh$names <- row.names(pveh)
pveh$per <- pveh$veh / sum(pveh$veh)

for(i in 1:nrow(pveh)){
  netv[[pveh$names[i]]] <- netv$volume*pveh$per[i]
}

switch(language,
       "portuguese" = cat("\nSalvando net em:\n"),
       "english" = cat("Saving net in:\n"),
       "spanish" = cat("Guardando net en:\n")
)

cat("network/net.rds\n")
saveRDS(netv, "network/net.rds")

