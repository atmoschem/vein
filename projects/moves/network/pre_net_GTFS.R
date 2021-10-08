library(sf)
library(gtfs2gps)
library(magrittr)
library(data.table)

x <- st_read("network/net_no_bus.gpkg")
names(x)
x$id <- 1:nrow(x)
crs_original <- st_crs(x)

# GTFS BUS LOCAL ####
p2 <- "../GTFS/mdotmta_gtfs_localbus.zip"
curi <- gtfs2gps::read_gtfs(p2) %>%
  gtfs2gps::filter_day_period(period_start = "08:00", period_end = "09:00") %>%
  gtfs2gps::gtfs2gps(parallel = F) %>%
  gtfs2gps::gps_as_sfpoints()
curi$bus <- 1

curi <- curi[as.numeric(curi$speed) < 100, ]
bmc <- st_read("../../shapefiles/limits/CSA_NSA_Tracts.shp")
bmc <- st_union(bmc)
bmc <- sfheaders::sf_remove_holes(bmc)
plot(bmc)

# intersectar con red  y luego agregar por id
curi <- st_transform(curi, st_crs(bmc))
x <- st_transform(x, st_crs(bmc))

dim(curi)
curi <- st_crop(curi, bmc)
dim(curi)

curib <- st_buffer(curi, 20)
curib$id <- 1:nrow(curib)

curit <- st_intersection(curib["bus"], x["id"])

dim(curit)
setDT(curit)
xx <- curit[, sum(bus), by = id]
names(xx)[2] <- "bus"
x <- merge(x, xx, by = "id", all.x = T)
plot(x["bus"], axes = T, pal = cptcity::cpt(colorRampPalette = T, rev = T))

# comparison pop moves ####
pop <- readxl::read_excel("config/inventory_MD.xlsx", "pop_moves")
setDT(pop)
fraction_bus <- pop[sourceTypeID %in% 42:43, sum(sourceTypePopulation)]/sum(pop$sourceTypePopulation)

y <- fraction_bus * ( pop[!sourceTypeID %in% 42:43, sum(sourceTypePopulation)]) / sum(x$bus, na.rm = T)

sum(x$bus * y, na.rm = T) / (pop[!sourceTypeID %in% 42:43, sum(sourceTypePopulation)])

x$bus <- x$bus * y
x$BUS_TRANSIT <- x$bus * pop[!sourceTypeID %in% 42, sum(sourceTypePopulation)] /  pop[!sourceTypeID %in% 42:43, sum(sourceTypePopulation)]
x$BUS_SCHOOL <- x$bus * pop[!sourceTypeID %in% 43, sum(sourceTypePopulation)] /  pop[!sourceTypeID %in% 42:43, sum(sourceTypePopulation)]
net <- x


# GTFS BUS INTERCITY ####
p2 <- "../GTFS/mdotmta_gtfs_commuterbus.zip"
curi <- gtfs2gps::read_gtfs(p2) %>%
  gtfs2gps::filter_day_period(period_start = "08:00", period_end = "09:00") %>%
  gtfs2gps::gtfs2gps(parallel = F) %>%
  gtfs2gps::gps_as_sfpoints()
curi$bus <- 1

curi <- curi[as.numeric(curi$speed) < 100, ]
bmc <- st_read("../../shapefiles/limits/CSA_NSA_Tracts.shp")
bmc <- st_union(bmc)
bmc <- sfheaders::sf_remove_holes(bmc)
plot(bmc)

# intersectar con red  y luego agregar por id
curi <- st_transform(curi, st_crs(bmc))
x <- st_transform(x, st_crs(bmc))

dim(curi)
curi <- st_crop(curi, bmc)
dim(curi)

curib <- st_buffer(curi, 20)
curib$id <- 1:nrow(curib)

curit <- st_intersection(curib["bus"], x["id"])

dim(curit)
setDT(curit)
xx <- curit[, sum(bus), by = id]
names(xx)[2] <- "bus2"
x <- merge(x, xx, by = "id", all.x = T)
plot(x["bus2"], axes = T, pal = cptcity::cpt(colorRampPalette = T, rev = T))

# comparison pop moves ####
pop <- readxl::read_excel("config/inventory_MD.xlsx", "pop_moves")
setDT(pop)
fraction_bus <- pop[sourceTypeID %in% 41, sum(sourceTypePopulation)]/sum(pop$sourceTypePopulation)

y <- fraction_bus * ( pop[!sourceTypeID %in% 41, sum(sourceTypePopulation)]) / sum(x$bus2, na.rm = T)

sum(x$bus2 * y, na.rm = T) / (pop[!sourceTypeID %in% 41, sum(sourceTypePopulation)])

x$bus2 <- x$bus2 * y
x$BUS_INTERCITY <- x$bus2

x
st_write(x, "network/net_baltimore_city.gpkg", delete_layer = T)
rm(list = ls())
gc()
