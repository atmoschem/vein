
options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(stars)
library(eixport)
sessionInfo()



# 5) Post-estimation grids ####
# g <- read_stars("../../cams/CAMS-GLOB-ANT_v4.2_carbon-monoxide_2017.nc", 
# sub = "tro")

# g <- eixport::wrf_grid("../../wrf/wrfinput_d01")
# Number of lat points 179
# Number of lon points 179
# saveRDS(g, "../../wrf/gd01.rds")

# g <- eixport::wrf_grid("../../wrf/wrfinput_d02")
# using grid info from: ../../wrf/wrfinput_d02 
# Number of lat points 198
# Number of lon points 219
# saveRDS(g, "../../wrf/gd02.rds")
for(gg in 1:2) {

 g <- readRDS(paste0("../../wrf/gd0",gg, ".rds"))

 g <- st_as_sf(g)

 st_crs(g) <- 4326

 crs <- 3857

 language <- "spanish" # english spanish portugues

 pols <- c("CO",
          "HC",
          "NOx",
          "CO2",
          "PM",
          "NO2",
          "NO",
          "SO2",
          "CH4",
          "ETOH",
          "NH3",
          "N2O",
          "PM10",
          "NMHC_G_EXHAUST",
          "NMHC_E_EXHAUST",
          "NMHC_D_EXHAUST",
          "NMHC_G_EVAPORATIVES_HISTORIC",
          "NMHC_E_EVAPORATIVES_HISTORIC")

 dom <- paste0("d0", gg, "_")

 month <- 7

 source("scripts/post_grid.R", encoding = "UTF-8")

}

rm(list = ls())
gc()



