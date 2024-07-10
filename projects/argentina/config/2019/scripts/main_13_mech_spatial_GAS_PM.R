

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



# # MECH spatial GAS PM  ####
# g <- read_stars("../../cams/CAMS-GLOB-ANT_v4.2_carbon-monoxide_2017.nc", 
#                 sub = "tro")
# g <- eixport::wrf_grid("../../wrf/wrfinput_d01")
d <- 1
language <- "portuguese" # english spanish
for(kk in 1:2){

  g <- readRDS(paste0("../../wrf/gd0", kk, ".rds"))
  month <- 7
  g <- st_as_sf(g)
  st_crs(g) <- 4326
  mech <- "CBMZ"
  dom <- paste0("d0", kk, "_")

  gas <- c("CO",
           "CO2",
           "NO2",
           "NO",
           "SO2",
           "CH4",
           "NH3",
           "N2O")

  mw <- c(28.01,
          44.01,
          46.0055,
          30.01,
          64.066,
          16.04,
          17.031,
          44.013)

  pms <- c("PM",
           "PM10")

  aer <- "pm2023"

  source("scripts/mech3_spatial_GAS_PM.R",
         encoding = "UTF-8",
         echo = F)
}
rm(list = ls())
gc()


