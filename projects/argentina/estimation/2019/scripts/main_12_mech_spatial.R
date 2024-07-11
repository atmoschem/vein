

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


# MECH spatial ####
language <- "portuguese" # english spanish
# g <- read_stars("../../cams/CAMS-GLOB-ANT_v4.2_carbon-monoxide_2017.nc", 
#                 sub = "tro")
# g <- eixport::wrf_grid("../../wrf/wrfinput_d01")

for (me in 1:2){

 g <- readRDS(paste0("../../wrf/gd0", me, ".rds"))

 month <- 7

 g <- st_as_sf(g)

 st_crs(g) <- 4326

 mech <- "CBMZ"

 dom <- paste0("d0", me, "_")

source("scripts/mech3_spatial_v2.R",
       encoding = "UTF-8",
       echo = F)
}

rm(list = ls())
gc()



