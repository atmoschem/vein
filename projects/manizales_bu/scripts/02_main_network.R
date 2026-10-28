options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# 1) Network ####
net <- st_read("network/manizales_simu.gpkg")
# net <- net[1:100, ] # debug subset (first 100 links only), disabled: the fuel
# calibration needs the whole network
crs <- 4326
categories <- c(
  "pc",
  "lcv",
  "trucks",
  "bus",
  "mc",
  "taxi",
  "capacity",
  "ps",
  "ffs"
)
source("scripts/net.R", encoding = "UTF-8")
rm(list = ls())
gc()
