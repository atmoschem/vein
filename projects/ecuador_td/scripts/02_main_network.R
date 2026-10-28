options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# 1) Network ####
net <- st_read("network/ecuador_provincias.gpkg")
crs <- 4326
source("scripts/net.R", encoding = "UTF-8")
rm(list = ls())
gc()
