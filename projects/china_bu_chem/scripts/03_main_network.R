options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 1) Network ####
language <- "english" # portuguese english spanish
net <- sf::st_read("network/net.gpkg")
crs <- 32648 # WGS 84 / UTM zone 48N
tit <- "Vehicular volume [veh/h] in Beijing, China"
metadata <- readRDS("config/metadata.rds")
source("scripts/net.R", encoding = "UTF-8")
beepr::beep(2)
