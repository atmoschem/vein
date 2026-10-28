options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(ggthemes)
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 1) Network ####
# source("network/pre_net_simu_BMC_qgisv2.R")
# source("network/pre_net_GTFS.R")
language <- "english" # english spanish portuguese
path <- "config/inventory_MD.xlsx"
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
net <- sf::st_read("network/net_baltimore_city.gpkg")
crs <- 6487
tit <- "Travel demand model outputs from Baltimore 2019 (veh/h)"
names(net)
categories <- unique(metadata$vein_name) # in network/net.gpkg
source("scripts/net.R", encoding = "UTF-8")
rm(list = ls())
gc()
