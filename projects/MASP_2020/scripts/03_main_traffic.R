options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 2) Traffic ####
# to improve your inventory, improve your input data
language <- "portuguese" # english spanish
net <- st_read("network/net.gpkg")
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
year <- 2020
theme <- "black" # dark clean ink
survival <- TRUE
source("scripts/traffic.R", encoding = "UTF-8")
