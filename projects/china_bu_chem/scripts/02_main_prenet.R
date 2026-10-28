options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 0.1 prenet ####
language <- "english" # portuguese english spanish
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
source("scripts/prenet.R", encoding = "UTF-8")
beepr::beep(2)
