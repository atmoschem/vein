options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 1 Network ####
language <- "english" # portuguese english spanish
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
plots  <- TRUE
source("scripts/prenet_osm.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()
