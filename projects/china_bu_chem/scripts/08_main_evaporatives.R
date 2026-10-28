options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# Evaporatives
metadata <- readRDS("config/metadata.rds")
year <- 2022 # also the deterioration year (yeardet) in emis_china
pol <- c("Evaporative_driving",
         "Evaporative_parking")
source("scripts/evaporatives.R", encoding = "UTF-8")
beepr::beep(2)
