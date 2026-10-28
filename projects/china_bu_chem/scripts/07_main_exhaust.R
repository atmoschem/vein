options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# Exhaust
language <- "english" # portuguese english spanish
# fuel_eval.R leaves a filtered (no ELEC/HY) and modified metadata in the workspace
metadata <- readRDS("config/metadata.rds")
year <- 2022 # also the deterioration year (yeardet) in emis_china
pol <- c(
    "CO", "HC",  "NOx",# "CO2", "SO2",
    "PM10","PM2.5"
)
source("scripts/exhaust.R", encoding = "UTF-8")
beepr::beep(2)
