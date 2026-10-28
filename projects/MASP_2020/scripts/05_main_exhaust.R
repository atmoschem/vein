options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# Exhaust
language <- "english" # english spanish
pol <- c(
    "ETOH", "CO", "NMHC",
    "PM", "NO2", "NO", "SO2", "NH3"
)
# for a comprehensive estimation, uncomment the following lines
# pol <- c(
#    "ETOH", "CO", "HC", "NMHC", "NOx", "CO2",
#    "PM", "NO2", "NO", "SO2", "CH4", "NH3", "N2O"
# )

scale <- "tunnel2018"
source("scripts/exhaust.R", encoding = "UTF-8")
