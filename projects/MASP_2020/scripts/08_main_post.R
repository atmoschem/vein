options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 4) Post-estimation ####
language <- "portuguese" # english spanish
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
# pol <- c(
#    "ETOH",
#    "CO", "HC", "NOx", "CO2", "SO2",
#    "N2O", "CH4", "NH3",
#    "PM", "PM10",
#    "NO2", "NO",
#    "D_NMHC", "G_NMHC", "E_NMHC",
#    "G_EVAP_01", "E_EVAP_01"
# )
pol <- c(
    "ETOH",
    "CO", "SO2",
    "NH3",
    "PM", "PM10",
    "NO2", "NO",
    "D_NMHC", "G_NMHC", "E_NMHC",
    "G_EVAP_01", "E_EVAP_01"
)
# Month October
g <- eixport::wrf_grid("wrf/wrfinput_d02")
# Number of lat points 48
# Number of lon points 60
crs <- 3857
source("scripts/post.R", encoding = "UTF-8")
