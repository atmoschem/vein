options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed
library(units)

# 4) Post-estimation ####
language <- "portuguese" # english spanish
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
g <- eixport::wrf_grid("wrf/wrfinput_d02")
# Number of lat points 51
# Number of lon points 63
crs <- 31983
factor_emi <- 365 / (nrow(tfs) / 24) # daily to annual
pol <- c("CO", "HC", "NMHC", "NOx", "CO2", "PM",
         "NO2", "NO", "NH3", "SO2", "ETOH", "N2O", "CH4")
source("scripts/post_speed.R", encoding = "UTF-8")
rm(list = ls())
gc()
