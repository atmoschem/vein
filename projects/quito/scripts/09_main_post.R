options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# Post-estimation ####
print("pos-estimacion")
language <- "spanish" # english spanish portuguese
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
# g <- make_grid(net, width = 1000)
g <- eixport::wrf_grid("wrf/wrfinput_d04")
factor_emi <- 365 / (nrow(tfs) / 24) # hourly to annual
# Number of lat points 141
# Number of lon points 141
crs <- 31986
years <- 2024
source("scripts/post.R", encoding = "UTF-8")
rm(list = ls())
gc()
