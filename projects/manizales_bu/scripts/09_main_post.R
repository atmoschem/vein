options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# Post-estimation ####
language <- "spanish" # english spanish portuguese
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
g <- eixport::wrf_grid("wrf/wrfinput_d03")
factor_emi <- 365 / (nrow(tfs) / 24) # hourly to annual
# Number of lat points 100
# Number of lon points 110
crs <- 32618 # WGS 84 / UTM zone 18N (Manizales)
years <- 2017
source("scripts/post.R", encoding = "UTF-8")
rm(list = ls())
gc()
