options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# plots ####
print("plots")
language <- "spanish" # english portuguese
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")

pol <- c("CO", "NMHC_EXHAUST_G", "NO", "NO2", "PM2.5")
year <- 2024
factor_emi <- 365 / (nrow(tfs) / 24) # hourly to annual
hours <- 8
bg <- "white"
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks <- "quantile" # "sd" "quantile" "pretty"
tit <- "Emisiones en Quito, Ecuador"
source("scripts/plots.R")

# plots2 ####
source("scripts/plots2.R", encoding = "UTF-8")
rm(list = ls())
gc()
