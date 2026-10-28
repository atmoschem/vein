options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# plots
language <- "spanish" # english spanish portuguese
fuel <- readRDS("config/fuel.rds")
provincia <- unique(fuel$region)[1]
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
year <- 2019
source("scripts/plots.R", encoding = "UTF-8")
