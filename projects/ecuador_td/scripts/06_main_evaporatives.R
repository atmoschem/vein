options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# Evaporatives ####
language <- "spanish" # english spanish portuguese
fuel <- readRDS("config/fuel.rds")
provincia <- unique(fuel$region)[1]
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
verbose <- FALSE
year <- 2019
source("scripts/evaporatives_eea.R", encoding = "UTF-8")
# rm(list = ls())
# gc()
