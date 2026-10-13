options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# Evaporatives ####
print("Evaporativas")
language <- "spanish" # english spanish portuguese
# provincia <- "PI"
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tfs <- readRDS("config/tfs.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
verbose <- FALSE
year <- 2024
source("scripts/evaporatives_eea.R", encoding = "UTF-8")
rm(list = ls())
gc()
