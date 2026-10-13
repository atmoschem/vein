options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# Wear ####
print("desgaste de frenos y neumaticos")
language <- "spanish" # english spanish portuguese
# provincia <- "PI"
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
speed <- readRDS("network/speed.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
tfs <- readRDS("config/tfs.rds")
verbose <- FALSE
year <- 2024
nt <- 32 # check_nt() / 2
source("scripts/wear_eea.R", encoding = "UTF-8")
rm(list = ls())
gc()
