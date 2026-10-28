options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# Exhaust ####
language <- "spanish" # english spanish portuguese
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
speed <- readRDS("network/speed.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
verbose <- FALSE
year <- 2017
nt <- 1 # check_nt() / 2
remove_fuel <- c("ELEC", "HY", "GLP")

IM <- FALSE
im_ok <- readRDS("config/im_ok.rds")
im_co <- readRDS("config/im_co.rds")
im_hc <- readRDS("config/im_hc.rds")
im_nox <- readRDS("config/im_nox.rds")
im_pm <- readRDS("config/im_pm.rds")

pol <- c(
  "CO",
  "HC",
  "NMHC",
  "NOx",
  "CO2",
  "PM",
  "NO2",
  "NO"
)
source("scripts/hot_exhaust_eea.R", encoding = "UTF-8")

pol <- c(
  "CO",
  "NOx",
  "NMHC",
  "HC",
  "NO",
  "NO2"
)
source("scripts/cold_start_eea.R", encoding = "UTF-8")
# rm(list = ls())
# gc()
