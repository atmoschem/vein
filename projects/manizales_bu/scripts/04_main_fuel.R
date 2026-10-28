options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# 3) Estimation ####
language <- "spanish" # english spanish portuguese
tfs <- readRDS("config/tfs.rds")
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
speed <- readRDS("network/speed.rds")
verbose <- FALSE
year <- 2017
remove_fuel <- c("ELEC", "HY")

# fuel calibration with fuel consumption data
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
provincia <- "MANIZALES"
factor_emi <- 365 / (nrow(tfs) / 24) # hourly to annual
source("scripts/fuel_eval_eea.R", encoding = "UTF-8", echo = F)
rm(list = ls())
gc()
