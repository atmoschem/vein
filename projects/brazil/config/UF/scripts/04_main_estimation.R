options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames

# fuel calibration, exhaust and evaporatives run in one R session, as in
# main.R: exhaust.R and evaporatives.R use objects left by fuel_eval.R
# 3) Estimation ####
language <- "portuguese" # english chinese spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
verbose <- FALSE

# fuel calibration with fuel consumption data
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
source("scripts/fuel_eval.R", encoding = "UTF-8")

# Exhaust
pol <- c(
  "CO", "HC", "NMHC", "NOx", "CO2",
  "PM", "NO2", "NO", "SO2", "CH4", "NH3", "N2O"
)
scale <- "tunnel2018"
plot_ef <- FALSE
source("scripts/exhaust.R", encoding = "UTF-8")

# Evaporative
plot_ef <- FALSE
source("scripts/evaporatives.R", encoding = "UTF-8")
rm(list = ls())
gc()
