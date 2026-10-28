options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed
library(units)

# Exhaust
language <- "portuguese" # english spanish
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
met <- readRDS("config/met.rds")
net <- readRDS("network/net.rds")
lkm <- net$lkm
year <- 2018 # base year, as in the configuration block
scale <- "tunnel"
verbose <- FALSE
s <- readRDS("config/s.rds")
fuel <- readRDS("config/fuel.rds")
IM <- FALSE
pol <- c(
  "CO",
  "HC",
  "NMHC",
  "NOx",
  "CO2",
  "PM",
  "NO2",
  "NO",
  "NH3",
  "SO2",
  "ETOH",
  "N2O",
  "CH4"
)
source("scripts/exhaust.R", encoding = "UTF-8")
rm(list = ls())
gc()
