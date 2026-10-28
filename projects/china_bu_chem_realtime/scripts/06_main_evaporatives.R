options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

language <- "english" # english spanish portuguese
tfs <- readRDS("config/tfs.rds")
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
h <- readRDS("config/h.rds")
std <- readRDS("config/std.rds")
speed <- as.data.frame(net[["speed"]])
verbose <- FALSE
year <- 2022
verbose <- FALSE
remove_fuel <- c("ELEC", "HY")
# hour of the traffic snapshot (network/2023-06-12_09-04-06_traffic_volume.csv,
# 09:04): selects the met row (met$Hour is 0-23) for temperature and humidity
hour_snapshot <- 9
pol <- c("Evaporative_driving",
         "Evaporative_parking")
source("scripts/evaporatives.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()
