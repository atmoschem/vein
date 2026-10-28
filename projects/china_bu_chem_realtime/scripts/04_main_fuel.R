options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 3) Estimation ####
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

# Fuel consumption
language <- "english" # portuguese english spanish
source("scripts/fuel_eval.R", encoding = "UTF-8")
rm(list = ls())
gc()
