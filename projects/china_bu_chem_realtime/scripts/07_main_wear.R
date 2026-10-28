options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# Tyres, Breaks and Road
language <- "english" #portuguese english spanish
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
tfs <- readRDS("config/tfs.rds")
speed <- as.data.frame(net[["speed"]])
veh <- readRDS("config/fleet_age.rds")
pol <- c("PM2.5", "PM10")
verbose <- FALSE
source("scripts/wear.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()
