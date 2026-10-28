options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(ggthemes)
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 3.2 Emissions per vehicle ####
language <- "english" # english spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
speed <- readRDS("network/speed_net.rds")
speed_bin <- readRDS("network/speed_bin.rds")
fuel_type <- readRDS("config/fuel_type.rds")
lkm <- net$lkm
verbose <- TRUE

path_ef_distance <- "csv/ef_rateperdistance.csv.gz"
path_ef_vehicle <- "csv/ef_ratepervehicle.csv.gz"

source("scripts/emissions_per_vehicle_csv.R", encoding = "UTF-8")
rm(list = ls())
gc()
