options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(ggthemes)
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 3.2 Emissions per distance ####
language <- "english" # portuguese spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
speed <- readRDS("network/speed_net.rds")
speed_bin <- readRDS("network/speed_bin.rds")
fuel_type <- readRDS("config/fuel_type.rds")
lkm <- net$lkm
verbose <- FALSE

# SQL MOVES PARAMETERS
host <- "localhost"
moves_db <- "baltimore_2017_aug_emissionfactor" # _mi_aft _mo
user <- "moves"
password <- "moves"
port <- 3307
source("scripts/emissions_per_distance.R", encoding = "UTF-8")
rm(list = ls())
gc()
