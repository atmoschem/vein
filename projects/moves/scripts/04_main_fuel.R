options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(ggthemes)
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 3) Estimation ####
language <- "english" # portuguese english spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
speed <- readRDS("network/speed_net.rds")
speed_bin <- readRDS("network/speed_bin.rds")
fuel <- readRDS("config/fuel.rds")
fuel_type <- readRDS("config/fuel_type.rds")
lkm <- net$lkm
units(net$lkm)$numerator # miles
verbose <- TRUE
year <- 2019

# SQL MOVES PARAMETERS
host <- "localhost"
moves_db <- "baltimore_2017_aug_emissionfactor"
user <- "moves"
password <- "moves"
port <- 3307

# 3.1 Fuel eval ####
source("scripts/fuel_eval.R", encoding = "UTF-8")
rm(list = ls())
gc()
