options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 2) Traffic ####
language <- "english" # portuguese english spanish
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
theme <- "black" # dark clean ink
k_G <- k_CNG <- k_D <- 1 
survival <- TRUE
plots <- TRUE
source("scripts/traffic.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()
