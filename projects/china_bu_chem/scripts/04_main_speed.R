options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
source("scripts/speed.R", encoding = "UTF-8")
beepr::beep(2)
