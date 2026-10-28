options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 4) Post-estimation ####
language <- "english" # portugueseenglish spanish
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
pol <- c(
    "CO", "HC", "NOx", 
    "PM2.5", "PM10") #generate NMHC by fuel
pol <- "CO"
crs <- 32648 # WGS 84 / UTM zone 48N
source("scripts/post.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()
