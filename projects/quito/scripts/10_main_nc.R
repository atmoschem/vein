options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# NetCDF ####
print("netcdf")
language <- "spanish" # english spanish portuguese
year <- 2024
source("scripts/nc.R", encoding = "UTF-8")
rm(list = ls())
gc()
