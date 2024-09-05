
options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(stars)
library(eixport)
sessionInfo()

# Tyres, Breaks and Road
language <- "english" #portuguese english spanish
metadata <- readRDS("config/metadata.rds")
mileage  <- readRDS("config/mileage.rds")
tfs      <- readRDS("config/tfs.rds")
# net      <- readRDS("network/net.rds")
veh      <- readRDS("config/fleet_age.rds")
pmonth <- readRDS("config/pmonth.rds")
# pol <- c("TSP", "PM10", "PM2.5", "PM1",  "PM0.1")
pol      <- c("PM2.5", "PM10")

verbose  <- FALSE
maxage <- 40
fuel <- readRDS("config/fuel.rds")
source("scripts/wear_eea.R", encoding = "UTF-8")
rm(list = ls())
gc()



