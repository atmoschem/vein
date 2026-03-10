

options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
sessionInfo()



# Evaporative
language <- "portuguese" # english chinese spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
#net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
meto <- readRDS("config/met.rds")
verbose <- FALSE
maxage <- 40
scale <- "tunnel2018"
plot_ef <- F

source("scripts/evaporatives4.R", encoding = "UTF-8", echo = F)
rm(list = ls())
gc()


#4) Post-estimation ####
# roads <- readRDS("network/net.rds")
crs <- 3857
osm_name <- "highway"
language <- "spanish" # english spanish portuguese
source("scripts/post2.R", encoding = "UTF-8")
rm(list = ls())
gc()




