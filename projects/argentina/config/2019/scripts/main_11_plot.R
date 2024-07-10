


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


# plots
language <- "spanish" # english spanish portuguese
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
pol <- c("CO", "HC", "NOx", "CO2", "PM", "NMHC")
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt

gpols <- c("CO",
           "HC",
           "NOx",
           "CO2",
           "PM",
           "NO2",
           "NO",
           "ETOH",
           "SO2",
           "CH4",
           "NH3",
           "N2O",
           "PM10",
           "NMHC_G",
           "NMHC_E",
           "NMHC_D")

source("scripts/plots.R", encoding = "UTF-8")
rm(list = ls())
gc()


