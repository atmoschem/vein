

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




# Exhaust
language <- "portuguese" # english chinese spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
# net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
verbose <- FALSE
pol <- c(
  "CO", "HC", "NMHC", "NOx", "CO2",
  "PM", "NO2", "NO", "SO2", "CH4",
  "NH3", "CH4", "N2O", "ETOH"
)
scale <- "tunnel2018"
plot_ef <- FALSE

s <- readRDS("config/s.rds")
factors <- readRDS("config/factors.rds")

source("scripts/exhaust_sulfur.R", encoding = "UTF-8")
rm(list = ls())
gc()



