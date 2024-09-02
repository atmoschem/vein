

options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(eixport)
sessionInfo()




# Exhaust ####
language <- "spanish" # english spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
verbose <- FALSE

fuel <- readRDS("config/fuel.rds")

pol <- c(
  "CO", "HC", "NMHC", "NOx", "CO2",
  "PM", "NO2", "NO", "CH4"
)

IM <- FALSE

source("scripts/hot_exhaust_eea.R")
rm(list = ls())
gc()



