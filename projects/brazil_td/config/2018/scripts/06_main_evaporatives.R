options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(eixport)

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
# temperature scenario for evaporatives: NULL = every scenario of the year,
# each labelled in type_emi (future years have several, e.g. "SSP 4.5")
evap_scenario <- NULL

source("scripts/evaporatives4.R", encoding = "UTF-8", echo = F)
rm(list = ls())
gc()
