

options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(eixport)
sessionInfo()


# 3) Estimation ####
language <- "portuguese" # english chinese spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
# net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
verbose <- FALSE
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
# fuel calibration with fuel consumption data
fuel <- readRDS("config/fuel.rds")
pol <- "FC"

source("scripts/fuel_eval_eea.R", encoding = "UTF-8")
rm(list = ls())
gc()


