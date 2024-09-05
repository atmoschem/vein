
options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(stars)
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

metadata[family == "PC", maxage := 40]
metadata[family == "LCV", maxage := 40]
metadata[family == "TRUCKS", maxage := 40]
metadata[family == "BUS", maxage := 40]
metadata[family == "MC", maxage := 40]
source("scripts/evaporatives_eea.R", encoding = "UTF-8")
rm(list = ls())
gc()

