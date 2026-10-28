options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed
library(units)

# 2b) Speed ####
language <- "portuguese" # english spanish
net <- readRDS("network/net.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
tfs <- readRDS("config/tfs.rds")
verbose <- FALSE
theme <- "black" # dark clean ink
source("scripts/speed.R", encoding = "UTF-8")
rm(list = ls())
gc()
