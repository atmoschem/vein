

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


# 2) Traffic ####
language <- "english" # spanish portuguese
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- TRUE
theme <- "black" # dark clean ink
survival   <- TRUE
fuel <- readRDS("config/fuel.rds")
source("scripts/traffic.R", encoding = "UTF-8")
rm(list = ls())
gc()


