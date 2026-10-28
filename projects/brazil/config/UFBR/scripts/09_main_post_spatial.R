options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames

# #4) Post-estimation spatial ####
# roads <- readRDS("network/net.rds") # network/ is empty in UFBR.tar.gz: add net.rds first
# crs <- 3857
# osm_name <- "highway"
# language <- "spanish" # english spanish portuguese
# source("scripts/post2_spatial.R", encoding = "UTF-8")
# rm(list = ls())
# gc()
