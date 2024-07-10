

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


#4) Post-estimation ####
# roads <- readRDS("network/net.rds")

crs <- 3857
osm_name <- "highway"
language <- "spanish" # english spanish portuguese
source("scripts/post2.R", encoding = "UTF-8")
rm(list = ls())
gc()



