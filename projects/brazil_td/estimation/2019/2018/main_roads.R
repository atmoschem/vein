options(encoding = "UTF-8")
library(colorout)
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
sessionInfo()


roads_path <- "/home/sibarra/BR/brazil_roads"
crs <- 3857
osm_name <- "highway"
language <- "spanish" # english spanish portuguese
dir.create("post/streets")
source("scripts/post_roads.R", encoding = "UTF-8")
rm(list = ls())
gc()
