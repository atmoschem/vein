options(encoding = "UTF-8")
if (requireNamespace("colorout", quietly = TRUE)) library(colorout)
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
sessionInfo()


roads_path <- "../../brazil_roads/" # cloned by the project main.R
crs <- 3857
osm_name <- "highway"
language <- "spanish" # english spanish portuguese
dir.create("post/streets")
source("scripts/post_roads.R", encoding = "UTF-8")
rm(list = ls())
gc()
