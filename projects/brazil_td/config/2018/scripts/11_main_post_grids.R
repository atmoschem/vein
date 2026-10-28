options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(eixport)

#7) Post-estimation spatial ####
g <- eixport::wrf_grid("../../wrf/wrfinput_d02")
crs <- 31983
language <- "spanish" # english spanish portugues
dir.create("post/grids")
source("scripts/post_grids.R", encoding = "UTF-8", echo = F)
rm(list = ls())
gc()
