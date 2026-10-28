options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(eixport)

# WRF ####
language <- "portuguese" # english spanish
wrfi <- "../../wrf/wrfinput_d02"
pasta_wrfinput <- "../../wrf/"
cols <- 63
rows <- 51
n_aero <- 15
wrf_times <- 24 #
hours <- 0

suppressWarnings(source(
  "scripts/wrf.R",
  encoding = "UTF-8",
  echo = T
))
rm(list = ls())
gc()
