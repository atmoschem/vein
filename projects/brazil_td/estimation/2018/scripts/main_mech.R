
options(encoding = "UTF-8")
library(colorout)
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(stars)
library(eixport)
sessionInfo()


# # MECH spatial ####
language <- "portuguese" # english spanish
# g <- read_stars("../../cams/CAMS-GLOB-ANT_v4.2_carbon-monoxide_2017.nc",
#                 sub = "tro")
# g <- eixport::wrf_grid("../../wrf/wrfinput_d01")

source("scripts/mech_spatial_EDGAR.R",
        encoding = "UTF-8",
        echo = F)
rm(list = ls())
gc()


