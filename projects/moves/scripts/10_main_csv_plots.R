options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(ggthemes)
library(eixport) # WRF Chem
library(data.table) # blasting speed

# plots ####
language <- "english" # portuguese spanish
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
hours <- 8
bg <- "white"
pal <- "colo_lightningmccarl_into_the_night" # find more in ?cptcity::find_cpt
tit <- "Baltimore City"
source("scripts/plots.R")
rm(list = ls())
gc()
