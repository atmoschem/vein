options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(ggthemes)
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 4) Post-estimation ####
data(decoder)
metadata <- readRDS("config/metadata.rds")
language <- "english" # portuguese spanish
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
grid_width <- 200 # mts
crs <- 6487
source("scripts/post.R", encoding = "UTF-8", echo = TRUE)
rm(list = ls())
gc()
