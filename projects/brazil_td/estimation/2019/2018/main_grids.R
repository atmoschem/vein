
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



# 5) Post-estimation grids ####
g <- read_stars("../../cams/CAMS-GLOB-ANT_v4.2_carbon-monoxide_2017.nc", 
sub = "tro")

g <- st_as_sf(g)

st_crs(g) <- 4326

crs <- 3857

language <- "spanish" # english spanish portugues

dir.create("post/grids")
source("scripts/post_grids.R", encoding = "UTF-8", echo = T)

rm(list = ls())
gc()

	
	
