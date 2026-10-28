options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(ggthemes)
library(eixport) # WRF Chem
library(data.table) # blasting speed

# 2) Traffic ####
language <- "english" # portuguese spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
categories <- unique(metadata$vein_name) # in net
verbose <- FALSE
year <- 2019
theme <- "ink" # dark clean ink
speed <- TRUE
ps <- "CONGSPD"
ffs <- "FFSPEED"
capacity <- "CAPE"
dist <- "miles"
k_D <- k_CNG <- k_G <- k_E85 <- 1
survival <- FALSE
source("scripts/traffic.R", encoding = "UTF-8")
rm(list = ls())
gc()
