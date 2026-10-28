options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
sessionInfo()

# 0 Configuration
language <- "english" # spanish portuguese
path <- "../../../config/inventory_all.xlsx"
readxl::excel_sheets(path)
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readRDS("../../../config/mileage.rds")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readRDS("../../../config/fleet_age.rds")
fuel <- readRDS("../../../config/fuel.rds")
pmonth <-  readRDS("../../../config/fuel_month.rds")
met <-  readRDS("../../../config/met.rds")
scale <- "tunnel2018"
theme <- "black" # dark clean ing
delete_directories <- TRUE
source("config/config.R", encoding = "UTF-8")
rm(list = ls())
gc()
