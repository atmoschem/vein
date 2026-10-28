options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed
sessionInfo()

# 0 Configuration
language <- "english" # portuguese english spanish
path <- "config/inventory_chn_bu_v5.xlsx"
readxl::excel_sheets(path) # For libre office, readODS::read_ods()
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
metadata <- metadata[metadata$exist == "yes",]
mileage <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel <- readxl::read_xlsx(path = path, sheet = "fuel")
met <- readxl::read_xlsx(path = path, sheet = "met")
std <- readxl::read_xlsx(path = path, sheet = "std")
h <- readxl::read_xlsx(path = path, sheet = "h")
year <- 2022
theme <- "black" # dark clean ink
delete_directories <- TRUE
source("config/config.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()
