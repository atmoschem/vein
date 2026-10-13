options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames
sessionInfo()

# 0 Configuration
print("0 configuracion")
language <- "spanish" # spanish portuguese english
path <- "config/inventory_ecuador_2026.xlsx"
readxl::excel_sheets(path)

metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel <- readxl::read_xlsx(path = path, sheet = "fuel")
(UFS <- unique(fuel$UF))
fuel_spec <- readxl::read_xlsx(path = path, sheet = "fuel_spec")
met <- readxl::read_xlsx(path = path, sheet = "met")
euro <- readxl::read_xlsx(path = path, sheet = "euro")
tech <- readxl::read_xlsx(path = path, sheet = "tech")
im_ok <- readxl::read_xlsx(path = path, sheet = "im_ok")
im_co <- readxl::read_xlsx(path = path, sheet = "im_co")
im_hc <- readxl::read_xlsx(path = path, sheet = "im_hc")
im_nox <- readxl::read_xlsx(path = path, sheet = "im_nox")
im_pm <- readxl::read_xlsx(path = path, sheet = "im_pm25")
year <- 2024
month <- 6
agemax <- 40
provincia <- "QU"
col_region <- "UF" # esta columna debe estar presente en fuel y met
scale <- "none"
theme <- "black" # dark clean ing
delete_directories <- FALSE
source("config/config.R", encoding = "UTF-8")
rm(list = ls())
gc()
