options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames
sessionInfo()

# for fun, install devtools::install_github('bbc/bbplot')
# devtools::install_github('bbc/bbplot')

# 0 Configuration
language <- "spanish" # spanish portuguese english
path <- "config/inventory_ecuador.xlsx"
readxl::excel_sheets(path)

metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel <- readxl::read_xlsx(path = path, sheet = "fuel")
fuel_spec <- readxl::read_xlsx(path = path, sheet = "fuel_spec")
pmonth <- readxl::read_xlsx(path = path, sheet = "fuel_month")
met <- readxl::read_xlsx(path = path, sheet = "met")
euro <- readxl::read_xlsx(path = path, sheet = "euro")
tech <- readxl::read_xlsx(path = path, sheet = "tech")
year <- 2019
month <- 6
agemax <- 40
provincia <- unique(fuel$region)[1]
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
col_region <- "region" # esta columna debe estar presente en fuel y met
scale <- "none"
theme <- "black" # dark clean ing
delete_directories <- TRUE

im_ok <- readxl::read_xlsx(path = path, sheet = "im_ok")
im_co <- readxl::read_xlsx(path = path, sheet = "im_co")
im_hc <- readxl::read_xlsx(path = path, sheet = "im_hc")
im_nox <- readxl::read_xlsx(path = path, sheet = "im_nox")
im_pm <- readxl::read_xlsx(path = path, sheet = "im_pm25")

source("config/config.R", encoding = "UTF-8")
rm(list = ls())
gc()
