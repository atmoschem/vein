

options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(stars)
library(eixport)
sessionInfo()

# 0 Configuration
# brazil
language <- "english" # spanish portuguese
path <- "../../config/inventory.xlsx"
readxl::excel_sheets(path)
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readRDS("../../config/rds/mileage.rds")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")


veh <- readRDS("../../config/rds/fleet_age.rds")

# assuming fuel 2019
fuel_month <- readxl::read_xlsx(path = path, sheet = "fuel_month")
# columns used: date, FUEL_M3, density_tm3, type, fuel
fuel_month$Year <- 2019
s <- readxl::read_xlsx(path = path, sheet = "s")
fuel_spec <- readxl::read_xlsx(path = path, sheet = "fuel_spec")

met <-  readRDS("../../config/rds/met.rds")

euro <- readxl::read_xlsx(path = path, sheet = "euro")
tech <- readxl::read_xlsx(path = path, sheet = "tech")
col_region <- "region" # esta columna debe estar presente en fuel y met
scale <- "none"
theme <- "black" # dark clean ing
delete_directories <- TRUE

im_ok <- readxl::read_xlsx(path = path, sheet = "im_ok")
im_co <- readxl::read_xlsx(path = path, sheet = "im_co")
im_hc <- readxl::read_xlsx(path = path, sheet = "im_hc")
im_nox <- readxl::read_xlsx(path = path, sheet = "im_nox")
im_pm <- readxl::read_xlsx(path = path, sheet = "im_pm25")


source("config/config.R")

rm(list = ls())
gc()


