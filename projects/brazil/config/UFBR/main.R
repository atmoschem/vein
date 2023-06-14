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


# 1) Network ####
# crs <- 3857
# source("scripts/net.R", encoding = "UTF-8")
# rm(list = ls())
# gc()

# 2) Traffic ####
language <- "english" # spanish portuguese
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- TRUE
theme <- "black" # dark clean ink
survival   <- TRUE
fuel <- readRDS("config/fuel.rds")
maxage <- 40
source("scripts/traffic.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 3) Estimation ####
language <- "portuguese" # english chinese spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
# net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
verbose <- FALSE
maxage <- 40

# fuel calibration with fuel consumption data
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
source("scripts/fuel_eval.R", encoding = "UTF-8")
rm(list = ls())
gc()


# Exhaust
language <- "portuguese" # english chinese spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
# net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
verbose <- FALSE
pol <- c(
  "CO", "HC", "NMHC", "NOx", "CO2",
  "PM", "NO2", "NO", "SO2", "CH4", "NH3", "CH4", "N2O"
)
scale <- "tunnel2018"
plot_ef <- FALSE
maxage <- 40
source("scripts/exhaust.R", encoding = "UTF-8")

# Evaporative
language <- "portuguese" # english chinese spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
verbose <- FALSE
maxage <- 40
scale <- "tunnel2018"
plot_ef <- F

source("scripts/evaporatives.R", encoding = "UTF-8", echo = F)
rm(list = ls())
gc()


# Tyres, Breaks and Road
language <- "english" #portuguese english spanish
metadata <- readRDS("config/metadata.rds")
mileage  <- readRDS("config/mileage.rds")
tfs      <- readRDS("config/tfs.rds")
net      <- readRDS("network/net.rds")
veh      <- readRDS("config/fleet_age.rds")
pmonth <- readRDS("config/pmonth.rds")
pol      <- c("PM2.5", "PM10")
verbose  <- FALSE
source("scripts/wear.R", encoding = "UTF-8")
rm(list = ls())
gc()


#4) Post-estimation ####
roads <- readRDS("network/net.rds")
crs <- 3857
osm_name <- "highway"
language <- "spanish" # english spanish portuguese
source("scripts/post2.R", encoding = "UTF-8")
rm(list = ls())
gc()

# #4) Post-estimation spatial ####
# roads <- readRDS("network/net.rds")
# crs <- 3857
# osm_name <- "highway"
# language <- "spanish" # english spanish portuguese
# source("scripts/post2_spatial.RR", encoding = "UTF-8")
# rm(list = ls())
# gc()



# plots
language <- "spanish" # english spanish portuguese
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
pol <- c("CO", "HC", "NOx", "CO2", "PM", "NMHC")
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
source("scripts/plots.R", encoding = "UTF-8")
rm(list = ls())
gc()


# # MECH spatial ####
# language <- "portuguese" # english spanish
# aer <- c(
#   e_so4i = 0.003672,
#   e_so4j = 0.023328,
#   e_no3i = 0.00345,
#   e_no3j = 0.01155,
#   e_pm25i = 0.04825,
#   e_pm25j = 0.14475,
#   e_orgi = 0.08284,
#   e_orgj = 0.35316,
#   e_eci = 0.30926,
#   e_ecj = 0.01974#,h2o = 0.277
# )
# pol <- c("CO", "NO", "NO2", "SO2", "CO2", "NH3", "CH4", "N2O")
# mol <- c(12, 14 + 16, 14 + 16 * 2, 32 + 16 * 2, 12 + 16 * 2, 14 + 3, 14 * 2 + 16, 14*2 + 16)
# mech <- c("CB05", "S99", "RADM2", "MOZT1", "RACM2") # actually RACM
# suppressWarnings(source("scripts/mech3_spatial.R", encoding = "UTF-8", echo = F))
# rm(list = ls())
# gc()
