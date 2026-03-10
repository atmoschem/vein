options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(eixport)
sessionInfo()

# 0 Configuration
language <- "english" # spanish portuguese
path <- "../../config/inventory_all.xlsx"
readxl::excel_sheets(path)
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readRDS("../../config/mileage.rds")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readRDS("../../config/fleet_age.rds")
fuel <- readRDS("../../config/fuel.rds")
pmonth <- readRDS("../../config/fuel_month.rds")
met <- readRDS("../../config/met.rds")
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
survival <- TRUE
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
  "CO",
  "HC",
  "NMHC",
  "NOx",
  "NO",
  "NO2",
  "NH3",
  "PM",
  "SO2",
  "CO2",
  "CH4",
  "N2O",
  "ETOH"
)
scale <- "tunnel2018"
plot_ef <- FALSE
maxage <- 40
sfuel <- readRDS("../../config/fuel.rds")
source("scripts/exhaust.R", encoding = "UTF-8")

# Evaporative
language <- "portuguese" # english chinese spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
#net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
meto <- readRDS("config/met.rds")
verbose <- FALSE
maxage <- 40
scale <- "tunnel2018"
plot_ef <- F

source("scripts/evaporatives4.R", encoding = "UTF-8", echo = F)
rm(list = ls())
gc()


# Tyres, Breaks and Road
language <- "english" #portuguese english spanish
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
pmonth <- readRDS("config/pmonth.rds")
pol <- c("PM2.5", "PM10")
verbose <- FALSE
maxage <- 40
source("scripts/wear.R", encoding = "UTF-8")
rm(list = ls())
gc()


#4) Post-estimation ####
crs <- 3857
osm_name <- "highway"
language <- "spanish" # english spanish portuguese
source("scripts/post2.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 5 plots ####
language <- "spanish" # english spanish portuguese
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
pol <- c("CO", "HC", "NOx", "CO2", "PM", "NMHC")
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
source("scripts/plots.R", encoding = "UTF-8")
rm(list = ls())
gc()


# 6 Post roads
roads_path <- "../../brazil_roads/"
crs <- 3857
osm_name <- "highway"
language <- "portuguese" # english spanish portuguese
dir.create("post/streets")
source("scripts/post_roads.R", encoding = "UTF-8")
rm(list = ls())
gc()


#7) Post-estimation spatial ####
g <- eixport::wrf_grid("../../wrf/wrfinput_d02")
crs <- 31983
language <- "spanish" # english spanish portugues
dir.create("post/grids")
source("scripts/post_grids.R", encoding = "UTF-8", echo = F)
rm(list = ls())
gc()


# 8 MECH spatial ####
library(units)
language <- "portuguese" # english spanish
mech <- "RADM2"
aer <- "pm2023" # pmiag, pmneu
pol <- c("CO", "NO", "NO2", "SO2", "NH3")
mw <- c(12 + 16, 14 + 16, 14 + 16 * 2, 32 + 16 * 2, 14 + 3)
suppressWarnings(source(
  "scripts/mech_spatial.R",
  encoding = "UTF-8",
  echo = F
))
rm(list = ls())
gc()

# WRF ####
language <- "portuguese" # english spanish
wrfi <- "../../wrf/wrfinput_d02"
pasta_wrfinput <- "../../wrf/"
cols <- 63
rows <- 51
n_aero <- 15
wrf_times <- 24 #
hours <- 0

suppressWarnings(source(
  "scripts/wrf.R",
  encoding = "UTF-8",
  echo = T
))
rm(list = ls())
gc()
