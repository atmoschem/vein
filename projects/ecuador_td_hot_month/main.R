options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames
sessionInfo()

# 0 Configuration
language <- "spanish" # spanish portuguese english
path_to_moves <- "../"
path <- "config/inventory_ecuador.xlsx"
readxl::excel_sheets(path)
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel <- readxl::read_xlsx(path = path, sheet = "fuel")
pmonth <- readxl::read_xlsx(path = path, sheet = "pmonth")
met <- readxl::read_xlsx(path = path, sheet = "met")
euro <- readxl::read_xlsx(path = path, sheet = "euro")
tech <- readxl::read_xlsx(path = path, sheet = "tech")
year <- 2019
month <- 6
agemax <- 40
provincia <- "Imbabura"
scale = "none"
theme <- "black" # dark clean ing
delete_directories <- TRUE
source("config/config.R", encoding = "UTF-8")

# 1) Network ####
net <- st_read("network/ecuador_provincias.gpkg")
crs <- 4326
source("scripts/net.R", encoding = "UTF-8")

# 2) Traffic ####
language <- "spanish" # english spanish portuguese
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
year <- 2019
theme <- "black" # dark clean ink
k_D <- 1
k_G <- 1
survival <- TRUE
source("scripts/traffic_month.R", encoding = "UTF-8")

# 3) Estimation ####
language <- "spanish" # english spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")

verbose <- FALSE
year <- 2019

# fuel calibration with fuel consumption data
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
provincia <- "Imbabura"
source("scripts/fuel_eval_eea.R", encoding = "UTF-8")
rm(list = ls())
gc()

# Exhaust ####
language <- "spanish" # english spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")

verbose <- FALSE
year <- 2019

pol <- c(
  "CO", "HC", "NMHC", "NOx", "CO2",
  "PM", "NO2", "NO"
)
provincia <- "Imbabura"
source("scripts/exhaust_eea.R", encoding = "UTF-8")

# plots
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
pol <- c("CO", "HC", "NOx", "CO2", "PM", "NMHC")
year <- 2019
bg <- "white"
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks <- "quantile" # "sd" "quantile" "pretty"
provincia <- "Imbabura"
source("scripts/plots.R", encoding = "UTF-8")
