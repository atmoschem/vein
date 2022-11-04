
###########################################################
###                                                    ####
###   ANNUAL EMISSIONS INVENTORY FOR ECUADOR PROVINCE  ####
###                                                    ####
###########################################################
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
im_ok <- readxl::read_xlsx(path = path, sheet = "im_ok")
im_co <- readxl::read_xlsx(path = path, sheet = "im_co")
im_hc <- readxl::read_xlsx(path = path, sheet = "im_hc")
im_nox <- readxl::read_xlsx(path = path, sheet = "im_nox")
im_pm <- readxl::read_xlsx(path = path, sheet = "im_pm")
year <- 2019
month <- 6
agemax <- 40
provincia <- unique(fuel$region)[1]
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
col_region <- "region" # esta columna debe estar presente en fuel y met
scale <- "none"
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
source("scripts/traffic.R", encoding = "UTF-8")

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
provincia <- unique(fuel$region)[1]
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
nt <- 1 # check_nt() / 2
source("scripts/fuel_eval_eea.R", encoding = "UTF-8")
# rm(list = ls())
# gc()

# Exhaust ####
language <- "spanish" # english spanish portuguese
provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
verbose <- FALSE
year <- 2019
nt <- 1 # check_nt() / 2

pol <- c(
  "CO", "HC", "NMHC", "NOx", "CO2",
  "PM", "NO2", "NO"
)
IM <- FALSE
source("scripts/hot_exhaust_eea.R", encoding = "UTF-8")

pol <- c(
  "CO", "NOx", "NMHC", "HC", "NO2", "NO"
)
source("scripts/cold_start_eea.R", encoding = "UTF-8")
# rm(list = ls())
# gc()

# Evaporatives ####
language <- "spanish" # english spanish portuguese
provincia <- unique(fuel$region)[1]
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
verbose <- FALSE
year <- 2019
source("scripts/evaporatives_eea.R", encoding = "UTF-8")
# rm(list = ls())
# gc()


# Wear ####
language <- "spanish" # english spanish portuguese
provincia <- unique(fuel$region)[1]
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
verbose <- FALSE
year <- 2019
nt <- 1 # check_nt() / 2
source("scripts/wear_eea.R", encoding = "UTF-8")

# plots
language <- "spanish" # english spanish portuguese
provincia <- unique(fuel$region)[1]
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
year <- 2019
source("scripts/plots.R", encoding = "UTF-8")