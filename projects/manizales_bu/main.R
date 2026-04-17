###########################################################
###                                                    ####
###   ANNUAL EMISSIONS INVENTORY FOR MANIZALES CITY    ####
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

#wrf

download.file(
  url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/wrf_manizales.zip",
  destfile = "wrf.zip"
)
unzip(zipfile = "wrf.zip")
# 0 Configuration
language <- "spanish" # spanish portuguese english
path <- "config/inventory_manizales_im.xlsx"
readxl::excel_sheets(path)

metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel <- readxl::read_xlsx(path = path, sheet = "fuel")
fuel_spec <- readxl::read_xlsx(path = path, sheet = "fuel_spec")
met <- readxl::read_xlsx(path = path, sheet = "met")
euro <- readxl::read_xlsx(path = path, sheet = "euro")
tech <- readxl::read_xlsx(path = path, sheet = "tech")
im_ok <- readxl::read_xlsx(path = path, sheet = "im_ok")
im_co <- readxl::read_xlsx(path = path, sheet = "im_co")
im_hc <- readxl::read_xlsx(path = path, sheet = "im_hc")
im_nox <- readxl::read_xlsx(path = path, sheet = "im_nox")
im_pm <- readxl::read_xlsx(path = path, sheet = "im_pm25")
year <- 2017
month <- 6
agemax <- 40
provincia <- "MANIZALES"
col_region <- "region" # esta columna debe estar presente en fuel y met
scale <- "none"
theme <- "black" # dark clean ing
delete_directories <- TRUE
source("config/config.R", encoding = "UTF-8")

# 1) Network ####
net <- st_read("network/manizales_simu.gpkg")
crs <- 4326
categories <- c(
  "pc",
  "lcv",
  "trucks",
  "bus",
  "mc",
  "taxi",
  "capacity",
  "ps",
  "ffs"
)
source("scripts/net.R", encoding = "UTF-8")

# 2) Traffic ####
language <- "spanish" # english spanish portuguese
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
year <- 2017
theme <- "black" # dark clean ink
k_D <- 1
k_G <- 1

ispeed <- TRUE
tfs <- readRDS("config/tfs.rds")
ps <- "ps"
ffs <- "ffs"
capacity <- "capacity"
source("scripts/traffic.R", encoding = "UTF-8")

# 3) Estimation ####
language <- "spanish" # english spanish portuguese
tfs <- readRDS("config/tfs.rds")
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
speed <- readRDS("network/speed.rds")
verbose <- FALSE
year <- 2017
remove_fuel <- c("ELEC", "HY")

# fuel calibration with fuel consumption data
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
provincia <- "MANIZALES"
factor_emi <- 365 / (nrow(tfs) / 24) # hourly to annual
source("scripts/fuel_eval_eea.R", encoding = "UTF-8")
# rm(list = ls())
# gc()

# Exhaust ####
language <- "spanish" # english spanish portuguese
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
speed <- readRDS("network/speed.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
verbose <- FALSE
year <- 2019
nt <- 1 # check_nt() / 2
remove_fuel <- c("ELEC", "HY", "GLP")

IM <- FALSE
im_ok <- readRDS("config/im_ok.rds")
im_co <- readRDS("config/im_co.rds")
im_hc <- readRDS("config/im_hc.rds")
im_nox <- readRDS("config/im_nox.rds")
im_pm <- readRDS("config/im_pm.rds")

pol <- c(
  "CO",
  "HC",
  "NMHC",
  "NOx",
  "CO2",
  "PM",
  "NO2",
  "NO"
)
source("scripts/hot_exhaust_eea.R", encoding = "UTF-8")

pol <- c(
  "CO",
  "NOx",
  "NMHC",
  "HC",
  "NO2",
  "NO"
)
source("scripts/cold_start_eea.R", encoding = "UTF-8")
# rm(list = ls())
# gc()

# Evaporatives ####
language <- "spanish" # english spanish portuguese
provincia <- "MANIZALES"
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
verbose <- FALSE
year <- 2017
source("scripts/evaporatives_eea.R", encoding = "UTF-8")
# rm(list = ls())
# gc()

# paved roads ####
language <- "spanish" # english spanish
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
tfs <- readRDS("config/tfs.rds")
net <- readRDS("network/net.rds")
veh <- readRDS("config/fleet_age.rds")
lkm <- net$lkm
tf_PC <- tfs$PC_G
tf_LCV <- tfs$LCV_G
tf_TRUCKS <- tfs$TRUCKS_L_D
tf_BUS <- tfs$BUS_URBAN_D
tf_MC <- tfs$MC_150_G
sL1 <- 2.4 # silt [g/m^2] se ADT < 500 (CENMA CHILE) i
sL2 <- 0.7 # silt [g/m^2] se 500 < ADT < 5000 (CENMA CHILE)
sL3 <- 0.6 # silt [g/m^2] se 5000 < ADT < 10000 (CENMA CHILE)
sL4 <- 0.3 # silt [g/m^2] se ADT > 10000 (CENMA CHILE)
source("scripts/paved_roads.R", encoding = "UTF-8")

# Wear ####
language <- "spanish" # english spanish portuguese
provincia <- "MANIZALES"
# provincia <- unique(fuel$region)[as.numeric(basename(getwd()))]
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
euro <- readRDS("config/euro.rds")
tech <- readRDS("config/tech.rds")
fuel_spec <- readRDS("config/fuel_spec.rds")
tfs <- readRDS("config/tfs.rds")
verbose <- FALSE
year <- 2017
nt <- 1 # check_nt() / 2
source("scripts/wear_eea.R", encoding = "UTF-8")

# Post-estimation ####
language <- "spanish" # english spanish portuguese
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
g <- eixport::wrf_grid("wrf_manizales/wrfinput_d03")
factor_emi <- 365 / (nrow(tfs) / 24) # hourly to annual
# Number of lat points 100
# Number of lon points 110
crs <- 31983
source("scripts/post.R", encoding = "UTF-8")

# plots ####
language <- "spanish" # english portuguese
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
pol <- c("CO", "NMHC_EXHAUST_G", "NO", "NO2", "PM2.5")
year <- 2017
factor_emi <- 365 / (nrow(tfs) / 24) # hourly to annual
hours <- 8
bg <- "white"
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks <- "quantile" # "sd" "quantile" "pretty"
tit <- "Emisiones en Manizales, Colombia"
source("scripts/plots.R")


# MECH ####
# "CB4", "CB05","CB05opt2", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1" "CBMZ"
mechs <- c("CBMZ", "CB05", "S99", "RADM2")
for (z in seq_along(mechs)) {
  print(mechs[z])
  language <- "spanish" # portuguese english spanish
  net <- readRDS("network/net.rds")
  g <- eixport::wrf_grid("wrf/wrfinput_d03")
  pol <- c("CO", "NO", "NO2")
  mol <- c(12, 14 + 16, 14 + 16 * 2)
  aer <- "pmneu2" # pmiag, pmneu
  mech <- mechs[z]
  source("scripts/mech2.R", encoding = "UTF-8")
}


# WRF CHEM ####
mechs <- c("CBMZ", "S99", "RADM2", "CB05")
mechs <- "CBMZ"
for (z in seq_along(mechs)) {
  print(mechs[z])
  mech <- mechs[z]
  language <- "english" # english spanish
  tfs <- readRDS("config/tfs.rds")
  net <- readRDS("network/net.rds")
  cols <- 100
  rows <- 110
  wrf_times <- 168 # ?
  dir_wrfinput <- "wrf"
  dir_wrfchemi <- "wrf"
  domain <- 3
  hours <- 0
  io_style_emissions <- 1 # 2 for all the hours, 1 for two emission files 0-12z
  n_aero <- 15 # number of PM species
  rotate <- "cols" # see ?GriddedEmissionsArray
  source("scripts/wrf.R", encoding = "UTF-8")
}
