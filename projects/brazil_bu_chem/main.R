options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed
sessionInfo()

# 0 Configuration
language <- "portuguese" # english spanish
path     <- "config/inventory.xlsx"
readxl::excel_sheets(path) # For libre office, readODS::read_ods()
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage  <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs      <- readxl::read_xlsx(path = path, sheet = "tfs")
veh      <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel     <- readxl::read_xlsx(path = path, sheet = "fuel")
met      <- readxl::read_xlsx(path = path, sheet = "met")
s        <- readxl::read_xlsx(path = path, sheet = "s")
im_ok    <- readxl::read_xlsx(path = path, sheet = "im_ok")
im_co    <- readxl::read_xlsx(path = path, sheet = "im_co")
im_hc    <- readxl::read_xlsx(path = path, sheet = "im_hc")
im_nox   <- readxl::read_xlsx(path = path, sheet = "im_nox")
im_pm    <- readxl::read_xlsx(path = path, sheet = "im_pm")
year     <- 2018
theme    <- "black" # dark clean ink
scale    <- "default"
delete_directories <- TRUE
source("config/config.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 1) Network ####
language   <- "portuguese" # english spanish
net        <- sf::st_read("network/net.gpkg")
crs        <- 31983
tit        <- "Fluxo veicular [veh/h] em São Paulo"
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
source("scripts/net.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 2) Traffic ####
language   <- "portuguese" # english spanish
net        <- readRDS("network/net.rds")
metadata   <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh        <- readRDS("config/fleet_age.rds")
verbose    <- FALSE
theme      <- "black" # dark clean ink
k_G <- k_E <- k_D <- 1
survival   <- FALSE
source("scripts/traffic.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 3) Estimation ####
language <- "portuguese" # english spanish
metadata <- readRDS("config/metadata.rds")
mileage  <- readRDS("config/mileage.rds")
tfs      <- readRDS("config/tfs.rds")
veh      <- readRDS("config/fleet_age.rds")
met      <- readRDS("config/met.rds")
net      <- readRDS("network/net.rds")
lkm      <- net$lkm
verbose  <- FALSE
s        <- readRDS("config/s.rds")

# Fuel eval
language   <- "portuguese" # english spanish
fuel       <- readRDS("config/fuel.rds")
pol        <- "FC"
factor_emi <- 365 / (nrow(tfs) / 24) # daily to annual
source("scripts/fuel_eval.R", encoding = "UTF-8")
rm(list = ls())
gc()

# Exhaust
language <- "portuguese" # english spanish
metadata <- readRDS("config/metadata.rds")
mileage  <- readRDS("config/mileage.rds")
tfs      <- readRDS("config/tfs.rds")
veh      <- readRDS("config/fleet_age.rds")
met      <- readRDS("config/met.rds")
net      <- readRDS("network/net.rds")
lkm      <- net$lkm
scale    <- "tunnel"
verbose  <- FALSE
s        <- readRDS("config/s.rds")
fuel     <- readRDS("config/fuel.rds")
IM       <- FALSE
pol      <- c(
  "CO", "HC", "NMHC", "NOx", "CO2", 
  "PM", "NO2", "NO", "NH3", "SO2", "ETOH",
  "N2O", "CH4"
)
source("scripts/exhaust.R", encoding = "UTF-8")
rm(list = ls())
gc()

# Evaporatives
language <- "portuguese" # english spanish
metadata <- readRDS("config/metadata.rds")
mileage  <- readRDS("config/mileage.rds")
tfs      <- readRDS("config/tfs.rds")
veh      <- readRDS("config/fleet_age.rds")
met      <- readRDS("config/met.rds")
net      <- readRDS("network/net.rds")
lkm      <- net$lkm
verbose  <- FALSE
source("scripts/evaporatives.R", encoding = "UTF-8")
rm(list = ls())
gc()

# Tyres, Breaks and Road
language <- "english" #portuguese english spanish
metadata <- readRDS("config/metadata.rds")
mileage  <- readRDS("config/mileage.rds")
tfs      <- readRDS("config/tfs.rds")
net      <- readRDS("network/net.rds")
veh      <- readRDS("config/fleet_age.rds")
pol      <- c("PM2.5", "PM10")
verbose  <- FALSE
source("scripts/wear.R", encoding = "UTF-8")
rm(list = ls())
gc()


# 4) Post-estimation ####
language   <- "portuguese" # english spanish
net        <- readRDS("network/net.rds")
tfs        <- readRDS("config/tfs.rds")
g          <- eixport::wrf_grid("wrf/wrfinput_d02")
# Number of lat points 51
# Number of lon points 63
crs        <- 31983
factor_emi <- 365 / (nrow(tfs) / 24) # daily to annual
source("scripts/post.R", encoding = "UTF-8")
rm(list = ls())
gc()

# # plots
language   <- "portuguese" # english spanish
metadata   <- readRDS("config/metadata.rds")
tfs        <- readRDS("config/tfs.rds")
veh        <- readRDS("config/fleet_age.rds")
pol        <- c("CO", "HC", "NOx", "CO2", 
                "PM2.5", "PM10", "NH3")
factor_emi <- 365 / (nrow(tfs) / 24) # daily to annual
hours      <- 8
bg         <- "white"
pal        <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks     <- "quantile" # "sd" "quantile" "pretty"
tit        <- "Emissões veiculares em São Paulo"
source("scripts/plots.R")
rm(list = ls())
gc()

# MECH ####
mech <- "RADM2" # CB05opt2 "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1"
for( k in seq_along(mech)) {
  language <- "english" # english spanish
  net      <- readRDS("network/net.rds")
  g        <- eixport::wrf_grid("wrf/wrfinput_d02")
  type     <- 'grids' #streets
  pol      <- c("CO", "NO", "NO2", "SO2", "NH3")
  mol      <- c(12 + 16, 14 + 16, 14 + 16 * 2, 32 + 16 * 2, 14 + 3)
  aer      <- "pm2023" # pmiag, pmneu
  source("scripts/mech2.R", encoding = "UTF-8")
}
rm(list = ls())
gc()

# WRF CHEM
# type only grids
mech <- "RADM2"
for( k in seq_along(mech)) {
  language       <- "portuguese" # english spanish
  net            <- readRDS("network/net.rds")
  cols           <- 63
  rows           <- 51
  n_aero         <- 15
  wrf_times      <- 24 # 
  pasta_wrfinput <- "wrf"
  pasta_wrfchemi <- "wrf"
  wrfi           <- "wrf/wrfinput_d02"
  domain         <- 2
  hours          <- 0
  source("scripts/wrf.R", encoding = "UTF-8")
}

