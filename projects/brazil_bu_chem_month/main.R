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
path <- "config/inventory_month.xlsx"
readxl::excel_sheets(path) # For libre office, readODS::read_ods()
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel <- readxl::read_xlsx(path = path, sheet = "fuel_month")
met <- readxl::read_xlsx(path = path, sheet = "met")
s <- readxl::read_xlsx(path = path, sheet = "s")
standard <- readxl::read_xlsx(path = path, sheet = "standard")
year <- 2018
month <- 7
theme <- "black" # dark clean ink
scale <- "default"
delete_directories <- TRUE
source("config/config_month.R", encoding = "UTF-8")

# 1) Network ####
language <- "portuguese" # english spanish
net <- sf::st_read("network/cetspransv2.gpkg")
crs <- 31983
tit <- "Fluxo veicular [veh/h] em São Paulo"
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
source("scripts/net.R", encoding = "UTF-8")

# 2) Traffic ####
language <- "portuguese" # english spanish
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
k_G <- k_E <- k_D <- 1
verbose <- FALSE
year <- 2018
theme <- "black" # dark clean ink
source("scripts/traffic.R", encoding = "UTF-8")

# 3) Estimation ####
language <- "portuguese" # english spanish
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
met <- readRDS("config/met.rds")
net <- readRDS("network/net.rds")
lkm <- net$lkm
scale <- "tunnel"
verbose <- FALSE
year <- 2018
month <- 7

# Fuel eval
language <- "portuguese" # english spanish
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
factor_emi <- dmonth(year, month) / (nrow(tfs) / 24) # daily to annual
source("scripts/fuel_eval_month.R", encoding = "UTF-8")

# Exhaust
language <- "portuguese" # english spanish
pol <- c(
    "CO", "HC", "NMHC", "NO",  "SO2",
    "PM", "NO2", "ETOH", "NH3"
)
source("scripts/exhaust.R", encoding = "UTF-8")

# Evaporatives
source("scripts/evaporatives.R", encoding = "UTF-8")

# ressuspensao gera PM e PM10
language <- "portuguese" # english spanish
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
source("scripts/pavedroads.R", encoding = "UTF-8")
# llegue hasta aqui

# 4) Post-estimation ####
language <- "portuguese" # english spanish
year <- 2018
month <- 7
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
pol <- c(
    "CO", "HC", "ETOH", "SO2",
    "PM", "PM10",
    "NO2", "NO",
    "NH3",
    "D_NMHC", "G_NMHC", "E_NMHC",
    "G_EVAP_01", "E_EVAP_01"
) # Month October
g <- eixport::wrf_grid("wrf/wrfinput_d02")
# Number of lat points 51
# Number of lon points 63
crs <- 31983
factor_emi <- dmonth(year, month) / (nrow(tfs) / 24) # daily to annual
source("scripts/post.R", encoding = "UTF-8")

# # plots
language <- "portuguese" # english spanish
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
pol <- c("CO", "HC", "NO","NO2",   "PM")
year <- 2018
month <- 7
factor_emi <- dmonth(year, month) / (nrow(tfs) / 24) # daily to annual
hours <- 8
bg <- "white"
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks <- "quantile" # "sd" "quantile" "pretty"
tit <- "Emissões veiculares em São Paulo"
source("scripts/plots.R")

# MECH ####
language <- "english" # english spanish
month <- 7
evap <- c("G_EVAP_01", "E_EVAP_01")
g <- eixport::wrf_grid("wrf/wrfinput_d02")
pol <- c("CO", "NO", "NO2", "SO2")
mol <- c(12, 14 + 16, 14 + 16 * 2, 32 + 16 * 2)
aer <- "pmneu2" # pmiag, pmneu
# mech <- "iag" # iag_cb05v2, neu_cb05, iag_racm
# source("scripts/mech.R", encoding = "UTF-8")
mech <- "CBMZ" # "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1"
# option 2 (if cb05-=> ecb05_opt2)
source("scripts/mech.R", encoding = "UTF-8")

# WRF CHEM
language <- "portuguese" # english spanish
net <- readRDS("network/net.rds")
mech <- "CBMZ"
month <- 7
cols <- 63
rows <- 51
wrf_times <- 24 # ?
data("emis_opt") # names(emis_opt)
pasta_wrfinput <- ".."
pasta_wrfchemi <- "wrf"
wrfi <- "wrf/wrfinput_d02"
domain <- 2
lt_emissions <- "2011-07-25 00:00:00"
hours <- 0
source("scripts/wrf.R", encoding = "UTF-8")