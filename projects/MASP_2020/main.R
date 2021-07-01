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
path <- "config/inventory.xlsx"
readxl::excel_sheets(path) # For libre office, readODS::read_ods()
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel <- readxl::read_xlsx(path = path, sheet = "fuel")
met <- readxl::read_xlsx(path = path, sheet = "met")
year <- 2020
month <- 3
theme <- "black" # dark clean ink
scale <- "tunnel2018"
delete_directories <- TRUE
source("config/config.R", encoding = "UTF-8")

# 1) Network ####
language <- "portuguese" # english spanish
net <- sf::st_read("network/net.gpkg")
crs <- 4326
tit <- "Fluxo veicular [veh/h] em São Paulo"
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
source("scripts/net.R", encoding = "UTF-8")

# 2) Traffic ####
language <- "portuguese" # english spanish
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
year <- 2020
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
scale <- "tunnel2018"
verbose <- FALSE
year <- 2020

# Fuel eval
language <- "portuguese" # english spanish
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
source("scripts/fuel_eval.R", encoding = "UTF-8")

# Exhaust
language <- "english" # english spanish
pol <- c(
    "ETOH", "CO", "HC", "NMHC", "NOx", "CO2",
    "PM", "NO2", "NO", "SO2", "CH4", "NH3", "CH4", "N2O"
)
scale <- "tunnel2018"
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
sL1 <- 2.4 # silt [g/m^2] se ADT < 500 (CENMA CHILE) i
sL2 <- 0.7 # silt [g/m^2] se 500 < ADT < 5000 (CENMA CHILE)
sL3 <- 0.6 # silt [g/m^2] se 5000 < ADT < 10000 (CENMA CHILE)
sL4 <- 0.3 # silt [g/m^2] se ADT > 10000 (CENMA CHILE)
source("scripts/pavedroads.R", encoding = "UTF-8")

# 4) Post-estimation ####
language <- "portuguese" # english spanish
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
pol <- c(
    "ETOH",
    "CO", "HC", "NOx", "CO2", "SO2", 
    "N2O", "CH4", "NH3",
    "PM", "PM10",
    "NO2", "NO",
    "D_NMHC", "G_NMHC", "E_NMHC",
    "G_EVAP_01", "E_EVAP_01"
) # Month October
g <- eixport::wrf_grid("wrf/wrfinput_d02")
    # Number of lat points 152
    # Number of lon points 160
# g <- make_grid(st_transform(net, 3857), 500)
# Number of lon points: 47
# Number of lat points: 72
crs <- 3857
source("scripts/post.R", encoding = "UTF-8")

# plots ####
language <- "portuguese" # english spanish
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
pol <- c("CO", "HC", "NOx", "CO2", "PM", "NH3")
hours <- 8
bg <- "white"
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks <- "quantile" # "sd" "quantile" "pretty"
tit <- "Emissões veiculares em São Paulo"
source("scripts/plots.R")

# llegue aqui
# MECH ####
language <- "english" # english spanish
net <- readRDS("network/net.rds")
evap <- c("G_EVAP_01", "E_EVAP_01")
g <- eixport::wrf_grid("wrf/wrfinput_d02")
# g <- make_grid(st_transform(net, 3857), 500)
pol <- c("CO", "NO", "NO2", "SO2", "CO2", "NH3", "CH4", "N2O", "ETOH")
mol <- c(12, 14 + 16, 14 + 16 * 2, 32 + 16 * 2, 12 + 16 * 2, 14 + 3, 14 * 2 + 16, 12 + 4, 46) #C2H5OH

aer <- "pmneu2" # pmiag, pmneu
# mech <- "iag" # iag_cb05v2, neu_cb05, iag_racm
# source("scripts/mech.R", encoding = "UTF-8")
mech <- "CB05" # "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1" "CBMZ"^
source("scripts/mech2.R", encoding = "UTF-8")

# WRF CHEM ####
language <- "english" # english spanish
tfs <- readRDS("config/tfs.rds")
net <- readRDS("network/net.rds")
cols <- 51
rows <- 63
dir_wrfinput <- "wrf"
dir_wrfchemi <- "wrf"
domain <- 2
hours <- 0
io_style_emissions <- 2    # 2 for all the hours, 1 for two emission files 0-12z
n_aero <- 15            # number of PM species
rotate <- "cols" # see ?GriddedEmissionsArray
source("scripts/wrf.R", encoding = "UTF-8")
