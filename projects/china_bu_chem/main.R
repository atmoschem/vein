options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed
sessionInfo()

# 0 Configuration
language <- "english" # portuguese english spanish
path <- "config/inventory_chn_bu_v5.xlsx"
readxl::excel_sheets(path) # For libre office, readODS::read_ods()
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
metadata <- metadata[metadata$exist == "yes",]
mileage <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs")
veh <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel <- readxl::read_xlsx(path = path, sheet = "fuel")
met <- readxl::read_xlsx(path = path, sheet = "met")
std <- readxl::read_xlsx(path = path, sheet = "std")
h <- readxl::read_xlsx(path = path, sheet = "h")
year <- 2022
theme <- "black" # dark clean ink
delete_directories <- TRUE
source("config/config.R", encoding = "UTF-8")
beepr::beep(2)

# 0.1 prenet ####
language <- "english" # portuguese english spanish
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
source("scripts/prenet.R", encoding = "UTF-8")
beepr::beep(2)

# 1) Network ####
language <- "english" # portuguese english spanish
net <- sf::st_read("network/net.gpkg")
crs <- 32648 # WGS 84 / UTM zone 48N
tit <- "Vehicular volume [veh/h] in Beijing, China"
metadata <- readRDS("config/metadata.rds")
source("scripts/net.R", encoding = "UTF-8")
beepr::beep(2)

net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
source("scripts/speed.R", encoding = "UTF-8")
beepr::beep(2)

# 2) Traffic ####
language <- "english" # portuguese english spanish
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
year <- 2022
theme <- "black" # dark clean ink
  
k_G <- k_CNG <- k_D <- 1 
survival <- TRUE
source("scripts/traffic.R", encoding = "UTF-8")
beepr::beep(2)


# 3) Estimation ####
language <- "english" # english spanish portuguese
tfs <- readRDS("config/tfs.rds")
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
h <- readRDS("config/h.rds")
std <- readRDS("config/std.rds")
speed <- readRDS("network/speed.rds")
verbose <- FALSE
year <- 2022
verbose <- FALSE
remove_fuel <- c("ELEC", "HY")

# fuel calibration with fuel consumption data
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
# 30 is the number of days of the month of interest
factor_emi <- 30 / (nrow(tfs) / 24) # days to month
source("scripts/fuel_eval.R", encoding = "UTF-8")
beepr::beep(2)

# Exhaust
language <- "english" # portuguese english spanish
pol <- c(
    "CO", "HC",  "NOx",# "CO2", "SO2",
    "PM10","PM2.5"
)
pol <- "CO"
source("scripts/exhaust.R", encoding = "UTF-8")
beepr::beep(2)

# Evaporatives
metadata <- readRDS("config/metadata.rds")
pol <- c("Evaporative_driving",
         "Evaporative_parking")
source("scripts/evaporatives.R", encoding = "UTF-8")
beepr::beep(2)

# Tyres, Breaks and Road
language <- "english" #portuguese english spanish
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
tfs <- readRDS("config/tfs.rds")
chnet <- readRDS("network/net.rds")
speed <- readRDS("network/speed.rds")
veh <- readRDS("config/fleet_age.rds")
pol <- c("PM2.5", "PM10")
verbose <- FALSE
source("scripts/wear.R", encoding = "UTF-8")
beepr::beep(2)

# 4) Post-estimation ####
language <- "english" # portugueseenglish spanish
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
pol <- c(
    "CO", "HC", "NOx", 
    "PM2.5", "PM10") #generate NMHC by fuel
pol <- "CO"
g <- make_grid(net, 100)
# Number of lon points: 46
# Number of lat points: 28
crs <- 32648 # WGS 84 / UTM zone 48N
factor_emi <- 365 / (nrow(tfs) / 24) # daily to annual
source("scripts/post.R", encoding = "UTF-8")
beepr::beep(2)


# # plots
language <- "english" #portuguese english spanish
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
year <- 2022
factor_emi <- 365 # convertir estimativa diaria a anual
hours <- 8
bg <- "white"
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks <- "quantile" # "sd" "quantile" "pretty"
tit <- "Vehicular emissions Some Streets Beijing"
source("scripts/plots.R")
beepr::beep(2)

# MECH ####
language <- "english" # english spanish
net <- readRDS("network/net.rds")
evap <- c("Evaporative")
g <- make_grid(net, 100)
pol <- c("CO", "NO", "NO2")
mol <- c(12 + 16, 14 + 16, 14 + 16 * 2)
save_species <- TRUE
type <- "streets" # "grids"
aer <- "pmneu2" # pmiag, pmneu
mech <- "CB05opt2" # "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1"
remove_mech_files <- TRUE
# source("scripts/mech2.R", encoding = "UTF-8", echo = T)
# needs to test
source("scripts/mech2_china.R", encoding = "UTF-8", echo = T)
beepr::beep(2)


# # WRF CHEM
# language <- "portuguese" # english spanish
# net <- readRDS("network/net.rds")
# cols <- 63
# rows <- 51
# wrf_times <- 24 # ?
# data("emis_opt") # names(emis_opt)
# emis_option <- emis_opt$ecb05_opt2
# emis_option[length(emis_option)] <- "E_PM_10"
# pasta_wrfinput <- "wrf"
# pasta_wrfchemi <- "wrf"
# wrfi <- "wrf/wrfinput_d02"
# domain <- 2
# lt_emissions <- "2011-07-25 00:00:00"
# hours <- 0
# source("scripts/wrf.R", encoding = "UTF-8")