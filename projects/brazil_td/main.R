options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames
sessionInfo()

# 0 Configuration
language           <- "english" # spanish portuguese
path               <- "config/inventory.xlsx"
readxl::excel_sheets(path)
metadata           <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage            <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs                <- readxl::read_xlsx(path = path, sheet = "tfs")
veh                <- readxl::read_xlsx(path = path, sheet = "fleet")
fuel               <- readxl::read_xlsx(path = path, sheet = "fuel")
pmonth             <- readxl::read_xlsx(path = path, sheet = "pmonth")
met                <- readxl::read_xlsx(path = path, sheet = "met")
year               <- 2018
scale              <- "tunnel"
theme              <- "black" # dark clean ing
delete_directories <- TRUE
source("config/config.R", encoding = "UTF-8")

# 1) Network ####
net <- sf::st_read("network/net.gpkg")
crs <- 31983
source("scripts/net.R", encoding = "UTF-8")

# 2) Traffic ####
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
categories <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh <- readRDS("config/fleet_age.rds")
k_D <- 1/7.250961
k_E <- 1/5.708199
k_G <- 1/5.866790
verbose <- FALSE
year <- 2018
theme <- "black" # dark clean ink
source("scripts/traffic.R", encoding = "UTF-8")

# 3) Estimation ####
language <- "portuguese" # english chinese spanish portuguese
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
pmonth <- readRDS("config/pmonth.rds")
met <- readRDS("config/met.rds")
verbose <- FALSE
year <- 2018
cores <- c(
    "black", "red", "green3", "blue", "cyan",
    "magenta", "yellow", "gray", "brown"
)
# fuel calibration with fuel consumption data
fuel <- readRDS("config/fuel.rds")
pol <- "FC"
source("scripts/fuel_eval.R", encoding = "UTF-8")

# Exhaust
pol <- c(
    "CO", "HC", "NMHC", "NOx", "CO2", "RCHO",
    "PM", "NO2", "NO"
)
source("scripts/exhaust.R", encoding = "UTF-8")

# Evaporative
source("scripts/evaporatives.R", encoding = "UTF-8")

# distribute emissions into OSM roads ####
# To download OpenStreetMap data...
# Read and edit the file osm.R
# Then run manually
#download_osm <- TRUE
#OSM_region <- "sudeste"
#"scripts/osm.R"
# I already included OSM hre network/roads.rds

# 4) Post-estimation ####
net <- readRDS("network/net.rds")
roads  <- readRDS("network/roads.rds") # I already included OSM
tfs <- readRDS("config/tfs.rds")
pol <- c(
    "CO", "HC", "NOx", "CO2", "SO2",
    "PM", "PM10",
    "NO2", "NO",
    "D_NMHC", "G_NMHC", "E_NMHC",
    "G_EVAP_10", "E_EVAP_10"
)
g <- eixport::wrf_grid("wrf/wrfinput_d02")
# Number of lat points 51
# Number of lon points 63
crs <- 31983
factor_emi <- 365 / (nrow(tfs) / 24) # daily to annual
source("scripts/post.R", encoding = "UTF-8")

# plots
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
pol <- c("CO", "HC", "NOx", "CO2", "PM", "NMHC")
year <- 2018
factor_emi <- 365 # convertir estimativa diaria a anual
hours <- 8
bg <- "white"
pal <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks <- "quantile" # "sd" "quantile" "pretty"
tit <- "Emissões veiculares em São Paulo [t/ano]"
eval(parse("scripts/plots.R", encoding = "UTF-8"))

# WRF CHEM
cols <- 63 # da grade
rows <- 51 # da grade
data("emis_opt") # names(emis_opt)
emis_option <- emis_opt$eradm
pasta_wrfinput <- "wrf"
pasta_wrfchemi <- "wrf"
wrfi <- "wrf/wrfinput_d02"
domain <- 2
pol <- c("CO", "NO")
peso_molecular <- c(12 + 16, 14 + 16)
wrf_times <- 24
lt_emissions <- "2011-07-25 00:00:00"
eval(parse("scripts/wrf.R", encoding = "UTF-8"))