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
net                <- sf::st_read("network/net.gpkg")
crs                <- 31983
source("scripts/net.R", encoding = "UTF-8")

# 2) Traffic ####
net                <- readRDS("network/net.rds")
metadata           <- readRDS("config/metadata.rds")
categories         <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
veh                <- readRDS("config/fleet_age.rds")
k_D                <- 1/2.482039
k_E                <- 1/5.708199
k_G                <- 1/5.866790
verbose            <- FALSE
year               <- 2018
theme              <- "black" # dark clean ink
source("scripts/traffic.R", encoding = "UTF-8")

# 3) Estimation ####
language           <- "portuguese" # english chinese spanish portuguese
metadata           <- readRDS("config/metadata.rds")
mileage            <- readRDS("config/mileage.rds")
veh                <- readRDS("config/fleet_age.rds")
net                <- readRDS("network/net.rds")
pmonth             <- readRDS("config/pmonth.rds")
met                <- readRDS("config/met.rds")
verbose            <- FALSE
year               <- 2018

# fuel calibration with fuel consumption data
fuel              <- readRDS("config/fuel.rds")
pol               <- "FC"
source("scripts/fuel_eval.R", encoding = "UTF-8")

# Exhaust
pol               <- c("CO", "HC", "NMHC", "NOx", "CO2",
                       "PM", "NO2", "NO", "SO2")
scale             <- "tunnel"
source("scripts/exhaust.R", encoding = "UTF-8")

# Evaporative
source("scripts/evaporatives.R", encoding = "UTF-8")

# Paved Roads
metadata           <- readRDS("config/metadata.rds")
mileage            <- readRDS("config/mileage.rds")
year               <- 2019
pol               <- c("PM25RES", "PM10RES")
source("scripts/ressuspensao.R", encoding = "UTF-8")
# distribute emissions into OSM roads ####
# To download OpenStreetMap data...
# Read and edit the file osm.R
# Then run manually
#download_osm <- TRUE
#OSM_region <- "sudeste"
#"scripts/osm.R"
# I already included OSM hre network/roads.rds

# 4) Post-estimation ####
language          <- "spanish" # english chinese spanish portuguese
net               <- readRDS("network/net.rds")
roads             <- st_transform(readRDS("network/roads.rds"), 31983) # I already included OSM
months_subset     <- 8           #10:11 for instance
g                 <- st_transform(eixport::wrf_grid(
  paste0(system.file("extdata",
                     package = "eixport"),
         "/wrfinput_d01")), 31983)
# Number of lat points 99
# Number of lon points 149
crs               <- 31983
osm_name          <- "fclass"          #OSM column for type of road (motorway, trunk...)
source("scripts/post.R", encoding = "UTF-8")


# plots
metadata          <- readRDS("config/metadata.rds")
tfs               <- readRDS("config/tfs.rds")
veh               <- readRDS("config/fleet_age.rds")
pol               <- c("CO", "HC", "NOx", "CO2", "PM", "NMHC")
year              <- 2018
bg                <- "white"
pal               <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks            <- "quantile" # "sd" "quantile" "pretty"
tit               <- "Emissões veiculares em São Paulo [t/ano]"
source("scripts/plots.R", encoding = "UTF-8")

# MECH ####
language          <- "portuguese" # english spanish
months_subset     <- "08"   #only one month each time
g                 <- eixport::wrf_grid(paste0(system.file("extdata",
                                                          package = "eixport"),
                                              "/wrfinput_d01"))
aer               <- "pmneu2"          # "pmiag", "pmneu"
#  option 1 (if cb05-=> ecb05_opt1)
mech              <- "iag_cb05"        # "iag_cb05v2", "neu_cb05", "iag_racm"
pol               <- c("CO", "NO",   "NO2",   "SO2", "CO2")
mol               <- c(12,  14+16, 14+16*2, 32+16*2, 12+16*2)
source('scripts/mech.R', encoding = 'UTF-8')
#  option 2 (if cb05-=> ecb05_opt2)
# mech              <- "CB05"        # "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1"
# source('scripts/mech2.R', encoding = 'UTF-8')

# # remove some pollutant?
# file.remove("post/spec_grid/E_BENZENE.rds")


# WRF CHEM
language          <- "portuguese" # english spanish
# hourly distribution for NOx with HDV
# hourly distribution for other than NOX with PC
tfs               <- readRDS("config/tfs.rds")
remove_pol        <- "E"
year              <- 2018
days              <- 30   #days of month
cols              <- 99 # da grade
rows              <- 149 # da grade
wrf_times         <- 24 # ?
data("emis_opt")# names(emis_opt)
emis_option       <- gsub(".rds", "", list.files(path = "post/spec_grid"))
pasta_wrfinput    <- system.file("extdata",
                                 package = "eixport")
pasta_wrfchemi    <- "wrf"
wrfi              <- paste0(system.file("extdata",
                                        package = "eixport"),
                            "/wrfinput_d01")
domain            <- 1
wrf_times         <- 24
offset_hours      <- 0
source("scripts/wrf.R", encoding = "UTF-8")
