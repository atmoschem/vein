options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(ggthemes)
library(eixport) # WRF Chem
library(data.table) # blasting speed
sessionInfo()


# 0 Configuration
language <- "english" # portuguese spanishx'x'
path <- "config/inventory_MD.xlsx"
readxl::excel_sheets(path)
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs <- readxl::read_xlsx(path = path, sheet = "tfs_moves")
veh <- readxl::read_xlsx(path = path, sheet = "veh_age")
vmt_age <- readxl::read_xlsx(path = path, sheet = "vmt_age")
fuel <- readxl::read_xlsx(path = path, sheet = "fuel")
fuel_type <- readxl::read_xlsx(path = path, sheet = "fuel_type")
met <- readxl::read_xlsx(path = path, sheet = "met")
year <- 2019
month <- 8
theme <- "black" # dark clean ink
delete_directories <- T
source("config/config.R", encoding = "UTF-8")

# 1) Network ####
# source("network/pre_net_simu_BMC_qgisv2.R")
# source("network/pre_net_GTFS.R")
language <- "english" # english spanish portuguese
path <- "config/inventory_MD.xlsx"
metadata <- readxl::read_xlsx(path = path, sheet = "metadata")
net <- sf::st_read("network/net_baltimore_city.gpkg")
crs <- 6487
tit <- "Travel demand model outptus from Baltimore 2019 (veh/h)"
names(net)
categories <- unique(metadata$vein_name) # in network/net.gpkg
source("scripts/net.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 2) Traffic ####
language <- "english" # portuguese spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
categories <- unique(metadata$vein_name) # in net
verbose <- FALSE
year <- 2019
theme <- "ink" # dark clean ink
speed <- TRUE
ps <- "CONGSPD"
ffs <- "FFSPEED"
capacity <- "CAPE"
dist <- "miles"
k_D <- k_CNG <- k_G <- k_E85 <- 1
survival <- FALSE
source("scripts/traffic.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 3) Estimation ####
language <- "english" # portuguese english spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
speed <- readRDS("network/speed_net.rds")
speed_bin <- readRDS("network/speed_bin.rds")
fuel <- readRDS("config/fuel.rds")
fuel_type <- readRDS("config/fuel_type.rds")
lkm <- net$lkm
units(net$lkm)$numerator # miles
verbose <- TRUE
year <- 2019

# SQL MOVES PARAMETERS
host <- "localhost"
moves_db <- "baltimore_2017_aug_emissionfactor"
user <- "moves"
password <- "moves"
port <- 3307

# 3.1 Fuel eval ####
source("scripts/fuel_eval.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 3.2 Emissions per distance ####
language <- "english" # portuguese spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
speed <- readRDS("network/speed_net.rds")
speed_bin <- readRDS("network/speed_bin.rds")
fuel_type <- readRDS("config/fuel_type.rds")
lkm <- net$lkm
verbose <- FALSE

# SQL MOVES PARAMETERS
host <- "localhost"
moves_db <- "baltimore_2017_aug_emissionfactor" # _mi_aft _mo
user <- "moves"
password <- "moves"
port <- 3307
source("scripts/emissions_per_distance.R", encoding = "UTF-8")
rm(list = ls())
gc()


language <- "english" # english spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
speed <- readRDS("network/speed_net.rds")
speed_bin <- readRDS("network/speed_bin.rds")
fuel_type <- readRDS("config/fuel_type.rds")
lkm <- net$lkm
verbose <- FALSE

# SQL MOVES PARAMETERS
host <- "localhost"
moves_db <- "baltimore_2017_aug_emissionfactor" # _mi_aft _mo
user <- "moves"
password <- "moves"
port <- 3307

source("scripts/emissions_per_distance_NMHC.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 3.2 Emissions per vehicle ####
language <- "english" # english spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
speed <- readRDS("network/speed_net.rds")
speed_bin <- readRDS("network/speed_bin.rds")
fuel_type <- readRDS("config/fuel_type.rds")
lkm <- net$lkm
verbose <- TRUE

# SQL MOVES PARAMETERS
host <- "localhost"
moves_db <- "baltimore_2017_aug_emissionfactor" # _mi_aft _mo
user <- "moves"
password <- "moves"
port <- 3307

source("scripts/emissions_per_vehicle.R", encoding = "UTF-8")
rm(list = ls())
gc()


language <- "english" # english spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
speed <- readRDS("network/speed_net.rds")
speed_bin <- readRDS("network/speed_bin.rds")
fuel_type <- readRDS("config/fuel_type.rds")
lkm <- net$lkm
verbose <- TRUE

# SQL MOVES PARAMETERS
host <- "localhost"
moves_db <- "baltimore_2017_aug_emissionfactor" # _mi_aft _mo
user <- "moves"
password <- "moves"
port <- 3307

source("scripts/emissions_per_vehicle_NMHC.R", encoding = "UTF-8")
rm(list = ls())
gc()

# 4) Post-estimation ####
data(decoder)
metadata <- readRDS("config/metadata.rds")
language <- "english" # portuguese spanish
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
grid_width <- 200 # mts
crs <- 6487
source("scripts/post.R", encoding = "UTF-8")
rm(list = ls())
gc()

# plots ####
language <- "english" # portuguese spanish
metadata <- readRDS("config/metadata.rds")
tfs <- readRDS("config/tfs.rds")
hours <- 8
bg <- "white"
pal <- "colo_lightningmccarl_into_the_night" # find more in ?cptcity::find_cpt
tit <- "Baltimore City"
source("scripts/plots.R")
rm(list = ls())
gc()

# MECH ####
# mechs <- c("CB4", "CB05", "CB05opt2", "S99", "S7","CS7", "S7T", "S11", "S11D",
# "S16C","S18B","RADM2", "RACM2","MOZT1" "CBMZ")
mechs <- c("CB05", "CB05opt2")
for (z in seq_along(mechs)) {
    print(mechs[z])
    language <- "english" # portuguese spanish
    net <- readRDS("network/net.rds")
    tfs <- readRDS("config/tfs.rds")
    mech <- mechs[z]
    source("scripts/mech.R", encoding = "UTF-8")
}
rm(list = ls())
gc()