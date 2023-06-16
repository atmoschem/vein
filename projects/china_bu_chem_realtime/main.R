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
rm(list = ls())
gc()

# 1 Network ####
language <- "english" # portuguese english spanish
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
plots  <- TRUE
source("scripts/prenet_osm.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()

# 2) Traffic ####
language <- "english" # portuguese english spanish
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
veh <- readRDS("config/fleet_age.rds")
verbose <- FALSE
theme <- "black" # dark clean ink
k_G <- k_CNG <- k_D <- 1 
survival <- TRUE
plots <- TRUE
source("scripts/traffic.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()


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
speed <- as.data.frame(net[["speed"]])
verbose <- FALSE
year <- 2022
verbose <- FALSE
remove_fuel <- c("ELEC", "HY")

# Exhaust
language <- "english" # portuguese english spanish
source("scripts/fuel_eval.R", encoding = "UTF-8")
rm(list = ls())
gc()

language <- "english" # english spanish portuguese
tfs <- readRDS("config/tfs.rds")
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
h <- readRDS("config/h.rds")
std <- readRDS("config/std.rds")
speed <- as.data.frame(net[["speed"]])
verbose <- FALSE
year <- 2022
verbose <- FALSE
remove_fuel <- c("ELEC", "HY")
pol <- c(
  "CO", "HC",  "NOx",# "CO2", "SO2",
  "PM10","PM2.5"
)
source("scripts/exhaust.R", encoding = "UTF-8")
rm(list = ls())
gc()


language <- "english" # english spanish portuguese
tfs <- readRDS("config/tfs.rds")
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
veh <- readRDS("config/fleet_age.rds")
net <- readRDS("network/net.rds")
met <- readRDS("config/met.rds")
h <- readRDS("config/h.rds")
std <- readRDS("config/std.rds")
speed <- as.data.frame(net[["speed"]])
verbose <- FALSE
year <- 2022
verbose <- FALSE
remove_fuel <- c("ELEC", "HY")
pol <- c("Evaporative_driving",
         "Evaporative_parking")
source("scripts/evaporatives.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()

# Tyres, Breaks and Road
language <- "english" #portuguese english spanish
net <- readRDS("network/net.rds")
metadata <- readRDS("config/metadata.rds")
mileage <- readRDS("config/mileage.rds")
tfs <- readRDS("config/tfs.rds")
speed <- as.data.frame(net[["speed"]])
veh <- readRDS("config/fleet_age.rds")
pol <- c("PM2.5", "PM10")
verbose <- FALSE
source("scripts/wear.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()

# 4) Post-estimation ####
language <- "english" # portugueseenglish spanish
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
pol <- c(
    "CO", "HC", "NOx", 
    "PM2.5", "PM10") #generate NMHC by fuel
pol <- "CO"
crs <- 32648 # WGS 84 / UTM zone 48N
source("scripts/post.R", encoding = "UTF-8")
beepr::beep(2)
rm(list = ls())
gc()
