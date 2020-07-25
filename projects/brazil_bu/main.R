options(encoding = "UTF-8")
library(vein)                     # vein
library(sf)                       # spatial data
library(cptcity)                  # 7120 colour palettes
library(ggplot2)                  # plots
library(eixport)                  # WRF Chem
library(data.table)               # blasting speed
sessionInfo()

# 0 Configuration
language             <- "portuguese" # english chinese spanish 
path                 <- "config/inventory.xlsx"
readxl::excel_sheets(path )       # For libre office, readODS::read_ods()
metadata             <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage              <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs                  <- readxl::read_xlsx(path = path, sheet = "tfs")
veh                  <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel                 <- readxl::read_xlsx(path = path, sheet = "fuel")
met                  <- readxl::read_xlsx(path = path, sheet = "met")
year                 <- 2018
theme                <- "black"   # dark clean ink  
scale                <- "default"
source("config.R")

# 1) Network ####
net                  <- sf::st_read("network/net.gpkg")
crs                  <- 31983
tit                  <- "Fluxo veicular [veh/h] em São Paulo"
categories             <- c("pc", "lcv", "trucks", "bus", "mc") # in network/net.gpkg
source("scripts/net.R")

# 2) Traffic ####
net                 <- readRDS("network/net.rds")
metadata            <- readRDS("config/metadata.rds")
categories            <- c("pc", "lcv", "trucks", "bus", "mc")  # in network/net.gpkg
veh                 <- readRDS("config/fleet_age.rds")
k_D                 <- 1/0.5661912 
k_E                 <- 1/0.1764558 
k_G                 <- 1/0.2528435
verbose             <- FALSE
year                <- 2018
theme               <- "black"     # dark clean ink  
source('scripts/traffic.R')

# 3) Estimation #### 
metadata            <- readRDS("config/metadata.rds")
mileage             <- readRDS("config/mileage.rds")
tfs                 <- readRDS("config/tfs.rds")
veh                 <- readRDS("config/fleet_age.rds")
met                 <- readRDS("config/met.rds")
net                 <- readRDS("network/net.rds")
lkm                 <- net$lkm
scale               <- "default"
verbose             <- FALSE
year                <- 2018

# Fuel eval
fuel                <- readRDS("config/fuel.rds")
pol                 <- "FC"
factor_emi          <- 365/(nrow(tfs)/24)    # daily to annual
source('scripts/fuel_eval.R')

# Exhaust
pol                 <- c("CO", "HC", "NMHC",  "NOx", "CO2","RCHO",
                         "PM", "NO2", "NO")
source('scripts/exhaust.R')

# Evaporatives
source('scripts/evaporatives.R')

# ressuspensao gera PM e PM10
metadata            <- readRDS("config/metadata.rds")
mileage             <- readRDS("config/mileage.rds")
tfs                 <- readRDS("config/tfs.rds")
net                 <- readRDS("network/net.rds")
veh                 <- readRDS("config/fleet_age.rds")
lkm                 <- net$lkm
tf_PC               <- tfs$PC_G
tf_LCV              <- tfs$LCV_G
tf_TRUCKS           <- tfs$TRUCKS_L_D
tf_BUS              <- tfs$BUS_URBAN_D
tf_MC               <- tfs$MC_150_G
sL1                 <- 0.6        # silt [g/m^2] se ADT < 500 (US-EPA AP42) i
sL2                 <- 0.2        # silt [g/m^2] se 500 < ADT < 5000 (US-EPA AP42)
sL3                 <- 0.06       # silt [g/m^2] se 5000 < ADT < 10000 (US-EPA AP42)
sL4                 <- 0.03       # silt [g/m^2] se ADT > 10000 (US-EPA AP42)
source('scripts/pavedroads.R')

# 4) Post-estimation #### 
net                 <- readRDS("network/net.rds")
tfs                 <- readRDS("config/tfs.rds")
pol                 <- c("CO", "HC",  "NOx", "CO2","RCHO", 
                         "PM", "PM10",
                         "NO2", "NO",
                         "D_NMHC","G_NMHC","E_NMHC",
                         "G_EVAP_10", "E_EVAP_10")
g                   <- eixport::wrf_grid("wrf/wrfinput_d02")
# Number of lat points 51
# Number of lon points 63
crs                 <- 31983
factor_emi          <- 365/(nrow(tfs)/24)    # daily to annual
source('scripts/post.R')

# plots
metadata            <- readRDS("config/metadata.rds")
tfs                 <- readRDS("config/tfs.rds")
veh                 <- readRDS("config/fleet_age.rds")
pol                 <- c("CO", "HC", "NOx", "CO2","PM", "NMHC")
year                <- 2018
factor_emi          <- 365           # convertir estimativa diaria a anual
hours               <- 8
bg                  <- "white"
pal                 <- "mpl_viridis" # procura mais paletas com ?cptcity::find_cpt
breaks              <- "quantile"        # "sd" "quantile" "pretty"
tit                 <- "Emissões veiculares em São Paulo [t/ano]"
source('scripts/plots.R')

# WRF CHEM
cols                <- 63
rows                <- 51
data("emis_opt")                     # names(emis_opt)
emis_option         <- emis_opt$eradm 
pasta_wrfinput      <- "wrf"
pasta_wrfchemi      <- "wrf"
wrfi                <- "wrf/wrfinput_d02"
domain              <- 2
pol                 <- c("CO", "NO")
peso_molecular      <- c(12 + 16, 14 + 16)
wrf_times           <- 24
lt_emissions        <- "2011-07-25 00:00:00"
source("scripts/wrf.R")
