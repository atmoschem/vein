options(encoding = "UTF-8")
library(vein)                     # vein
library(sf)                       # spatial data
library(cptcity)                  # 7120 colour palettes
library(ggplot2)                  # plots
library(eixport)                  # create wrfchemi
library(data.table)
sessionInfo()

# 0 Configuration
language             <- "chinese" # english chinese spanish portuguese 
path                 <- "config/inventory.xlsx"
# To read libre office calc, use readODS::read_ods()
readxl::excel_sheets(path)
metadata             <- readxl::read_xlsx(path = path, sheet = "metadata")
mileage              <- readxl::read_xlsx(path = path, sheet = "mileage")
tfs                  <- readxl::read_xlsx(path = path, sheet = "tfs")
veh                  <- readxl::read_xlsx(path = path, sheet = "fleet_age")
fuel                 <- readxl::read_xlsx(path = path, sheet = "fuel")
pmonth               <- readxl::read_xlsx(path = path, sheet = "pmonth")
met                  <- readxl::read_xlsx(path = path, sheet = "met")
year                 <- 2018
scale                <- "tunnel"
theme                <- "black" #dark clean ing  
delete_directories   <- TRUE
eval(parse('config.R', encoding = 'UTF-8'))

# 1) Network ####
net                  <- sf::st_read("network/net.gpkg")
crs                  <- 31983
region               <- "São Paulo"
eval(parse('scripts/net.R', encoding = 'UTF-8'))

# 2) Traffic ####
net                  <- readRDS("network/net.rds")
metadata             <- readRDS("config/metadata.rds")
categories            <- c("pc", "lcv", "trucks", "bus", "mc")  # in network/net.gpkg
veh                  <- readRDS("config/fleet_age.rds")
k_D                  <- 1/1.853396
k_E                  <- 1/1.772143
k_G                  <- 1/2.347402
verbose              <- FALSE
year                 <- 2018
theme               <- "black"     # dark clean ink  
eval(parse('scripts/traffic.R', encoding = 'UTF-8'))

# 3) Estimation #### 
metadata             <- readRDS("config/metadata.rds")
mileage              <- readRDS("config/mileage.rds")
veh                  <- readRDS("config/fleet_age.rds")
net                  <- readRDS("network/net.rds")
pmonth               <- readRDS("config/pmonth.rds")
verbose              <- FALSE
year                 <- 2018
cores                <- c("black", "red", "green3", "blue", "cyan",
                          "magenta", "yellow", "gray", "brown")
# fuel calibration with fuel consumption data
fuel                 <- readRDS("config/fuel.rds")
pol                  <- "FC"
eval(parse('scripts/fuel_eval.R', encoding = 'UTF-8'))

# Exhaust
pol                  <- c("CO", "HC", "NMHC",  "NOx", "CO2","RCHO",
                          "PM", "NO2", "NO")
eval(parse('scripts/exhaust.R', encoding = 'UTF-8'))

# Evaporative
diurnal_ef           <- "D_20_35"
running_losses_ef    <- "R_20_35"
hot_soak_ef          <- "S_20_35"
eval(parse('scripts/evaporatives.R', encoding = 'UTF-8'))

# ressuspensao gera PM e PM10
metadata             <- readRDS("config/metadata.rds")
mileage              <- readRDS("config/mileage.rds")
tfs                  <- readRDS("config/tfs.rds")
net                  <- readRDS("network/net.rds")
veh                  <- readRDS("config/fleet_age.rds")
lkm                  <- net$lkm
tf_PC                <- tfs$PC_G
tf_LCV               <- tfs$LCV_G
tf_TRUCKS            <- tfs$TRUCKS_L_D
tf_BUS               <- tfs$BUS_URBAN_D
tf_MC                <- tfs$MC_150_G
sL1                  <- 0.6        # silt [g/m^2] se ADT < 500 (US-EPA AP42) i
sL2                  <- 0.2        # silt [g/m^2] se 500 < ADT < 5000 (US-EPA AP42)
sL3                  <- 0.06       # silt [g/m^2] se 5000 < ADT < 10000 (US-EPA AP42)
sL4                  <- 0.03       # silt [g/m^2] se ADT > 10000 (US-EPA AP42)
eval(parse('scripts/ressuspensao.R', encoding = 'UTF-8'))

# 4) Post-estimation #### 
net                  <- readRDS("network/net.rds")
pol                  <- c("CO", "HC",  "NOx", "CO2","RCHO", 
                          "PM", "PM10",
                          "NO2", "NO",
                          "D_NMHC","G_NMHC","E_NMHC",
                          "G_EVAP", "E_EVAP",
                          "NMHC")

g                    <- eixport::wrf_grid("wrf/wrfinput_d02")
# Number of lat points 51
# Number of lon points 63
crs                  <- 31983
factor_emi           <- 365        # convertir estimativa diaria a anual
eval(parse('scripts/post.R', encoding = 'UTF-8'))

# plots
metadata             <- readRDS("config/metadata.rds")
tfs                  <- readRDS("config/tfs.rds")
veh                  <- readRDS("config/fleet_age.rds")
pol                  <- c("CO", "HC", "NOx", "CO2","PM", "NMHC")
year                 <- 2018
factor_emi           <- 365        # convertir estimativa diaria a anual
hours                <- 8
bg                   <- "white"
pal                  <- "mpl_viridis"# procura mais paletas com ?cptcity::find_cpt
breaks               <- "quantile"        # "sd" "quantile" "pretty"
tit                  <- "Emissões veiculares em São Paulo [t/ano]"
eval(parse('scripts/plots.R', encoding = 'UTF-8'))

# WRF CHEM
cols                 <- 63                                       # da grade
rows                 <- 51                                       # da grade
data("emis_opt")# names(emis_opt)
emis_option          <- emis_opt$eradm 
pasta_wrfinput       <- "wrf"
pasta_wrfchemi       <- "wrf"
wrfi                 <- "wrf/wrfinput_d02"
domain               <- 2
pol                  <- c("CO", "NO")
peso_molecular       <- c(12 + 16, 14 + 16)
wrf_times            <- 24
lt_emissions         <- "2011-07-25 00:00:00"
eval(parse('scripts/wrf.R', encoding = 'UTF-8'))
