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
language <- "english" # portuguese spanish
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

# file.copy(from = "../../baltimore/vein_simu_bu/csv/ef_rateperdistance.csv.gz", "csv")
# file.copy(from = "../../baltimore/vein_simu_bu/csv/ef_ratepervehicle.csv.gz", "csv")
