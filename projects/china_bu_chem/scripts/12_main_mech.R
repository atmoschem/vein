options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# MECH ####
language <- "english" # english spanish
net <- readRDS("network/net.rds")
evap <- c("Evaporative")
g <- make_grid(net, 100)
pol <- c("CO", "NO", "NO2")
# g/mol: CO, NO, NO2. NO is NOx * 0.9 (post.R) and NOx mass is NO2-equivalent,
# so NO mass is NO2-equivalent too and is converted to moles with 46, not 30
mol <- c(12 + 16, 14 + 16 * 2, 14 + 16 * 2)
save_species <- TRUE
type <- "streets" # "grids"
aer <- "pmneu2" # pmiag, pmneu
mech <- "CB05opt2" # "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1"
remove_mech_files <- TRUE
# source("scripts/mech2.R", encoding = "UTF-8", echo = T)
# needs to test
source("scripts/mech2_china.R", encoding = "UTF-8", echo = T)
beepr::beep(2)
