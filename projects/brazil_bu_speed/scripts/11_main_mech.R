options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed
library(units)

# MECH ####
mech <- "RADM2" # CB05opt2 "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1"
for (k in seq_along(mech)) {
  language <- "english" # english spanish
  net <- readRDS("network/net.rds")
  g <- eixport::wrf_grid("wrf/wrfinput_d02")
  type <- 'grids' #streets
  pol <- c("CO", "NO", "NO2", "SO2", "NH3")
  # g/mol; vein NO is NO2-equivalent mass (NO = NOx * (1 - fNO2), NOx as NO2), so NO uses 46
  mol <- c(12 + 16, 14 + 16 * 2, 14 + 16 * 2, 32 + 16 * 2, 14 + 3)
  aer <- "pm2023" # pmiag, pmneu
  source("scripts/mech2.R", encoding = "UTF-8")
}
rm(list = ls())
gc()
