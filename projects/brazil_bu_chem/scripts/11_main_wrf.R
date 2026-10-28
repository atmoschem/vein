options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed
library(units)

# WRF CHEM
# type only grids
mech <- "RADM2"
for (k in seq_along(mech)) {
  language <- "portuguese" # english spanish
  net <- readRDS("network/net.rds")
  cols <- 63
  rows <- 51
  n_aero <- 15
  wrf_times <- 24 #
  pasta_wrfinput <- "wrf"
  pasta_wrfchemi <- "wrf"
  wrfi <- "wrf/wrfinput_d02"
  domain <- 2
  hours <- 0
  source("scripts/wrf.R", encoding = "UTF-8")
}
