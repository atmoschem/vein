options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# for this part need a lot of memory
# # WRF CHEM ####
# mechs <- c("CBMZ", "S99", "RADM2", "CB05")
# mechs <- "CBMZ"
# for (z in seq_along(mechs)) {
#   print(mechs[z])
#   mech <- mechs[z]
#   language <- "english" # english spanish
#   tfs <- readRDS("config/tfs.rds")
#   net <- readRDS("network/net.rds")
#   cols <- 100
#   rows <- 110
#   wrf_times <- 168 # ?
#   dir_wrfinput <- "wrf"
#   dir_wrfchemi <- "wrf"
#   domain <- 3
#   hours <- 0
#   io_style_emissions <- 1 # 2 for all the hours, 1 for two emission files 0-12z
#   n_aero <- 15 # number of PM species
#   rotate <- "cols" # see ?GriddedEmissionsArray
#   source("scripts/wrf.R", encoding = "UTF-8")
# }
