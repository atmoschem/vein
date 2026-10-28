options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# # WRF CHEM
# language <- "portuguese" # english spanish
# net <- readRDS("network/net.rds")
# cols <- 63
# rows <- 51
# wrf_times <- 24 # ?
# data("emis_opt") # names(emis_opt)
# emis_option <- emis_opt$ecb05_opt2
# emis_option[length(emis_option)] <- "E_PM_10"
# pasta_wrfinput <- "wrf"
# pasta_wrfchemi <- "wrf"
# wrfi <- "wrf/wrfinput_d02"
# domain <- 2
# lt_emissions <- "2011-07-25 00:00:00"
# hours <- 0
# source("scripts/wrf.R", encoding = "UTF-8")
