options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames

# # MECH spatial ####
# language <- "portuguese" # english spanish
# aer <- c(
#   e_so4i = 0.003672,
#   e_so4j = 0.023328,
#   e_no3i = 0.00345,
#   e_no3j = 0.01155,
#   e_pm25i = 0.04825,
#   e_pm25j = 0.14475,
#   e_orgi = 0.08284,
#   e_orgj = 0.35316,
#   e_eci = 0.30926,
#   e_ecj = 0.01974#,h2o = 0.277
# )
# pol <- c("CO", "NO", "NO2", "SO2", "CO2", "NH3", "CH4", "N2O")
# mol <- c(12, 14 + 16, 14 + 16 * 2, 32 + 16 * 2, 12 + 16 * 2, 14 + 3, 14 * 2 + 16, 14*2 + 16)
# mech <- c("CB05", "S99", "RADM2", "MOZT1", "RACM2") # actually RACM
# suppressWarnings(source("scripts/mech3_spatial.R", encoding = "UTF-8", echo = F))
# rm(list = ls())
# gc()
