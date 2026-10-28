options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(data.table) # faster data.frames
library(units)
library(eixport)

# 8 MECH spatial ####
library(units)
language <- "portuguese" # english spanish
mech <- "RADM2"
aer <- "pm2023" # pmiag, pmneu
pol <- c("CO", "NO", "NO2", "SO2", "NH3")
# g/mol; vein NO is NO2-equivalent mass (NO = NOx * (1 - fNO2), NOx as NO2), so NO uses 46
mw <- c(12 + 16, 14 + 16 * 2, 14 + 16 * 2, 32 + 16 * 2, 14 + 3)
suppressWarnings(source(
  "scripts/mech_spatial.R",
  encoding = "UTF-8",
  echo = F
))
rm(list = ls())
gc()
