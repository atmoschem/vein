options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(ggthemes)
library(eixport) # WRF Chem
library(data.table) # blasting speed

# MECH ####
# mechs <- c("CB4", "CB05", "CB05opt2", "S99", "S7","CS7", "S7T", "S11", "S11D",
# "S16C","S18B","RADM2", "RACM2","MOZT1" "CBMZ")
mechs <- c("CB05", "CB05opt2")
for (z in seq_along(mechs)) {
    print(mechs[z])
    language <- "english" # portuguese spanish
    net <- readRDS("network/net.rds")
    tfs <- readRDS("config/tfs.rds")
    mech <- mechs[z]
    source("scripts/mech.R", encoding = "UTF-8")
}
rm(list = ls())
gc()
