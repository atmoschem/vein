options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# WRF CHEM ####
# mechs <- c("CBMZ", "S99", "RADM2", "CB05")
mechs <- c("CBMZ")

for (z in seq_along(mechs)) {
    print(mechs[z])
    mech <- mechs[z]
    language <- "english" # english spanish
    tfs <- readRDS("config/tfs.rds")
    wrf_times <- 673 # nrow(tfs)
    net <- readRDS("network/net.rds")
    cols <- 51
    rows <- 63
    dir_wrfinput <- "wrf"
    dir_wrfchemi <- "wrf"
    domain <- 2
    hours <- 0
    io_style_emissions <- 2 # 2 for all the hours, 1 for two emission files 0-12z
    n_aero <- 15 # number of PM species
    rotate <- "cols" # see ?GriddedEmissionsArray
    # radm2 and racm2 would not include ETOH. Then, ETOH emissions (mol) is added in HC3
    # if mech is CB05, CBMZ or MOZT1, ETOH is renamed to C2H5OH
    # CB05 works con ETOH, si se usa opt2 en wrf emis_opt$ecb05_opt2[order(emis_opt$ecb05_opt2)]

    source("scripts/wrf.R", encoding = "UTF-8")
}
