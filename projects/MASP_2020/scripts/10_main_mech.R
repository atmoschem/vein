options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # WRF Chem
library(data.table) # blasting speed

# MECH ####
# Needs post/grids/ETOH.rds
# mechs <- c("CBMZ", "CB05", "S99", "RADM2")
mechs <- c("CBMZ")
for (z in seq_along(mechs)) {
    print(mechs[z])
    language <- "english" # english spanish
    net <- readRDS("network/net.rds")
    evap <- c("G_EVAP_01", "E_EVAP_01")
    g <- eixport::wrf_grid("wrf/wrfinput_d02")
    # Number of lat points 51
    # Number of lon points 60
    pol <- c("CO", "NO", "NO2", "SO2", "NH3")
    # g/mol: CO, NO, NO2, SO2, NH3. vein's NO is NO2-equivalent mass
    # (NO = NOx * (1 - fNO2) with NOx as NO2), so NO uses 46, not 30
    mol <- c(12 + 16, 14 + 16 * 2, 14 + 16 * 2, 32 + 16 * 2, 14 + 3)
    aer <- "pmneu2" # pmiag, pmneu
    # mech <- "iag" # iag_cb05v2, neu_cb05, iag_racm
    # source("scripts/mech.R", encoding = "UTF-8")
    mech <- mechs[z] # "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1" "CBMZ"^
    source("scripts/mech2.R", encoding = "UTF-8")
}
