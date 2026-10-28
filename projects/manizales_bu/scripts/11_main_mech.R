options(encoding = "UTF-8")
library(vein) # vein
library(sf) # spatial data
library(cptcity) # 7120 colour palettes
library(ggplot2) # plots
library(eixport) # create wrfchemi
library(data.table) # faster data.frames

# MECH ####
# "CB4", "CB05","CB05opt2", "S99", "S7","CS7", "S7T", "S11", "S11D","S16C","S18B","RADM2", "RACM2","MOZT1" "CBMZ"
mechs <- c("CBMZ", "CB05", "S99", "RADM2")
for (z in seq_along(mechs)) {
  print(mechs[z])
  language <- "spanish" # portuguese english spanish
  net <- readRDS("network/net.rds")
  g <- eixport::wrf_grid("wrf/wrfinput_d03")
  pol <- c("CO", "NO", "NO2")
  # g/mol: CO = 28; vein NO is NO2-equivalent mass (NO = NOx*(1-fNO2), with
  # NOx expressed as NO2), so NO also uses 46; NO2 = 46
  mol <- c(12 + 16, 14 + 16 * 2, 14 + 16 * 2)
  aer <- "pmneu2" # pmiag, pmneu
  mech <- mechs[z]
  source("scripts/mech2.R", encoding = "UTF-8")
}
