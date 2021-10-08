library(data.table)
library(sf)
library(cptcity)
library(ggplot2)
# netemail <- sf::st_read("../vein_baltimore/network/net.gpkg")
# netemail <- st_transform(netemail, 4326)
net <- sf::st_read("network/net_qgis.gpkg")

# V19 = PC + MC
# V20 = PT
# V16 17 18 TRUCKS
# V1 ... 15 ALL (without buses)

# the other volumes can have a total distribution
sapply(st_set_geometry(net[, c("V19T_1", "V20T_1", "F2", "F1")], NULL), sum)

metadata <- readxl::read_excel("config/inventory_MD.xlsx", "metadata")
setDT(metadata)
pop <- readxl::read_excel("config/inventory_MD.xlsx", "pop_moves")
setDT(pop)

net$ALL <- net$V1T_1 + net$V2T_1 + net$V3T_1 + net$V4T_1 + net$V5T_1 +
  net$V6T_1 + net$V7T_1 + net$V8T_1 + net$V9T_1 + net$V10T_1 +
  net$V11T_1 + net$V12T_1 + net$V13T_1 + net$V14T_1 + net$V15T_1 
  

tot_no_bus <- pop[HPMSVtypeID != 40, sum(sourceTypePopulation)]
tot_bus <- pop[HPMSVtypeID == 40, sum(sourceTypePopulation)]
per_MC_ALL <- pop[HPMSVtypeID == 10]$sourceTypePopulation/tot_no_bus
per_PC_ALL <- pop[sourceTypeID == 21]$sourceTypePopulation/tot_no_bus
per_PT_ALL <- pop[sourceTypeID == 31]$sourceTypePopulation/tot_no_bus
per_LCT_ALL <- pop[sourceTypeID == 32]$sourceTypePopulation/tot_no_bus
per_REFUSE_TRUCKS_ALL <- pop[sourceTypeID == 51]$sourceTypePopulation/tot_no_bus
per_SUSH_TRUCKS_ALL <- pop[sourceTypeID == 52]$sourceTypePopulation/tot_no_bus
per_SULH_TRUCKS_ALL <- pop[sourceTypeID == 53]$sourceTypePopulation/tot_no_bus
per_MH_TRUCKS_ALL <- pop[sourceTypeID == 54]$sourceTypePopulation/tot_no_bus
per_CUSH_TRUCKS_ALL <- pop[sourceTypeID == 61]$sourceTypePopulation/tot_no_bus
per_CULH_TRUCKS_ALL <- pop[sourceTypeID == 62]$sourceTypePopulation/tot_no_bus


# MC PC ####
MC_PC <-  pop[sourceTypeID %in% c(11, 21), sum(sourceTypePopulation)]
per_MC <- pop[sourceTypeID %in% c(11), sum(sourceTypePopulation)]/MC_PC
per_PC <- pop[sourceTypeID %in% c(21), sum(sourceTypePopulation)]/MC_PC

net$MC <- net$V19T_1*per_MC + net$ALL*per_MC_ALL
net$PC <- net$V19T_1*per_PC + net$ALL*per_PC_ALL

# PT LCT ####
PT_LCT <-  pop[sourceTypeID %in% c(31, 32), sum(sourceTypePopulation)]
per_PT <- pop[sourceTypeID %in% c(31), sum(sourceTypePopulation)]/PT_LCT
per_LCT <- pop[sourceTypeID %in% c(32), sum(sourceTypePopulation)]/PT_LCT

net$PT <- net$V20T_1*per_PT + net$ALL*per_PT_ALL
net$LCT <- net$V20T_1*per_LCT + net$ALL*per_LCT_ALL

# TRUCKS ####
TRUCKS <- pop[sourceTypeID %in% c(51:54, 61, 62), sum(sourceTypePopulation)]
per_T_REFUSE <- pop[sourceTypeID %in% c(51), sum(sourceTypePopulation)]/TRUCKS
per_T_SU_SH <- pop[sourceTypeID %in% c(52), sum(sourceTypePopulation)]/TRUCKS
per_T_SU_LH <- pop[sourceTypeID %in% c(53), sum(sourceTypePopulation)]/TRUCKS
per_T_MH <- pop[sourceTypeID %in% c(54), sum(sourceTypePopulation)]/TRUCKS
per_T_CU_SH <- pop[sourceTypeID %in% c(61), sum(sourceTypePopulation)]/TRUCKS
per_T_CU_LH <- pop[sourceTypeID %in% c(62), sum(sourceTypePopulation)]/TRUCKS

net$TRUCKS_REFUSE <- (net$V16T_1 + net$V17T_1 + net$V18T_1)*per_T_REFUSE + net$ALL*per_REFUSE_TRUCKS_ALL
net$TRUCKS_SU_SH <- (net$V16T_1 + net$V17T_1 + net$V18T_1)*per_T_SU_SH + net$ALL*per_SUSH_TRUCKS_ALL
net$TRUCKS_SU_LH <- (net$V16T_1 + net$V17T_1 + net$V18T_1)*per_T_SU_LH + net$ALL*per_SULH_TRUCKS_ALL
net$TRUCKS_MH <- (net$V16T_1 + net$V17T_1 + net$V18T_1)*per_T_MH + net$ALL*per_MH_TRUCKS_ALL
net$TRUCKS_CU_SH <- (net$V16T_1 + net$V17T_1 + net$V18T_1)*per_T_CU_SH + net$ALL*per_CUSH_TRUCKS_ALL
net$TRUCKS_CU_LH <- (net$V16T_1 + net$V17T_1 + net$V18T_1)*per_T_CU_LH + net$ALL*per_CULH_TRUCKS_ALL


file.remove("network/net_no_bus.gpkg")
st_write(net, "network/net_no_bus.gpkg")
# plot(net["PC"], axes = T, pal = cpt(colorRampPalette = T, rev = T), key.pos = 1)
# edit inventory.xlsx
rm(list = ls())
gc()

