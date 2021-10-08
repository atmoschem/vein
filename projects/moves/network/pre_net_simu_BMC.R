library(data.table)
library(sf)
library(cptcity)
library(ggplot2)
# netemail <- sf::st_read("../vein_baltimore/network/net.gpkg")
# netemail <- st_transform(netemail, 4326)
net <- sf::st_read("../BMC_SIMU/HWY/HWY_LDNET_AM2_4.shp")
net <- st_crop(net, md)
names(net)
plot(net$geometry)
# writexl::write_xlsx(net, "config/net.xlsx")
# net <- st_transform(net, 6487)

# plot(netemail["MC"], axes = T)
# dim(net)
# find_cpt("ocal")
nr <- classInt::classIntervals(var = net$F1[net$F1 > 0], n = 100, style = "sd")


# attached zip file contains three shape files - am loaded highway 
# network 1) 6:30-7:30, 2) 7:30-8:30, and 3) 8:30 - 9:30.
# The horizon year is 2022, which is the closest we have to 2019.
# 
# Link variables (VOL1, VOL2...VOL20) are defined below.  
# Trip tables are divided into 5 value of time (1 lowest...5 highest).
# 
# Will follow up with additional documentation on link variables.  

# PHASE=ILOOP
# ; SOV (+ SOV Air & Drive to Transit in VOT3)
# PATH = LW.COST_SOV1, PENI = 1, VOL[1]=MI.1.1,EXCLUDEGRP=1,4,7
# PATH = LW.COST_SOV2, PENI = 1, VOL[2]=MI.1.2,EXCLUDEGRP=1,4,7
# PATH = LW.COST_SOV3, PENI = 1, VOL[3]=MI.1.3+MI.2.6+MI.3.@PER@,EXCLUDEGRP=1,4,7
# PATH = LW.COST_SOV4, PENI = 1, VOL[4]=MI.1.4,EXCLUDEGRP=1,4,7
# PATH = LW.COST_SOV5, PENI = 1, VOL[5]=MI.1.5,EXCLUDEGRP=1,4,7
# 
# ; HOV2 (+ HOV Air in VOT3)
# PATH = LW.COST_HOV21, PENI = 1, VOL[6]=MI.1.6,EXCLUDEGRP=4,7
# PATH = LW.COST_HOV22, PENI = 1, VOL[7]=MI.1.7,EXCLUDEGRP=4,7
# PATH = LW.COST_HOV23, PENI = 1, VOL[8]=MI.1.8+MI.2.7,EXCLUDEGRP=4,7
# PATH = LW.COST_HOV24, PENI = 1, VOL[9]=MI.1.9,EXCLUDEGRP=4,7
# PATH = LW.COST_HOV25, PENI = 1, VOL[10]=MI.1.10,EXCLUDEGRP=4,7
# 
# ; HOV3
# PATH = LW.COST_HOV31, PENI = 1, VOL[11]=MI.1.11,EXCLUDEGRP=4
# PATH = LW.COST_HOV32, PENI = 1, VOL[12]=MI.1.12,EXCLUDEGRP=4
# PATH = LW.COST_HOV33, PENI = 1, VOL[13]=MI.1.13,EXCLUDEGRP=4
# PATH = LW.COST_HOV34, PENI = 1, VOL[14]=MI.1.14,EXCLUDEGRP=4
# PATH = LW.COST_HOV35, PENI = 1, VOL[15]=MI.1.15,EXCLUDEGRP=4
# 
# ; TRUCKS
# PATH = LW.COST_CV, PENI = 1, VOL[16]=MI.2.1,EXCLUDEGRP=1,4,7
# PATH = LW.COST_MT, PENI = 1, VOL[17]=MI.2.2,EXCLUDEGRP=1,3,4,7
# PATH = LW.COST_HT, PENI = 1, VOL[18]=MI.2.3,EXCLUDEGRP=1,3,4,7
# 
# ; XXPC
# PATH = LW.COST_SOVXX, PENI = 1, VOL[19]=MI.2.4 EXCLUDEGRP=1,2,4,7
# PATH = LW.COST_HOVXX, PENI = 1, VOL[20]=MI.2.5 EXCLUDEGRP=2,4
# ENDPROCESS

# F1-F13 is 24 hour counts...but i am not sure on year - Brian?
#   
# V_1 is the sum of volumes 1-20
# V1_1 through V1_20 matches the list from the previous emails.  VOL 1 SOV VOT1
# 
# V1T_1 is the total volume AB V1_1 + BA V1_1
# 
# Capacity is CAPCLASS * 50 or you can use the value in CAPE.
# 

# net$PC_TOTAL <- net$V19T_1 + net$V20T_1
# names(net)[120] <- "XXPC = V19T_1 + V20T_1"
# net$PC_AB <- net$V19_1 + net$V20_1
# names(net)[121] <- "XXPC = V19_1 + V20_1"
# 
# plot(net[, c("XXPC = V19T_1 + V20T_1",
#                 "XXPC = V19_1 + V20_1")], 
#      key.pos = 4, 
#      pal = cpt(rev = T, colorRampPalette = T),
#      axes = T,
#      breaks = "sd")
# HOV: High occupancy vehicle https://en.wikipedia.org/wiki/High-occupancy_vehicle_lane
# SOV: Single occupancy vehicle http://www.nwarpc.org/pdf/Transportation/Sustainable_Mobility/Maximizing_Exchange_Minimizing_Travel.pdf

# F1 -> F13
# F1	FHWA Vehicle Class 1	motorcycle
# F2	FHWA Vehicle Class 2	passenger car
# F3	FHWA Vehicle Class 3	2 axle, 4 tire single units
# F4 	FHWA Vehicle Class 4	buses
# F5	FHWA Vehicle Class 5	2 axle, 6 tire single units
# F6	FHWA Vehicle Class 6	3 axle single units
# F7 	FHWA Vehicle Class 7	4 or more axle single units
# F8	FHWA Vehicle Class 8	4 or less axle single trailers
# F9	FHWA Vehicle Class 9	5 axle single trailers
# F10 	FHWA Vehicle Class 10	6 plus axle single trailers
# F11	FHWA Vehicle Class 11	5 or less axle multi-trailers
# F12	FHWA Vehicle Class 12	6 axle multi-trailers
# F13	FHWA Vehicle Class 13	7 or more axle multi-trailers

# goal ####
# The goal is to have volumes by MOVES categories
# Then, need toy data to proceed
# We can only be sure for PC = V19 + v20 and trucks v16 + v17 + v18
# HOV3 seems to be mostly buses, we can assume that for now v11 + v12 + v13 + v14 + v15

# the other volumes can have a total distribution
sapply(st_set_geometry(net[, c("V19T_1", "V20T_1", "F2", "F1")], NULL), sum)

metadata <- readxl::read_excel("../excel_inventory/inventory_MD.xlsx", "metadata")
setDT(metadata)
pop <- readxl::read_excel("../excel_inventory/inventory_MD.xlsx", "pop_moves")
setDT(pop)

# Motorcycle = F1
# Passenger Car = F2
# Passenger Truck = F3
# Light Commercial Truck = F3
# Intercity Bus = F4
# Transit Bus = F4
# School Bus = F4
# Refuse Truck = F5 + F6 + F8 
# Single Unit Short-haul Truck = F5 + F6 + F8 
# Single Unit Long-haul Truck = F7 + F9 + F10 
# Motor Home =  F5 + F6 + F8
# Combination Short-haul Truck = F11
# Combination Long-haul Truck = F13 + F12

# assuming vols 1 to 10 covers all vehicles
# assumes trucks only on SOV, which makes sense
net$ALL <- net$V1T_1 + net$V2T_1 + net$V3T_1 + net$V4T_1 + net$V5T_1 
efes <- st_set_geometry(net[, paste0("F", 1:13)], NULL)
setDT(efes)
efes <- efes[rowSums(efes) > 0]

col_efes <- colSums(efes)/sum(colSums(efes))
df <- as.data.table(matrix(col_efes, nrow = 1))
names(df) <- paste0("F", 1:13)

df

unique(pop$sourceTypeID)
MC_PC <-  pop[sourceTypeID %in% c(11, 21), sum(sourceTypePopulation)]
per_MC <- pop[sourceTypeID %in% c(11), sum(sourceTypePopulation)]/MC_PC
per_PC <- pop[sourceTypeID %in% c(21), sum(sourceTypePopulation)]/MC_PC
# assumes V19T + V6T ...V10T  = MC + PC
# assumes net$ALL*dfF1 = MC
# assumes net$ALL*dfF2 = PC

net$MC <- (net$V19T_1 + net$V6T_1 +net$V7T_1 +net$V8T_1 +net$V9T_1 +net$V10T_1)*per_MC + net$ALL*df$F1
net$PC <- (net$V19T_1 + net$V6T_1 +net$V7T_1 +net$V8T_1 +net$V9T_1 +net$V10T_1)*per_PC + net$ALL*df$F2


PT_LCT <-  pop[sourceTypeID %in% c(31, 32), sum(sourceTypePopulation)]
per_PT <- pop[sourceTypeID %in% c(31), sum(sourceTypePopulation)]/PT_LCT
per_LCT <- pop[sourceTypeID %in% c(31), sum(sourceTypePopulation)]/PT_LCT

# assuming V20T are PT
net$PT <- (net$V20T_1 + net$ALL*df$F3)*per_PT
net$LCT <- (net$V20T_1 + net$ALL*df$F3)*per_LCT

BUS <- pop[sourceTypeID %in% 41:43, sum(sourceTypePopulation)]
per_IBUS <- pop[sourceTypeID %in% c(41), sum(sourceTypePopulation)]/BUS
per_TBUS <- pop[sourceTypeID %in% c(42), sum(sourceTypePopulation)]/BUS
per_SBUS <- pop[sourceTypeID %in% c(43), sum(sourceTypePopulation)]/BUS
net$BUS_INTERCITY <-   (net$V11T_1 + net$V12T_1 + net$V13T_1 + net$V14T_1 + net$V15T_1)*per_IBUS + net$ALL*df$F4*per_IBUS
per_IBUS*(net$V11T_1 + net$V12T_1 + net$V13T_1 + net$V14T_1 + net$V15T_1 + net$ALL*df$F4)

net$BUS_TRANSIT <- (net$V11T_1 + net$V12T_1 + net$V13T_1 + net$V14T_1 + net$V15T_1)*per_TBUS+ net$ALL*df$F4*per_TBUS

net$BUS_SCHOOL <- (net$V11T_1 + net$V12T_1 + net$V13T_1 + net$V14T_1 + net$V15T_1)*per_SBUS+ net$ALL*df$F4*per_SBUS


TRUCKS <- pop[sourceTypeID %in% c(51:54, 61, 62), sum(sourceTypePopulation)]
per_T_REFUSE <- pop[sourceTypeID %in% c(51), sum(sourceTypePopulation)]/TRUCKS
per_T_SU_SH <- pop[sourceTypeID %in% c(52), sum(sourceTypePopulation)]/TRUCKS
per_T_SU_LH <- pop[sourceTypeID %in% c(53), sum(sourceTypePopulation)]/TRUCKS
per_T_MH <- pop[sourceTypeID %in% c(54), sum(sourceTypePopulation)]/TRUCKS
per_T_CU_SH <- pop[sourceTypeID %in% c(61), sum(sourceTypePopulation)]/TRUCKS
per_T_CU_LH <- pop[sourceTypeID %in% c(62), sum(sourceTypePopulation)]/TRUCKS
# assumes trucks + sum V15..V18
# assumes net$ALL (SOV) covers trucks F7...F13
net$TRUCKS_REFUSE <- (net$V16T_1 + net$V17T_1 + net$V18T_1+
  net$ALL*(df$F5 + df$F6 + df$F7 + df$F8 + df$F9 + df$F10 + df$F11 + df$F12 + df$F13))*per_T_REFUSE

net$TRUCKS_SU_SH <-  (net$V16T_1 + net$V17T_1 + net$V18T_1+
                        net$ALL*(df$F5 + df$F6 + df$F7 + df$F8 + df$F9 + df$F10 + df$F11 + df$F12 + df$F13))*per_T_SU_SH

net$TRUCKS_SU_LH <- (net$V16T_1 + net$V17T_1 + net$V18T_1+
                       net$ALL*(df$F5 + df$F6 + df$F7 + df$F8 + df$F9 + df$F10 + df$F11 + df$F12 + df$F13))*per_T_SU_LH

net$TRUCKS_MH <- (net$V16T_1 + net$V17T_1 + net$V18T_1+
                    net$ALL*(df$F5 + df$F6 + df$F7 + df$F8 + df$F9 + df$F10 + df$F11 + df$F12 + df$F13))*per_T_MH


net$TRUCKS_CU_SH <- (net$V16T_1 + net$V17T_1 + net$V18T_1+
                       net$ALL*(df$F5 + df$F6 + df$F7 + df$F8 + df$F9 + df$F10 + df$F11 + df$F12 + df$F13))*per_T_CU_SH
net$TRUCKS_CU_LH <- (net$V16T_1 + net$V17T_1 + net$V18T_1+
                       net$ALL*(df$F5 + df$F6 + df$F7 + df$F8 + df$F9 + df$F10 + df$F11 + df$F12 + df$F13))*per_T_CU_LH

file.remove("network/net.gpkg")
st_write(net, "network/net.gpkg")
# plot(net["PC"], axes = T, pal = cpt(colorRampPalette = T, rev = T), key.pos = 1)
# edit inventory.xlsx
rm(list = ls())
gc()
