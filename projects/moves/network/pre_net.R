library(data.table)
library(sf)
library(cptcity)
library(ggplot2)
net <- sf::st_read("../maryland_email.gpkg")
net <- st_transform(net, 6487)
names(net)
net$lanes <- as.numeric(as.character(net$SINGLE_UNIT_AADT))

metadata <- readxl::read_excel("../excel_inventory/inventory_MD.xlsx", "metadata")
setDT(metadata)
pop <- readxl::read_excel("../excel_inventory/inventory_MD.xlsx", "pop_moves")
setDT(pop)

# Motorcycle
# Passenger Car
# Passenger Truck
# Light Commercial Truck
# Intercity Bus
# Transit Bus
# School Bus
# Refuse Truck
# Single Unit Short-haul Truck
# Single Unit Long-haul Truck
# Motor Home
# Combination Short-haul Truck
# Combination Long-haul Truck
# LDV <- PC + PT
class(net$MOTORCYCLE_AADT)
net$MC <- net$MOTORCYCLE_AADT

LDV <- pop[sourceTypeID %in% c(21, 32), sum(sourceTypePopulation)]
per_PC <- pop[sourceTypeID %in% c(21), sum(sourceTypePopulation)]/LDV
per_PT <- pop[sourceTypeID %in% c(31), sum(sourceTypePopulation)]/LDV
class(net$CAR_AADT)
net$PC <- net$CAR_AADT*per_PC
net$PT <- net$CAR_AADT*per_PT

class(net$LIGHT_TRUCK_AADT)
net$LCT <- net$LIGHT_TRUCK_AADT

BUS <- pop[sourceTypeID %in% 41:43, sum(sourceTypePopulation)]
per_IBUS <- pop[sourceTypeID %in% c(41), sum(sourceTypePopulation)]/BUS
per_TBUS <- pop[sourceTypeID %in% c(42), sum(sourceTypePopulation)]/BUS
per_SBUS <- pop[sourceTypeID %in% c(43), sum(sourceTypePopulation)]/BUS
class(net$BUS_AADT)
net$BUS_INTERCITY <- net$BUS_AADT*per_IBUS
net$BUS_TRANSIT <- net$BUS_AADT*per_TBUS
net$BUS_SCHOOL <- net$BUS_AADT*per_SBUS


TRUCKS_SU <- pop[sourceTypeID %in% 51:54, sum(sourceTypePopulation)]
per_T_REFUSE <- pop[sourceTypeID %in% c(51), sum(sourceTypePopulation)]/TRUCKS_SU
per_T_SU_SW <- pop[sourceTypeID %in% c(52), sum(sourceTypePopulation)]/TRUCKS_SU
per_T_SU_LW <- pop[sourceTypeID %in% c(53), sum(sourceTypePopulation)]/TRUCKS_SU
per_T_MH <- pop[sourceTypeID %in% c(54), sum(sourceTypePopulation)]/TRUCKS_SU
class(net$SINGLE_UNIT_AADT)
net$TRUCKS_REFUSE <- net$SINGLE_UNIT_AADT*per_T_REFUSE
net$TRUCKS_SU_SW <- net$SINGLE_UNIT_AADT*per_T_SU_SW
net$TRUCKS_SU_LW <- net$SINGLE_UNIT_AADT*per_T_SU_LW
net$TRUCKS_MH <- net$SINGLE_UNIT_AADT*per_T_MH

class(net$COMBINATION_UNIT_AADT)
net$TRUCKS_CU_SH <- net$COMBINATION_UNIT_AADT/2
net$TRUCKS_CU_LH <- net$COMBINATION_UNIT_AADT/2

class(net$AADT_2018)
net$AADT_2018 <- net$AADT_2018

net$id <- 1:nrow(net)

ids <- net[is.na(net$PC), ]$id

pbapply::pblapply(seq_along(ids), function(i) {
  x <- st_buffer(net[net$id == ids[i], ], 1000)
  xt <- st_intersection(net, x)
  xt <- st_set_geometry(xt, NULL)
  TOT <- sum(xt[, c("MC", 
                    "PC", "PT", 
                    "LCT",
                    "BUS_INTERCITY", "BUS_TRANSIT", "BUS_SCHOOL",
                    "TRUCKS_REFUSE", "TRUCKS_SU_SW", "TRUCKS_SU_LW", "TRUCKS_MH",
                    "TRUCKS_CU_SH", "TRUCKS_CU_LH")], 
             na.rm = T)
  df <- data.frame(MC = sum(xt$MC, na.rm = T)/TOT,
                   PC = sum(xt$PC, na.rm = T)/TOT,
                   PT = sum(xt$PT, na.rm = T)/TOT,
                   LCT = sum(xt$LCT, na.rm = T)/TOT,
                   BUS_INTERCITY = sum(xt$BUS_INTERCITY, na.rm = T)/TOT,
                   BUS_TRANSIT = sum(xt$BUS_TRANSIT, na.rm = T)/TOT,
                   BUS_SCHOOL = sum(xt$BUS_SCHOOL, na.rm = T)/TOT,
                   TRUCKS_REFUSE = sum(xt$TRUCKS_REFUSE, na.rm = T)/TOT,
                   TRUCKS_SU_SW = sum(xt$TRUCKS_SU_SW, na.rm = T)/TOT,
                   TRUCKS_SU_LW = sum(xt$TRUCKS_SU_LW, na.rm = T)/TOT,
                   TRUCKS_MH = sum(xt$TRUCKS_MH, na.rm = T)/TOT,
                   TRUCKS_CU_SH = sum(xt$TRUCKS_CU_SH, na.rm = T)/TOT,
                   TRUCKS_CU_LH = sum(xt$TRUCKS_CU_LH, na.rm = T)/TOT
  )
  df$id <- ids[i]
  df
}) -> dx

dxb <- dx
dx <- rbindlist(dx)
# saveRDS(dx, "dx.rds")

names(dx)[1:(ncol(dx) - 1)] <- paste0("BUF_", names(dx)[1:(ncol(dx) - 1)])

net <- merge(net, dx, by = "id", all.x = T)

net
geo <- net$geometry

setDT(net)

net[,
    MC_FINAL := fifelse(is.na(MC), 
                        BUF_MC*AADT_2018, 
                        MC)]
net[,
    PC_FINAL := fifelse(is.na(PC), 
                        BUF_PC*AADT_2018, 
                        PC)]

net[,
    PT_FINAL := fifelse(is.na(PT), 
                        BUF_PT*AADT_2018, 
                        PT)]

net[,
    LCT_FINAL := fifelse(is.na(LCT), 
                         BUF_LCT*AADT_2018, 
                         LCT)]

net[,
    BUS_INTERCITY_FINAL := fifelse(is.na(BUS_INTERCITY), 
                                   BUF_BUS_INTERCITY*AADT_2018, 
                                   BUS_INTERCITY)]

net[,
    BUS_TRANSIT_FINAL := fifelse(is.na(BUS_TRANSIT), 
                                 BUF_BUS_TRANSIT*AADT_2018, 
                                 BUS_TRANSIT)]


net[,
    BUS_SCHOOL_FINAL := fifelse(is.na(BUS_SCHOOL), 
                                BUF_BUS_SCHOOL*AADT_2018, 
                                BUS_SCHOOL)]



net[,
    TRUCKS_REFUSE_FINAL := fifelse(is.na(TRUCKS_REFUSE), 
                             BUF_TRUCKS_REFUSE*AADT_2018, 
                                  TRUCKS_REFUSE)]




net[,
    TRUCKS_SU_SW_FINAL := fifelse(is.na(TRUCKS_SU_SW), 
                                  BUF_TRUCKS_SU_SW*AADT_2018, 
                                  TRUCKS_SU_SW)]


net[,
    TRUCKS_SU_LW_FINAL := fifelse(is.na(TRUCKS_SU_LW), 
                                  BUF_TRUCKS_SU_LW*AADT_2018, 
                                  TRUCKS_SU_LW)]


net[,
    TRUCKS_MH_FINAL := fifelse(is.na(TRUCKS_MH), 
                               BUF_TRUCKS_MH*AADT_2018, 
                               TRUCKS_MH)]


net[,
    TRUCKS_CU_SH_FINAL := fifelse(is.na(TRUCKS_CU_SH), 
                                  BUF_TRUCKS_CU_SH*AADT_2018, 
                                  TRUCKS_CU_SH)]

net[,
    TRUCKS_CU_LH_FINAL := fifelse(is.na(TRUCKS_CU_LH), 
                                  BUF_TRUCKS_CU_LH*AADT_2018, 
                                  TRUCKS_CU_LH)]


# Motorcycle
# Passenger Car
# Passenger Truck
# Light Commercial Truck
# Intercity Bus
# Transit Bus
# School Bus
# Refuse Truck
# Single Unit Short-haul Truck
# Single Unit Long-haul Truck
# Motor Home
# Combination Short-haul Truck
# Combination Long-haul Truck
# LDV <- PC + PT

net_final <- net[, c(paste0(c("MC", 
                              "PC", "PT", 
                              "LCT",
                              "BUS_INTERCITY", "BUS_TRANSIT", "BUS_SCHOOL",
                              "TRUCKS_REFUSE","TRUCKS_SU_SW", "TRUCKS_SU_LW", "TRUCKS_MH",
                              "TRUCKS_CU_SH", "TRUCKS_CU_LH"),
                            "_FINAL"), "NUM_LANES", "AAWDT_2018")]
  names(net_final) <- gsub("_FINAL", "", names(net_final))
net_final <- st_sf(net_final,
                   geometry = geo)

file.remove("network/net.gpkg")
st_write(net_final, "network/net.gpkg")
# plot(net_final["PC"], axes = T)
# edit inventory.xlsx
rm(list = ls())
gc()
´