switch(language,
       "portuguese" = cat("Filtrando carro electrico: \n"),
       "english" = cat("Filtering electric car\n"),
       "spanish" = cat("Filtrando autos electricos\n")
)

year
month <- fuel$Month[1]
date <- as.Date(ISOdate(year, month, 1,0,0))

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}

factor_emi <- numberOfDays(date = date) # daily to month

data(decoder)

setDT(metadata)
metadata <- metadata[fuel != "ELEC"]

# vehicles ####
lapply(seq_along(metadata$vehicles), function(kk) {
  readRDS(paste0("veh/", metadata$vehicles[kk], ".rds"))
}) -> lv

names(lv) <- metadata$vehicles

#SQL EF PER DISTANCE ####
con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = host,
                      db = moves_db,
                      user = user,
                      password = password,
                      port = port)


RMariaDB::dbGetQuery(con,paste0("
SELECT pollutantID, sourceTypeID, fuelTypeID, 
modelYearID, hourID, avgSpeedBinID, ratePerDistance, AVG(ratePerDistance)
FROM `rateperdistance`
WHERE pollutantID = 91 AND
processID = 1 AND
rateperdistance > 0
GROUP BY MOVESRunID, pollutantID, modelYearID,
fuelTypeID, avgSpeedBinID, hourID, sourceTypeID
")) -> ef

names(ef)[ncol(ef)] <- "EF" 
setDT(ef)

RMariaDB::dbDisconnect(con)

# moves rates per distance year 
moves_rpdy(
  veh = lv, 
  lkm = lkm, 
  ef = ef, 
  source_type_id = metadata$sourceTypeID, 
  fuel_type_id = metadata$fuelTypeID, 
  pollutant_id = 91, 
  fuel_type = fuel_type, 
  speed_bin = speed_bin, 
  profile = tfs, 
  vehicle = metadata$vein_name, 
  vehicle_type = metadata$name, 
  fuel_subtype = metadata$fuel,
  process_id = 1,
  verbose = verbose
) -> emi

#SQL EF PER VEHICLE ####

cat("Detecting pollutants and process in SQL ...\n")

con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = host,
                      db = moves_db,
                      user = user,
                      password = password,
                      port = port)

RMariaDB::dbGetQuery(con,paste0("
SELECT pollutantID, sourceTypeID, fuelTypeID, processID,
modelYearID, hourID, ratePerVehicle, AVG(ratePerVehicle)
FROM `rateperVehicle`
WHERE pollutantID = 91 AND
ratepervehicle > 0
GROUP BY MOVESRunID, pollutantID, modelYearID, processID,
fuelTypeID, hourID, sourceTypeID
")) -> ef

names(ef)[ncol(ef)] <- "EF" 
setDT(ef)

RMariaDB::dbDisconnect(con)

moves_rpsy_meta(
  metadata = metadata, 
  lkm = rep(1, length(lkm)), 
  ef = ef, 
  fuel_type = fuel_type, 
  profile = tfs,
  colkt = F,
  simplify = T, 
  verbose = verbose) -> emiv


  # merge ####
emiv$veh$value <- emiv$veh$value *units::set_units(1,"miles")
names(emiv$veh)
names(emi$veh)


emiv$veh$fuelTypeID <- emiv$veh$fuel

# emiallback up
bu_emi <- emi$veh
bu_emi$db <- "emissions_per_distance"
bu_emiv <- emiv$veh
bu_emiv$db <- "emissions_per_vehicle"

emi <- rbind(bu_emi,
             bu_emiv[, names(bu_emi), with = F])

emi[, sum(value), by = hour][, plot(V1, type = "b")]
fc <- emi[, sum(value)*factor_emi, by = fuelTypeID]

fuel <- fuel[, c("fuel", "gallons", "Year", "Month")]


names(fuel)[1:2] <- c("fuelTypeID","consumption_gallons")

fuel$consumption_gallons <- units::set_units(fuel$consumption_gallons, "gallons")

fuel <- merge(fc, fuel, by = "fuelTypeID", all.x = T)
names(fuel)[2] <- "estimated_gallons"

fuel$estimation_consumption <- as.numeric(fuel$estimated_gallons)/as.numeric(fuel$consumption_gallons)
# dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t
fuel <-fuel[fuelTypeID != "E85"]

print(fuel)

############################################################################
# obtaining k ####
############################################################################
k_D                <- as.numeric(1/fuel[fuelTypeID == "D", ]$estimation_consumption)
k_CNG              <- as.numeric(1/fuel[fuelTypeID == "CNG", ]$estimation_consumption)
k_G                <- as.numeric(1/fuel[fuelTypeID == "G", ]$estimation_consumption)
k_E85              <- 1

saveRDS(list(k_D = k_D,
             k_CNG = k_CNG,
             k_G = k_G),
        "veh/list_k_fuel.rds")

language <- "english" # portuguese spanish
metadata <- readRDS("config/metadata.rds")
net <- readRDS("network/net.rds")
tfs <- readRDS("config/tfs.rds")
veh <- readRDS("config/fleet_age.rds")
categories <- unique(metadata$vein_name) # in net
year <- 2019
theme <- "dark" # dark clean ink

verbose  <- FALSE
speed <- FALSE
fuel <- readRDS("config/fuel.rds")
source("scripts/traffic.R", encoding = "UTF-8")

############################################################################
# Re estimating FC ####
############################################################################
factor_emi <- numberOfDays(date = date) # daily to month

setDT(metadata)
metadata <- metadata[fuel != "ELEC"]

lapply(seq_along(metadata$vehicles), function(kk) {
  readRDS(paste0("veh/", metadata$vehicles[kk], ".rds"))
}) -> lv

names(lv) <- metadata$vehicles


#SQL EF PER DISTANCE ####
con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = host,
                      db = moves_db,
                      user = user,
                      password = password,
                      port = port)


RMariaDB::dbGetQuery(con,paste0("
SELECT pollutantID, sourceTypeID, fuelTypeID, 
modelYearID, hourID, avgSpeedBinID, ratePerDistance, AVG(ratePerDistance)
FROM `rateperdistance`
WHERE pollutantID = 91 AND
processID = 1 AND
rateperdistance > 0
GROUP BY MOVESRunID, pollutantID, modelYearID,
fuelTypeID, avgSpeedBinID, hourID, sourceTypeID
")) -> ef

names(ef)[ncol(ef)] <- "EF" 
setDT(ef)

RMariaDB::dbDisconnect(con)

# moves rates per distance year 
moves_rpdy(
  veh = lv, 
  lkm = lkm, 
  ef = ef, 
  source_type_id = metadata$sourceTypeID, 
  fuel_type_id = metadata$fuelTypeID, 
  pollutant_id = 91, 
  fuel_type = fuel_type, 
  speed_bin = speed_bin, 
  profile = tfs, 
  vehicle = metadata$vein_name, 
  vehicle_type = metadata$name, 
  fuel_subtype = metadata$fuel,
  process_id = 1,
  verbose = verbose
) -> emi


#SQL EF PER VEHICLE ####

cat("Detecting pollutants and process in SQL ...\n")

con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = host,
                      db = moves_db,
                      user = user,
                      password = password,
                      port = port)

RMariaDB::dbGetQuery(con,paste0("
SELECT pollutantID, sourceTypeID, fuelTypeID, processID,
modelYearID, hourID, ratePerVehicle, AVG(ratePerVehicle)
FROM `rateperVehicle`
WHERE pollutantID = 91 AND
ratepervehicle > 0
GROUP BY MOVESRunID, pollutantID, modelYearID, processID,
fuelTypeID, hourID, sourceTypeID
")) -> ef

names(ef)[ncol(ef)] <- "EF" 
setDT(ef)

RMariaDB::dbDisconnect(con)

moves_rpsy_meta(
  metadata = metadata, 
  lkm = rep(1, length(lkm)), 
  ef = ef, 
  fuel_type = fuel_type, 
  profile = tfs,
  colkt = F,
  simplify = T, 
  verbose = verbose) -> emiv


# merge ####
emiv$veh$value <- emiv$veh$value *units::set_units(1,"miles")
names(emiv$veh)
names(emi$veh)


emiv$veh$fuelTypeID <- emiv$veh$fuel

# emiallback up
bu_emi <- emi$veh
bu_emi$db <- "emissions_per_distance"
bu_emiv <- emiv$veh
bu_emiv$db <- "emissions_per_vehicle"

emi <- rbind(bu_emi,
             bu_emiv[, names(bu_emi), with = F])

fc <- emi$veh[, sum(value)*factor_emi, by = fuelTypeID]

fuel <- fuel[, c("fuel", "gallons", "Year", "Month")]


names(fuel)[1:2] <- c("fuelTypeID","consumption_gallons")

fuel$consumption_gallons <- units::set_units(fuel$consumption_gallons, "gallons")

fuel <- merge(fc, fuel, by = "fuelTypeID", all.x = T)
names(fuel)[2] <- "estimated_gallons"

fuel$estimation_consumption <- as.numeric(fuel$estimated_gallons)/as.numeric(fuel$consumption_gallons)
# dtf$estimation_consumption <- dtf$estimation_t / dtf$consumption_t
fuel <-fuel[fuelTypeID != "E85"]

print(fuel)


switch(language,
       "portuguese" = message("\nArquivos em: /emi/*:"),
       "english" = message("\nFiles in: /emi/*"),
       "spanish" = message("\nArchivos en: /emi/*")
)

switch(language,
       "portuguese" = message("Limpando..."),
       "english" = message("Cleaning..."),
       "spanish" = message("Limpiando...")
)

suppressWarnings(rm(i, j, pol, dt, dt0, dtf, factor_emi, fuel, emi))

suppressWarnings(
  rm(
    kPC, kPC_G, kPC_E, kPC_FG, kPC_FE,
    kLCV, kLCV_G, kLCV_E, kLCV_FG, kLCV_FE, kLCV_D,
    kTRUCKS, kTRUCKS_SL_D, kTRUCKS_L_D, kTRUCKS_M_D, kTRUCKS_SH_D, kTRUCKS_H_D,
    kBUS, kBUS_URBAN_D, kBUS_MICRO_D, kBUS_COACH_D,
    kMC, kMC_150_G, kMC_150_500_G, kMC_500_G,
    kMC_150_FG, kMC_150_500_FG, kMC_500_FG,
    kMC_150_FE, kMC_150_500_FE, kMC_500_FE,
    l_PC, l_LCV, l_TRUCKS, l_BUS, l_MC,
    i, arquivos, cores, df, f, ff,
    n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC,
    na, nveh, p, tit, tit2, categories, verbose, x, kf,
    k_G, k_D, k_E,
    metadata,
    net, veh, year,
    theme,
    speed,
    sp_mov,
    df,
    dfs
  )
)

invisible(gc())
