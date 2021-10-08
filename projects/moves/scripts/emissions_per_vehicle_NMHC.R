
file.remove("emi/emissions_rate_per_vehicle/STREETS_pollutantID_NMHC_BY_TYPE.csv.gz")

# switch(language,
#        "portuguese" = cat("Filtrando carro electrico: \n"),
#        "english" = cat("Filtering electric car\n"),
#        "spanish" = cat("Filtrando autos electricos\n")
# )
setDT(metadata)
metadata_original <- metadata

# me parece que la mejor opcion es filtrar por 
# contaminante y por proceso.
# 1) por ejemplo, leer EF para pollutantID = 2
# 2) luego filtrar por processID y relacionar con metadata
# 3) Luego, leer los veiculos con metadata
# 4) y estimar las eisiones para el processID

# 1) Identify pollutants in data.base

metadata <- metadata_original[fuel != "ELEC"]

# we keep pollutant 70 in case the user wants to applies a different speciation
# 79 NMHC, for instance, MOVES

data(decoder)
# 2) Obtener EF


# select unique fuels ####
cat("Detecting unique fuels for NMHC ...\n")

con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = host,
                      db = moves_db,
                      user = user,
                      password = password,
                      port = port)

RMariaDB::dbGetQuery(con,paste0("
SELECT DISTINCT fuelTypeID
FROM `rateperdistance`
WHERE pollutantID = 79 AND
rateperdistance > 0
")) -> fuelTypeID

names(fuelTypeID)[ncol(fuelTypeID)] <- "EF" 
setDT(fuelTypeID)

RMariaDB::dbDisconnect(con)

for(i in seq_along(fuelTypeID$EF)) {
    
cat("Detecting pollutants and process in SQL ...\n")

# pollutantID = i
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
WHERE pollutantID = 79 AND
fuelTypeID = ", fuelTypeID$EF[i], " AND
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
    simplify = F, 
    verbose = verbose) -> emi


#NMHC
metadata <- readRDS("config/metadata.rds")
setDT(metadata)
metadata$vtype <- c(rep("LDV", 13), rep("HDV", 39-14))
id_ldv <- unique(metadata[vtype == "LDV"]$sourceTypeID)
id_hdv <- unique(metadata[vtype == "HDV"]$sourceTypeID)

emi[, family := fifelse(
        sourceTypeID %in% id_ldv, "LDV", "HDV"
)]

emi[, family := fifelse(
        fuel %in% "CNG", "ALL", family
)]

data(decoder)

names(decoder$emission_process) <- c("processID", "process")

emi <- merge(emi, 
             decoder$emission_process, 
             by = "processID",
             all.x = TRUE)

# we need 
setDT(decoder$emission_process)
pros <- decoder$emission_process[processID %in% unique(emi$processID)]
# ssuming Refuelling as evap
exhaust <- unique(grep(pattern = "Exhaust", 
                       x = emi$process, 
                       value = T))

emi[, process2 := fifelse(process %in% exhaust, "exhaust", "evap")]


emi[, 
    sum(age_total, na.rm = T),
    by = .(id, hour, family, fuel, process2)] -> emi2

streets <- data.table::dcast.data.table(emi2, 
                                        formula = id +family + fuel + process2 ~ hour, 
                                        value.var = "V1")

names(streets)[5:ncol(streets)] <- paste0("H", names(streets)[5:ncol(streets)])

# Need streets results by 

data.table::fwrite(streets, 
                   "emi/emissions_rate_per_vehicle/STREETS_pollutantID_NMHC_BY_TYPE.csv.gz", 
                   append = T)

}

switch(language,
       "portuguese" = message("\n\nArquivos em: /emi/*:"),
       "english" = message("\nFiles in: /emi/*"),
       "spanish" = message("\nArchivos en: /emi/*")
)


switch(language,
       "portuguese" = message("Limpando..."),
       "english" = message("Cleaning..."),
       "spanish" = message("Limpiando...")
)

suppressWarnings(
        rm(
                i, j, pol,
                n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC,
                ns, ln, p, df, dl, cores
        )
)

invisible(gc())
